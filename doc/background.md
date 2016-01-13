# Background

Note: A large part of this text is adapted (with permission) from ["On
caching"](on-caching) from Roger's blog.

 [on-caching]: http://blog.differentpla.net/blog/2013/12/29/on-caching/

In [Electric Imp's](electric-imp) backend cloud service, we need to handle
accepting a large number of concurrent device connections. As each device
connects, we look up various information about that device. Because this
information is relatively static, it's a prime candidate for caching.

Also note that I've simplified the inner workings of the Electric Imp backend
for the purposes of this document.

## Caching

**TODO: flowchart goes here**

## A really naive implementation

A *really* naive caching implementation might use a `gen_server`
and look like this:

    get_value(Key) ->
        gen_server:call(value_cache, {get, Key}).

    handle_call({get, Key}, _From, State) ->
        case lists:keyfind(ModelId, 1, State) of
            false ->
                % cache miss; do the expensive thing:
                Value = expensive_calculation(Key),
                State2 = [{Key, Value} | State],
                {reply, Value, State2};
            {_, Value} ->
                % cache hit; return the value:
                {reply, Value, State}
        end.

In this example, we're using a singleton `gen_server` (because the cache needs
to be shared), and storing the information in a list.

*Yes, we could make this faster by using the `dict` or `gb_trees` module, or
by using a map; you're missing the point.*

The problem with this implementation is that it's a **singleton** `gen_server`,
which means that it can only handle one call at a time. This leads to
"convoying", where the callers all queue up behind one another, and progress is
only made slowly.

It's not so bad if every caller is asking for the same key: the first caller
incurs the delay, and subsequent queries are relatively quick. However, if
you're querying for a mixed set of keys, each caller has to wait, even if the
given key is already in the cache.

## Avoiding the singleton

If the problem is our callers queuing up because we used a singleton, let's
avoid the `gen_server` and use an ETS table, which is another way to provide
shared storage in an Erlang application.

Let's suppose that we're looking for the compiled squirrel bytecode for a given
device model. That might look like the following: 


    get_bytecode(ModelId, Redis) ->
        case ets:lookup(bytecode_cache, ModelId) of
            [] ->
                % cache miss; look in (e.g.) redis:
                {ok, ByteCode} = eredis:q(Redis, ["GET", ModelId]),
                ets:insert(bytecode_cache, {ModelId, ByteCode}),
                {ok, ByteCode};
            [{_, ByteCode}] ->
                % cache hit
                {ok, ByteCode}
        end.

If that function is called by the process handling the device connection, that
would seem to be enough, right? It's safe to use a public ETS table from
multiple processes, so we're done here.

Not so fast. This implementation falls down under a stampede.

If a large number of devices all connect at once (a "stampede"), and they're
all asking about the same information, and the cache is cold (for example,
after restarting a server), then there will be a race here. A large number of
the connections will see a cache miss and will query the backend at the same
time.

**TODO: naive sequence diagram here?**

See, for example, [Cache stampede](wiki-cache-stampede), on Wikipedia.

 [wiki-cache-stampede]: https://en.wikipedia.org/wiki/Cache_stampede

## Use a pool

We *could* try using a connection pool with a fixed size to reduce the number
of concurrent queries to the DB.

But even with that, all of the connections will be busy looking up the *same*
information.

That's a massive waste: we're duplicating a lot of effort looking up the same
key multiple times, and we've starved the pool, meaning that we can't look up
*other* keys.

## Conclusion

- Requirements?
  - Don't collapse under stampede, but get work done, implying...
  - Block like queries, don't block unlike queries.
- Pre-warm the cache?
- Lookup _on_ expiry/flush?

- Possible implementations:
  - one process per key
    - either registered process name, or look up in ETS. This effectively has
      the "promise" kicking around forever. I found that wasteful (personal
      opinion), but it might be a valid solution for some cases.
  - sharded gen_server: effectively you can either shard over the worker space,
    or you can shard over the cache space.
  - locking, using a promise: this is Wikipedia's "wait until the value is
    recomputed"

  - "Return a "not-found" and have the client handle the absence of the value
    properly" not valid in our case: if we drop the device connection, it'll
    reconnect immediately, which wastes more resources.
  - keep a stale item: this only works for warm caches; not applicable to our
    situation.

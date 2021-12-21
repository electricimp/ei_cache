# ei_cache

Stampede-resistant cache library for Erlang

## Overview

A lot of caching libraries do not place a value into cache until its value has
been resolved. If multiple requests for the same key arrive at the same time,
they all see the missing value and they all work on resolving the value, rather
than only the first one doing this.

The `ei_cache` library avoids this problem by having the first request for a
missing key register a "promise" in its place. Concurrent requests for the same
key will subscribe to this promise rather than attempt to resolve the value.

## Using this library

Add the following to your project's `rebar.config`:

    {deps, [
        {ei_cache, ".*",
         {git, "git://github.com/electricimp/ei_cache.git", {tag, "1.0"}}}
    ]}.

## Is it ready for use?

Yes. We're using it in production at Electric Imp.

## Using it

    Cache = foo.
    {ok, _} = ei_cache:start_link(Cache, fun(Key) -> lookup_key_somehow(Key) end).
    % ... or add it to your supervision tree.

    % Then, later...
    Value = ei_cache:get_value(Cache, Key).

## Metrics

By default, `ei_cache` reports metrics to an ETS table. You can get the current
values with `ei_cache_metrics:get_counts(Cache)`. Alternatively, you can report
metrics to folsom by putting the following in your config file:

    {ei_cache, [{metrics_module, ei_cache_metrics_folsom}]}

It reports the following metrics:

- `server_misses`: Cache misses.
- `client_hits`: First chance cache hits.
- `server_hits`: Second chance cache hits.
- `client_promises`: The client found a promise when querying the cache.
- `server_promises`: The server found a promise when querying the cache. This
  is a "second chance" promise, more-or-less.

A word on `client` vs. `server`: the whole point of this library is to avoid
the bottleneck imposed by using a single `gen_server`, so the caller (client)
does all of the ETS queries.

- If the value is present, then that's a "client hit".
- If the value isn't present, but there's already a query in flight, that's a
  "client promise".
- If the value isn't present, and this is the first miss, then we call the
  `gen_server`. That -- usually -- results in a "server miss".
- Sometimes, the client is racing for the same key and the server has either
  already found the value, or has already started a worker. Those are "server
  hit" and "server promise" respectively.

### Hit Ratio

For a simple set of hit/miss metrics:

    Hits = ClientHits + ServerHits.
    Misses = ClientPromises + ServerMisses + ServerPromises.
    Requests = Hits + Misses.
    Ratio = Hits / Requests.

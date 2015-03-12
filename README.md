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
         {git, "git://github.com/electricimp/ei_cache.git", {tag, "0.9"}}}
    ]}.

## Is it ready for use?

We built this to be used in Electric Imp's cloud backend. It's based on an
algorithm that we're already using in production. While *this* particular
library is not in production right now, it will be soon.

## Using it

    {ok, _} = ei_cache:start_link(foo, fun(Key) -> lookup_key_somehow(Key) end).
    % ... or add it to your supervision tree.

    % Then, later...
    Value = ei_cache:get_value(Key).

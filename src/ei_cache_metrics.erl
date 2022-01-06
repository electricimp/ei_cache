-module(ei_cache_metrics).
-export([init/1, client_promise/1, client_hit/1, server_miss/1, server_promise/1, server_hit/1]).

-define(APP, ei_cache).
-define(DEFAULT_METRICS_MODULE, ei_cache_metrics_ets).
-define(METRICS, (application:get_env(?APP, metrics_module, ?DEFAULT_METRICS_MODULE))).

init(Name) ->
    ?METRICS:init(Name),
    ?METRICS:new_spiral(Name, client_promises),
    ?METRICS:new_spiral(Name, client_hits),
    ?METRICS:new_spiral(Name, server_misses),
    ?METRICS:new_spiral(Name, server_promises),
    ?METRICS:new_spiral(Name, server_hits),
    ?METRICS:new_spiral(Name, hits),
    ?METRICS:new_spiral(Name, misses),
    ?METRICS:new_spiral(Name, requests),
    ok.

client_promise(Name) ->
    ?METRICS:increment(Name, client_promises),
    % client_promise is a miss.
    ?METRICS:increment(Name, misses),
    ?METRICS:increment(Name, requests).

client_hit(Name) ->
    ?METRICS:increment(Name, client_hits),
    ?METRICS:increment(Name, hits),
    ?METRICS:increment(Name, requests).

server_miss(Name) ->
    ?METRICS:increment(Name, server_misses),
    ?METRICS:increment(Name, misses),
    ?METRICS:increment(Name, requests).

server_promise(Name) ->
    ?METRICS:increment(Name, server_promises),
    % server_promise is a miss.
    ?METRICS:increment(Name, misses),
    ?METRICS:increment(Name, requests).

server_hit(Name) ->
    ?METRICS:increment(Name, server_hits),
    ?METRICS:increment(Name, hits),
    ?METRICS:increment(Name, requests).

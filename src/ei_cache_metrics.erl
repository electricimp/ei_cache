-module(ei_cache_metrics).
-export([init/1, client_promise/1, client_hit/1, server_miss/1, server_promise/1, server_hit/1]).

-define(METRICS, (application:get_env(ei_cache, metrics_module, ei_cache_metrics_ets))).

init(Name) ->
    ?METRICS:init(Name),
    ?METRICS:new_spiral(Name, client_promises),
    ?METRICS:new_spiral(Name, client_hits),
    ?METRICS:new_spiral(Name, server_misses),
    ?METRICS:new_spiral(Name, server_promises),
    ?METRICS:new_spiral(Name, server_hits),
    ok.

client_promise(Name) -> ?METRICS:increment(Name, client_promises).
client_hit(Name) -> ?METRICS:increment(Name, client_hits).
server_miss(Name) -> ?METRICS:increment(Name, server_misses).
server_promise(Name) -> ?METRICS:increment(Name, server_promises).
server_hit(Name) -> ?METRICS:increment(Name, server_hits).

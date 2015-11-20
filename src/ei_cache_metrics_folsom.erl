-module(ei_cache_metrics_folsom).
-export([init/1, new_spiral/2, increment/2]).

init(_Name) ->
    ok.

new_spiral(Name, Key) ->
    folsom_metrics:new_spiral({ei_cache, Name, Key}).

increment(Name, Counter) ->
    folsom_metrics:notify({ei_cache, Name, Counter}, 1).

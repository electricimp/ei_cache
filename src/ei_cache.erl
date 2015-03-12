-module(ei_cache).
-export([start_link/2, get_value/2, delete/1, delete/2, get_counts/1]).

start_link(Name, Fun) ->
    ei_cache_server:start_link(Name, Fun).

get_value(Name, Key) ->
    T = ei_cache_names:table(Name),
    get_value_result(Name, T, Key, ets:lookup(T, Key)).

get_value_result(Name, T, Key, []) ->
    S = ei_cache_names:server(Name),
    handle_reply(T, Key, gen_server:call(S, {get_value, Key}));

get_value_result(Name, T, Key, [{Key, {promise, P}}]) ->
    ei_cache_metrics:client_promise(Name),
    ei_cache_promise:get_value(T, Key, P);

get_value_result(Name, _T, Key, [{Key, {value, Value}}]) ->
    ei_cache_metrics:client_hit(Name),
    Value.

handle_reply(T, Key, {promise, P}) ->
    ei_cache_promise:get_value(T, Key, P);

handle_reply(_T, _Key, {value, Value}) ->
    Value.

delete(Name) ->
    % If there's a promise in flight at the time we do the delete, we leave it
    % alone.
    T = ei_cache_names:table(Name),
    ets:match_delete(T, {'_', {value, '_'}}).

delete(Name, Key) ->
    % If there's a promise in flight at the time we do the delete, we leave it
    % alone.
    T = ei_cache_names:table(Name),
    ets:match_delete(T, {Key, {value, '_'}}).

get_counts(Name) ->
    ei_cache_metrics:get_counts(Name).

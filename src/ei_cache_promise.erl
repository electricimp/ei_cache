-module(ei_cache_promise).
-export([start_link/3]).
-export([get_value/3]).

% Note: while we don't need a gen_server; do we want to use proc_lib?
start_link(Fun, Key, T) ->
    Pid = erlang:spawn_link(
            fun() ->
                    % Work out the value.
                    Value = Fun(Key),

                    % Put the new value in ETS.
                    ets:insert(T, {Key, {value, Value}}),

                    % Reply to the subscribers.
                    self() ! '$ei_cache_sentinel',
                    reply({value, Value})
            end),
    {ok, Pid}.

reply(Reply) ->
    receive
        {'$ei_cache_subscribe', {Pid, Ref}} ->
            Pid ! {'$ei_cache_reply', Ref, Reply},
            reply(Reply);
        '$ei_cache_sentinel' ->
            ok
    end.

get_value(T, Key, P) ->
    TimeoutMs = 5000,
    get_value(T, Key, P, TimeoutMs).

get_value(T, Key, P, TimeoutMs) ->
    Mref = erlang:monitor(process, P),
    P ! {'$ei_cache_subscribe', {self(), Mref}},
    receive
        {'$ei_cache_reply', Mref, {value, Value}} ->
            erlang:demonitor(Mref, [flush]),
            Value;
        {'DOWN', Mref, _, _, noproc} ->
            lookup_value(T, Key);
        {'DOWN', Mref, _, _, normal} ->
            lookup_value(T, Key);
        {'DOWN', Mref, _, _, Error} ->
            exit(Error)
    after TimeoutMs ->
              erlang:demonitor(Mref, [flush]),
              exit(timeout)
    end.

lookup_value(T, Key) ->
    case ets:lookup(T, Key) of
        [{Key, {value, Value}}] ->
            Value;
        _ ->
            exit(broken_promise)
    end.

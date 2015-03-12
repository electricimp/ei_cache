-module(ei_cache_promise).
-export([start/3]).
-export([get_value/3]).

% Note: while we don't need a gen_server; do we want to use proc_lib?
start(Fun, Key, T) ->
    Pid = erlang:spawn(
            fun() ->
                    % Work out the value.
                    Value = Fun(Key),

                    % Put the new value in ETS.
                    ets:insert(T, {Key, {value, Value}}),

                    % Reply to the subscribers.
                    self() ! sentinel,
                    reply({value, Value})
            end),
    {ok, Pid}.

reply(Reply) ->
    receive
        {subscribe, {Pid, Ref}} ->
            Pid ! {reply, Ref, Reply},
            reply(Reply);
        sentinel ->
            ok
    end.

get_value(T, Key, P) ->
    TimeoutMs = 5000,
    get_value(T, Key, P, TimeoutMs).

get_value(T, Key, P, TimeoutMs) ->
    Mref = erlang:monitor(process, P),
    P ! {subscribe, {self(), Mref}},
    receive
        {reply, Mref, {value, Value}} ->
            erlang:demonitor(Mref, [flush]),
            Value;
        {'DOWN', Mref, _, _, noproc} ->
            [{Key, {value, Value}}] = ets:lookup(T, Key),
            Value;
        {'DOWN', Mref, _, _, normal} ->
            [{Key, {value, Value}}] = ets:lookup(T, Key),
            Value;
        {'DOWN', Mref, _, _, Error} ->
            exit(Error)
    after TimeoutMs ->
              erlang:demonitor(Mref, [flush]),
              exit(timeout)
    end.

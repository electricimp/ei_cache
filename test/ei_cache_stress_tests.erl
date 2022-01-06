-module(ei_cache_stress_tests).
-include_lib("eunit/include/eunit.hrl").
-define(test(F), {??F, fun F/0}).

all_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      ?test(stress),
      ?test(stress2),
      ?test(stress3)
     ]}.

setup() ->
    % ensure the app config is loaded
    application:start(ei_cache),
    % Some of these tests involve process death, which some viewers might find
    % disturbing.
    error_logger:tty(false),
    ok.

cleanup(_) ->
    % We occasionally get {error, {already_started, _}} failures because
    % (somehow) the ei_cache_server process doesn't unregister itself quickly
    % enough. So: wait until there are no more ei_cache processes registered.
    wait_until_ei_cache_unregistered(),
    ok.

wait_until_ei_cache_unregistered() ->
    wait_until_ei_cache_unregistered(true).

wait_until_ei_cache_unregistered(false) ->
    ok;
wait_until_ei_cache_unregistered(true) ->
    erlang:yield(),
    Pred = fun(Name) ->
                   lists:prefix("ei_cache", atom_to_list(Name))
           end,
    wait_until_ei_cache_unregistered(lists:any(Pred, registered())).

stress() ->
    Iterations = 50000,
    Range = 1000,
    MaxDelay = 0,
    MaxLag = 125,
    run_stress(Iterations, Range, MaxDelay, MaxLag).

stress2() ->
    Iterations = 50000,
    Range = 1000,
    MaxDelay = 125,
    MaxLag = 125,
    run_stress(Iterations, Range, MaxDelay, MaxLag).

stress3() ->
    Iterations = 50000,
    Range = 1000,
    MaxDelay = 0,
    MaxLag = 0,
    run_stress(Iterations, Range, MaxDelay, MaxLag).

run_stress(Iterations, Range, MaxDelay, MaxLag) ->
    {ok, _} = ei_cache:start_link(
                hash_cache,
                fun(Key) ->
                        timer:sleep(rand:uniform(MaxLag + 1)),
                        erlang:phash2(Key)
                end),
    wait_for([spawn_opt(
                fun() ->
                        timer:sleep(rand:uniform(MaxDelay + 1)),
                        Key = rand:uniform(Range),
                        ?assertEqual(erlang:phash2(Key), ei_cache:get_value(hash_cache, Key))
                end, [link, monitor]) || _N <- lists:seq(1, Iterations)]),
    report_metrics(hash_cache).

% @todo Can we have a stress test that runs until it sees at least one of each metric reported?

wait_for([]) ->
    ok;
wait_for([{Pid, Ref} | Waits]) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            wait_for(Waits)
    end.

report_metrics(Name) ->
    Counts = ei_cache_metrics_ets:get_counts(Name),
    ?debugFmt("~p: ~p\n", [Name, Counts]).

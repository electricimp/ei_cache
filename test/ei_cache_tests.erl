-module(ei_cache_tests).
-include_lib("eunit/include/eunit.hrl").
-define(test(F), {??F, fun F/0}).

all_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      ?test(miss_calls_fun),
      ?test(two_caches),
      ?test(same_cache_different_processes),

      ?test(miss_then_hit),
      ?test(different_keys_in_sequence),
      ?test(different_keys_in_parallel),

      ?test(repeated_hits),

      ?test(stress),
      ?test(stress2),

      ?test(worker_dies)
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

miss_calls_fun() ->
    % Cache instances are named.
    Name = foo,
    % Cache instances have a function associated with them.
    % This cache instance, rather than look something up in redis, for example,
    % simply doubles the specified value.
    Fun = fun(Key) -> Key * 2 end,
    {ok, _} = ei_cache:start_link(Name, Fun),

    Key = 12,
    ?assertEqual(24, ei_cache:get_value(foo, Key)).

two_caches() ->
    {ok, _} = ei_cache:start_link(double_cache, fun(Key) -> Key * 2 end),
    {ok, _} = ei_cache:start_link(triple_cache, fun(Key) -> Key * 3 end),
    ?assertEqual(30, ei_cache:get_value(double_cache, 15)),
    ?assertEqual(15, ei_cache:get_value(triple_cache, 5)).

same_cache_different_processes() ->
    {ok, _} = ei_cache:start_link(double_cache, fun(Key) -> Key * 2 end),

    wait_for([
              spawn_opt(fun() -> ?assertEqual(20, ei_cache:get_value(double_cache, 10)) end, [link, monitor]),
              spawn_opt(fun() -> ?assertEqual(10, ei_cache:get_value(double_cache, 5)) end, [link, monitor])]),
    report_metrics(double_cache),
    ok.

miss_then_hit() ->
    % @todo How to assert the number of times that the function gets called?
    % We get it to send a message to us, and then assert that we didn't receive
    % any extras.
    Self = self(),
    {ok, _} = ei_cache:start_link(
                reverse_cache,
                fun(Key) ->
                        Self ! {get_value_called, Key},
                        timer:sleep(250),
                        lists:reverse(Key)
                end),
    ?assertEqual("cba", ei_cache:get_value(reverse_cache, "abc")),
    ?assertEqual("cba", ei_cache:get_value(reverse_cache, "abc")),
    % @todo Count the get_value_called messages, and then assert the number.
    receive {get_value_called, "abc"} -> ok end,
    receive M -> erlang:error({unexpected, M}) after 0 -> ok end,
    report_metrics(reverse_cache),
    ok.

different_keys_in_sequence() ->
    {ok, _} = ei_cache:start_link(
                reverse_cache,
                fun(Key) ->
                        timer:sleep(250),
                        lists:reverse(Key)
                end),
    ?assertEqual("cba", ei_cache:get_value(reverse_cache, "abc")),
    ?assertEqual("nml", ei_cache:get_value(reverse_cache, "lmn")),
    ?assertEqual("rqp", ei_cache:get_value(reverse_cache, "pqr")),
    ?assertEqual("zyx", ei_cache:get_value(reverse_cache, "xyz")),
    report_metrics(reverse_cache),
    ok.

different_keys_in_parallel() ->
    {ok, _} = ei_cache:start_link(
                reverse_cache,
                fun(Key) ->
                        timer:sleep(250),
                        lists:reverse(Key)
                end),
    % We know that each call will take at least 250ms; if we run 4 of them in
    % parallel and it takes more than 500ms, then we know we screwed up.
    % This is a bit timing specific, but I'm not entirely sure how to get
    % around that.
    % @todo If we had a gen_server at the other end that didn't reply to any
    % request until all of them arrived, we'd know that they weren't
    % sequential.
    Then = now(),
    wait_for([
              spawn_opt(fun() -> ?assertEqual("cba", ei_cache:get_value(reverse_cache, "abc")) end, [link, monitor]),
              spawn_opt(fun() -> ?assertEqual("nml", ei_cache:get_value(reverse_cache, "lmn")) end, [link, monitor]),
              spawn_opt(fun() -> ?assertEqual("rqp", ei_cache:get_value(reverse_cache, "pqr")) end, [link, monitor]),
              spawn_opt(fun() -> ?assertEqual("zyx", ei_cache:get_value(reverse_cache, "xyz")) end, [link, monitor])]),
    Elapsed = timer:now_diff(now(), Then),
    io:format("Elapsed = ~p\n", [Elapsed]),
    ?assert(Elapsed =< 500000),
    report_metrics(reverse_cache),
    ok.

repeated_hits() ->
    {ok, _} = ei_cache:start_link(
                reverse_cache,
                fun(Key) ->
                        timer:sleep(120),
                        lists:reverse(Key)
                end),
    ?assertEqual("qwerty", ei_cache:get_value(reverse_cache, "ytrewq")),
    wait_for(
      [spawn_opt(fun() ->
                         ?assertEqual("qwerty", ei_cache:get_value(reverse_cache, "ytrewq")),
                         ?assertEqual("qwerty", ei_cache:get_value(reverse_cache, "ytrewq")),
                         ?assertEqual("qwerty", ei_cache:get_value(reverse_cache, "ytrewq"))
                 end, [link, monitor]) || _ <- lists:seq(1, 333)]),
    report_metrics(reverse_cache),
    ok.

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

run_stress(Iterations, Range, MaxDelay, MaxLag) ->
    {ok, _} = ei_cache:start_link(
                hash_cache,
                fun(Key) ->
                        timer:sleep(crypto:rand_uniform(0, MaxLag + 1)),
                        erlang:phash2(Key)
                end),
    wait_for([spawn_opt(
                fun() ->
                        timer:sleep(crypto:rand_uniform(0, MaxDelay + 1)),
                        Key = crypto:rand_uniform(1, Range),
                        ?assertEqual(erlang:phash2(Key), ei_cache:get_value(hash_cache, Key))
                end, [link, monitor]) || _N <- lists:seq(1, Iterations)]),
    report_metrics(hash_cache).

% @todo Can we have a stress test that runs until it sees at least one of each metric reported?

worker_dies() ->
    {ok, _} = ei_cache:start_link(
                minefield,
                fun(_Key) ->
                        timer:sleep(120),
                        erlang:error(farewell_cruel_world)
                end),
    {Pid, Ref} = spawn_monitor(
      fun() ->
              ei_cache:get_value(minefield, 42)
      end),
    receive
        {'DOWN', Ref, process, Pid, {farewell_cruel_world, _}} -> ok
    end,
    % @todo How to do that again? If the worker dies, the cache should not have
    % a value for that at all.
    ok.

wait_for([]) ->
    ok;
wait_for([{Pid, Ref} | Waits]) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            wait_for(Waits)
    end.

report_metrics(Name) ->
    Counts = ei_cache:get_counts(Name),
    ?debugFmt("~p: ~p\n", [Name, Counts]).

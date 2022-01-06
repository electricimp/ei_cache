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

      ?test(worker_dies_quickly),
      ?test(worker_dies_slowly)
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
    % To assert that the function was not called more than once, we get it to
    % send a message to us and then count them.
    Self = self(),
    {ok, _} = ei_cache:start_link(
                reverse_cache,
                fun(Key) ->
                        Self ! get_value_called,
                        timer:sleep(250),
                        lists:reverse(Key)
                end),
    ?assertEqual("cba", ei_cache:get_value(reverse_cache, "abc")),
    ?assertEqual("cba", ei_cache:get_value(reverse_cache, "abc")),

    self() ! ok,
    ?assertEqual(1, count_get_value_messages()),
    report_metrics(reverse_cache),
    ok.

count_get_value_messages() ->
    count_get_value_messages(0).

count_get_value_messages(Count) ->
    receive
        get_value_called -> count_get_value_messages(Count + 1);
        ok -> Count
    end.

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
    % We have a process that "resolves" lookups by not replying to any request
    % until it's seen all of them. This way we know that they're not
    % sequential.
    Pid = spawn_link(fun() -> expect_requests(4) end),
    {ok, _} = ei_cache:start_link(
                reverse_cache,
                fun(Key) ->
                        Pid ! {request, {self(), Key}},
                        receive
                            {reply, Value} -> Value
                        end
                end),
    wait_for([
              spawn_opt(fun() -> ?assertEqual("cba", ei_cache:get_value(reverse_cache, "abc")) end, [link, monitor]),
              spawn_opt(fun() -> ?assertEqual("nml", ei_cache:get_value(reverse_cache, "lmn")) end, [link, monitor]),
              spawn_opt(fun() -> ?assertEqual("rqp", ei_cache:get_value(reverse_cache, "pqr")) end, [link, monitor]),
              spawn_opt(fun() -> ?assertEqual("zyx", ei_cache:get_value(reverse_cache, "xyz")) end, [link, monitor])]),
    report_metrics(reverse_cache),
    ok.

expect_requests(Expected) ->
    expect_requests([], 0, Expected).

expect_requests(Requests, Count, Expected) when Count =:= Expected ->
    lists:foreach(
      fun({Pid, Key}) ->
              Value = lists:reverse(Key),
              Pid ! {reply, Value}
      end, Requests);

expect_requests(Requests, Count, Expected) ->
    receive
        {request, Request} ->
            expect_requests([Request | Requests], Count + 1, Expected)
    end.

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

worker_dies_quickly() ->
    {ok, _} = ei_cache:start_link(
                minefield,
                fun(_Key) ->
                        erlang:error(farewell_cruel_world)
                end),
    % If the worker dies, we should see an error.
    ?assertMatch({'EXIT', _}, catch ei_cache:get_value(minefield, 42)),
    % ..and the ETS table should no longer contain the promise:
    % ..but we need to serialize on the server first:
    ok = gen_server:call(ei_cache_names:server(minefield), ping),
    ?assertMatch([], ets:match(ei_cache_minefield_tab, '$1')),
    ok.

worker_dies_slowly() ->
    {ok, _} = ei_cache:start_link(
                minefield,
                fun(_Key) ->
                        timer:sleep(120),
                        erlang:error(farewell_cruel_world)
                end),
    % If the worker dies, we should see an error.
    ?assertMatch({'EXIT', _}, catch ei_cache:get_value(minefield, 42)),
    % ..and the ETS table should no longer contain the promise:
    % ..but we need to serialize on the server first:
    ok = gen_server:call(ei_cache_names:server(minefield), ping),
    ?assertMatch([], ets:match(ei_cache_minefield_tab, '$1')),
    ok.

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

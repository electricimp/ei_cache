-module(ei_cache_server).
-export([start_link/2]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
          n, f, t
         }).

start_link(Name, Fun) ->
    gen_server:start_link({local, ei_cache_names:server(Name)}, ?MODULE, [Name, Fun], []).

init([Name, Fun]) ->
    ei_cache_metrics:init(Name),
    % The table is public because the promises also write to it. You don't need
    % to know that, though.
    Tid = ets:new(ei_cache_names:table(Name), [named_table, public]),
    process_flag(trap_exit, true),
    {ok, #state{n = Name, f = Fun, t = Tid}}.

handle_call({get_value, Key}, _From, State = #state{t = T}) ->
    handle_lookup_result(Key, ets:lookup(T, Key), State);

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_lookup_result(Key, [], State = #state{n = Name, f = F, t = T}) ->
    % Miss; we need to spin up a worker and return that.
    ei_cache_metrics:server_miss(Name),
    {ok, Pid} = ei_cache_promise:start_link(F, Key, T),
    Value = {promise, Pid},
    % Use insert_new so that if the promise finishes really quickly, we
    % don't overwrite the value with a now-defunct promise.
    case ets:insert_new(T, {Key, Value}) of
        false ->
            % It's already there.
            % @todo Invent a test that exercises this bit, somehow.
            handle_lookup_result(Key, ets:lookup(T, Key), State);
        _ ->
            {reply, Value, State}
    end;
handle_lookup_result(Key, [{Key, {value, Value}}], State = #state{n = Name}) ->
    ei_cache_metrics:server_hit(Name),
    {reply, {value, Value}, State};
handle_lookup_result(Key, [{Key, {promise, P}}], State = #state{n = Name}) ->
    ei_cache_metrics:server_promise(Name),
    {reply, {promise, P}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    % The promise exited normally; ignore it.
    {noreply, State};
handle_info({'EXIT', Pid, _Error}, State = #state{t = T}) ->
    ets:match_delete(T, {'_', {promise, Pid}}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

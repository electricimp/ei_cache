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

handle_call({get_value, Key}, _From, State = #state{n = Name, f = F, t = T}) ->
    case ets:lookup(T, Key) of
        [] ->
            % Miss; we need to spin up a worker and return that.
            % @todo Who's watching the worker?
            ei_cache_metrics:server_miss(Name),
            {ok, Pid} = ei_cache_promise:start(F, Key, T),
            Value = {promise, Pid},
            % Use insert_new so that if the promise finishes really quickly, we
            % don't overwrite the value with a now-defunct promise.
            case ets:insert_new(T, {Key, Value}) of
                false ->
                    % It's already there.
                    % @todo Invent a test that exercises this bit.
                    {reply, ets:lookup(T, Key), State};
                _ ->
                    {reply, Value, State}
            end;
        [{Key, {value, Value}}] ->
            ei_cache_metrics:server_hit(Name),
            {reply, {value, Value}, State};
        [{Key, {promise, P}}] ->
            ei_cache_metrics:server_promise(Name),
            {reply, {promise, P}, State}
    end;

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

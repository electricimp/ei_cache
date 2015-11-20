-module(ei_cache_metrics).
-export([init/1, client_promise/1, client_hit/1, server_miss/1, server_promise/1, server_hit/1, get_counts/1]).

init(Name) ->
    T = ets:new(metrics_table_name(Name), [named_table, public]),
    ets:insert(T, {client_promises, 0}),
    ets:insert(T, {client_hits, 0}),
    ets:insert(T, {server_misses, 0}),
    ets:insert(T, {server_promises, 0}),
    ets:insert(T, {server_hits, 0}),
    ok.

client_promise(Name) -> increment(Name, client_promises).
client_hit(Name) -> increment(Name, client_hits).
server_miss(Name) -> increment(Name, server_misses).
server_promise(Name) -> increment(Name, server_promises).
server_hit(Name) -> increment(Name, server_hits).

increment(Name, Counter) ->
    ets:update_counter(metrics_table_name(Name), Counter, 1),
    ok.

get_counts(Name) ->
    % I'd like to stick all of the metrics for all caches in a single table,
    % but:
    % - I don't have a process to attach that table to. There's no application
    %   supervisor.
    % - I'd like the counts to be reset if the cache dies in any way, probably.
    T = metrics_table_name(Name),
    Hit1 = ets:lookup_element(T, client_hits, 2),
    Hit2 = ets:lookup_element(T, server_hits, 2),
    Miss0 = ets:lookup_element(T, client_promises, 2),
    Miss1 = ets:lookup_element(T, server_misses, 2),
    Miss2 = ets:lookup_element(T, server_promises, 2),
    Hits = Hit1 + Hit2,
    Misses = Miss0 + Miss1 + Miss2,
    Requests = Hits + Misses,
    [{client_hits, Hit1},
     {server_hits, Hit2},
     {total_hits, Hits},
     {client_promises, Miss0},
     {server_misses, Miss1},
     {server_promises, Miss2},
     {total_misses, Misses},
     {total_requests, Requests},
     {hit_ratio, safe_div(Hits, Requests)},
     {miss_ratio, safe_div(Misses, Requests)}].

safe_div(_N, 0) ->
    0.0;
safe_div(N, D) ->
    N / D.

metrics_table_name(Name) ->
    get_name(Name, "_metrics_tab").

get_name(Name, Suffix) when is_atom(Name) ->
    list_to_atom("ei_cache_" ++ atom_to_list(Name) ++ Suffix).

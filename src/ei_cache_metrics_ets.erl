-module(ei_cache_metrics_ets).
-export([init/1, new_spiral/2, increment/2, get_counts/1]).

init(Name) ->
    % Create an ETS table to hold the metrics. This is called from
    % ei_cache_server, so the table is owned by the gen_server process.
    T = metrics_table_name(Name),
    T = ets:new(T, [named_table, public]).

new_spiral(Name, Key) ->
    ets:insert(metrics_table_name(Name), {Key, 0}).

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
    ClientHits = ets:lookup_element(T, client_hits, 2),
    ServerHits = ets:lookup_element(T, server_hits, 2),
    ClientPromises = ets:lookup_element(T, client_promises, 2),
    ServerMisses = ets:lookup_element(T, server_misses, 2),
    ServerPromises = ets:lookup_element(T, server_promises, 2),
    Hits = ets:lookup_element(T, hits, 2),
    Misses = ets:lookup_element(T, misses, 2),
    Requests = ets:lookup_element(T, requests, 2),
    [{client_hits, ClientHits},
     {server_hits, ServerHits},
     {total_hits, Hits},
     {client_promises, ClientPromises},
     {server_misses, ServerMisses},
     {server_promises, ServerPromises},
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

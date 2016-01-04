-module(ei_cache_names).
-export([supervisor/1, server/1, table/1]).

supervisor(Name) ->
    get_name(Name, "_sup").

server(Name) ->
    get_name(Name, "_server").

table(Name) ->
    get_name(Name, "_tab").

get_name(Name, Suffix) when is_atom(Name) ->
    list_to_atom("ei_cache_" ++ atom_to_list(Name) ++ Suffix).

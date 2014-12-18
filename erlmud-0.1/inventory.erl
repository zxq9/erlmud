-module(inventory).
-export([new/0, from_list/1, to_list/1, add/2, append/2,
         find/2, find/3, get_element/2, find_element/2, pids/1,
         drop/2, drop_pid/2, drop_target/2, drop_target/3,
         weight/1]).

%% Interface
new() -> {0, {[], dict:new()}}.

from_list([])   -> new();
from_list(List) -> from_list(List, new()).

from_list(List, Inv) -> lists:foldl(fun add/2, Inv, List).

to_list({_, {Manifest, _}}) -> Manifest.

add(Elem = {Pid, Names, _}, {_, {Manifest, Aliases}}) ->
    NewManifest = [Elem | Manifest],
    NewAliases = store(Names, Pid, Aliases),
    {calc_weight(NewManifest), {NewManifest, NewAliases}}.

find({Name, Index}, {_, {_, Aliases}}) ->
    case dict:find(Name, Aliases) of
        {ok, Pids} ->
            Length = length(Pids),
            if
                Index  <   1    -> {error, absent};
                Length =:= 0    -> {error, absent};
                Length >= Index -> {ok, lists:nth(Index, Pids)};
                Length <  Index -> {ok, lists:last(Pids)}
            end;
        error ->
            {error, absent}
    end;
find(Name, {_, {_, Aliases}}) ->
    case dict:find(Name, Aliases) of
        {ok, Pids} -> {ok, hd(Pids)};
        error      -> {error, absent}
    end.

find(Name, Index, Inv) ->
    find({Name, Index}, Inv).

get_element(Pid, Inv) ->
    case lists:keyfind(Pid, 1, to_list(Inv)) of
        false -> {error, absent};
        Elem  -> {ok, Elem}
    end.

find_element(Target, Inv) ->
    case find(Target, Inv) of
        {ok, Pid}      -> get_element(Pid, Inv);
        E = {error, _} -> E
    end.

pids(Inv) -> [Pid || {Pid, _, _} <- to_list(Inv)].

append(List, Inv) -> from_list(List, Inv).

drop(Elem, Inv) ->
    case lists:member(Elem, to_list(Inv)) of
        true  -> {ok, remove(Elem, Inv)};
        false -> {absent, Inv}
    end.

drop_pid(Pid, Inv) ->
    case get_element(Pid, Inv) of
        {ok, Elem}      -> {ok, remove(Elem, Inv)};
        {error, absent} -> {absent, Inv}
    end.

drop_target(Target, Inv) ->
    case find(Target, Inv) of
        {ok, Pid} -> drop_pid(Pid, Inv);
        Error     -> Error
    end.

drop_target(Name, Index, Inv) ->
    drop_target({Name, Index}, Inv).

weight({Weight, _}) -> Weight.

%% Magic
store([], _, Aliases) ->
    Aliases;
store([Name | Names], Pid, Aliases) ->
    Updated = case dict:find(Name, Aliases) of
        {ok, Pids} -> dict:store(Name, [Pid | Pids], Aliases);
        error      -> dict:store(Name, [Pid], Aliases)
    end,
    store(Names, Pid, Updated).

remove(Elem = {Pid, Names, _}, {_, {Manifest, Aliases}}) ->
    NewManifest = lists:delete(Elem, Manifest),
    NewAliases = scrub(Names, Pid, Aliases),
    {calc_weight(NewManifest), {NewManifest, NewAliases}}.

scrub([], _, Aliases) ->
    Aliases;
scrub([Name | Names], Pid, Aliases) ->
    Updated = case lists:delete(Pid, dict:fetch(Name, Aliases)) of
        []   -> dict:erase(Name, Aliases);
        Pids -> dict:store(Name, Pids, Aliases)
    end,
    scrub(Names, Pid, Updated).

calc_weight(Manifest) ->
    SumWeight = fun({_, _, {_, _, W, _}}, A) -> W + A end,
    lists:foldl(SumWeight, 0, Manifest).

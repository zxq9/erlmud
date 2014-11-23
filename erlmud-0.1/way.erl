-module(way).
-export([start_link/2,
         front/1, in/1, back/1, out/1,
         enter/2]).

%% Type interface
front({{Name, _}, _}) -> Name.
in({{_, InLoc}, _})   -> InLoc.
back({_, {Name, _}})  -> Name.
out({_, {_, OutLoc}}) -> OutLoc.

%% Interface
enter(WayPid, Entity) -> em_lib:call(WayPid, enter, Entity).

%% Startup
start_link(Parent, Conf) ->
    spawn_link(fun() -> init(Parent, Conf) end).

init(OutPid, WayID) ->
    note("Starting up"),
    wayman:reg_live({WayID, self()}),
    State = {WayID, OutPid},
    open(State).

%% Service
open(State = {WayID, OutPid}) ->
  receive
    {From, Ref, {enter, Entity}} ->
        From ! {Ref, transit(Entity, OutPid)},
        open(State);
    status ->
        note("Status:~n  Id: ~p~n  OutPid: ~p",
             [WayID, OutPid]),
        open(State);
    shutdown ->
        note("~p shutting down.", [WayID]),
        exit(shutdown);
    Any ->
        note("Received ~p", [Any]),
        open(State)
  end.

%% Other states belong here.
%closed(State) ->
%locked(State) ->

%% Request handlers
% Alternate exit trickery and other conditional processing belongs here
transit(Entity, OutPid) ->
    loc:arrive(OutPid, Entity).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

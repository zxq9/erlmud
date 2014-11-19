-module(way).
-export([start_link/2,
         name/1, in/1, out/1,
         enter/2]).

%% Type interface
name({Name, _, _}) -> Name.

in({_, In, _}) -> In.

out({_, _, Out}) -> Out.

%% Interface
enter(WayPid, Mob) -> em_lib:call(WayPid, enter, Mob).

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
    {From, Ref, {enter, Mob}} ->
        From ! {Ref, transit(Mob, OutPid, WayID)},
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

%closed(State) ->
%locked(State) ->

%% Request handlers
% Alternate exit trickery and other conditional processing belongs here
transit(Mob, OutPid, WayID) ->
    loc:arrive(OutPid, Mob, WayID).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

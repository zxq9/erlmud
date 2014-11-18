-module(way).
-export([start_link/1,
         name/1, in/1, out/1]).

%% Interface
name({Name, _, _}) -> Name.

in({_, In, _}) -> In.

out({_, _, Out}) -> Out.

%% Startup
start_link(Conf) ->
    spawn_link(fun() -> init(Conf) end).

init(WayID) ->
    note("Starting up"),
    wayman:reg_live({WayID, self()}),
    InPid = none,
    OutPid = none,
    State = {WayID, InPid, OutPid},
    open(State).

%% Service
open(State = {WayID, InPid, OutPid}) ->
  receive
    status ->
        note("Status:~n  Id: ~p~n  InPid: ~p~n  OutPid: ~p",
             [WayID, InPid, OutPid]),
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

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

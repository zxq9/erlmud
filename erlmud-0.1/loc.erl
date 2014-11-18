-module(loc).
-export([start_link/1, code_change/1,
         look/1, look/2, mob_jump/2]).

%% Interface
look(LocPid) -> em_lib:call(LocPid, look).

look(LocPid, Target) -> em_lib:call(LocPid, look, Target).

mob_jump(LocPid, Mob) -> em_lib:call(LocPid, jump_in, Mob).

%% Startup
start_link(Conf) ->
    spawn_link(fun() -> init(Conf) end).

init(Conf = {Id, {Name, Desc}}) ->
    note("Initializing with ~p", [Conf]),
    Mobs = [],
    Objs = [],
    loop({Id, Name, Desc, Mobs, Objs}).

%% Service
loop(State = {Id, Name, Desc, Mobs, Objs}) ->
  receive
    {audible, Origin, Sound} ->
        echo(Origin, Sound, State),
        loop(State);
    {visible, Origin, Action} ->
        reflect(Origin, Action, State),
        loop(State);
    {From, Ref, look} ->
        From ! {Ref, {Name, Desc, Mobs, Objs}},
        loop(State);
    {From, Ref, {look, Target}} ->
        View = look_at(Target, Mobs),
        From ! {Ref, View},
        loop(State);
    {From, Ref, {jump_in, Mob}} ->
        NewMobs = jump_in(Mob, Mobs),
        From ! {Ref, ok},
        loop({Id, Name, Desc, NewMobs, Objs});
    status ->
        note("Status:~n  Id: ~p~n  Name: ~p~n  Desc: ~p"
             "  Mobs: ~p~n  Objects: ~p",
             [Id, Name, Desc, Mobs, Objs]),
        loop(State);
    code_change ->
        ?MODULE:code_change(State);
    shutdown ->
        note("Shutting down."),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(State)
  end.

%% Request handlers
look_at(Target, Mobs) ->
    case lists:keyfind(Target, 1, Mobs) of
        {Name, _, Desc} -> {mob, {Name, Desc}};
        false           -> not_seen
    end.

jump_in(Mob = {_, Pid, _}, Mobs) ->
    note("Jumping in ~p", [Mob]),
    link(Pid),
    [Mob | Mobs].

echo(Origin, Sound, {_, _, _, Mobs, _}) ->
    Event = string:join([Origin, Sound], " "),
    [MPid ! {observe, Event} || {_, MPid, _} <- Mobs].

reflect(Origin, Sound, {_, _, _, Mobs, _}) ->
    Event = string:join([Origin, Sound], " "),
    [MPid ! {observe, Event} || {_, MPid, _} <- Mobs].

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

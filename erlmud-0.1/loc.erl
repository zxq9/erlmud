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

init(Conf = {ID, {Name, Desc}}) ->
    note("Initializing with ~p", [Conf]),
    Info = {Name, Desc},
    Mobs = [],
    Objs = [],
    Manifest = {Mobs, Objs},
    Ways = init_ways(ID),
    loop({ID, Info, Manifest, Ways}).

init_ways(ID) ->
    Entrances = wayman:get_entrances(ID),
    Exits = wayman:get_exits(ID),
    LiveIn = activate(Entrances),
    LiveOut = check(Exits),
    neighbors_monitor(LiveIn),
    {{LiveIn, LiveOut}, {Entrances, Exits}}.

activate(Entrances) ->
    Live = [{Way, way:start_link(Way)} || Way <- Entrances],
    [wayman:reg_live(Way) || Way <- Live],
    Live.

check(Exits) ->
    IDs = [{WayID, wayman:get_pid(WayID)} || WayID <- Exits],
    Alive = [{WayID, WayPid} || {WayID, {ok, WayPid}} <- IDs],
    [{WayID, WayPid, monitor(process, WayPid)} || {WayID, WayPid} <- Alive].

neighbors_monitor(LiveIn) ->
    NPids = [{locman:get_pid(way:in(WayID)), WayID, WayPid} || {WayID, WayPid} <- LiveIn],
    [LocPid ! {monitor, {way, WayID, WayPid}} || {{ok, LocPid}, WayID, WayPid} <- NPids],
    ok.

%% Service
loop(State = {ID,
              Info = {Name, Desc},
              Manifest = {Mobs, Objs},
              Ways = {{LiveIn, LiveOut}, {Entrances, Exits}}}) ->
  receive
    {audible, Origin, Sound} ->
        echo(Origin, Sound, Mobs),
        loop(State);
    {visible, Origin, Action} ->
        reflect(Origin, Action, Mobs),
        loop(State);
    {From, Ref, look} ->
        From ! {Ref, {Info, Manifest, Ways}},
        loop(State);
    {From, Ref, {look, Target}} ->
        View = look_at(Target, Mobs),
        From ! {Ref, View},
        loop(State);
    {From, Ref, {jump_in, Mob}} ->
        NewMobs = jump_in(Mob, Mobs),
        From ! {Ref, ok},
        loop({ID, Info, {NewMobs, Objs}, Ways});
    {monitor, {way, WayID, WayPid}} ->
        NewLiveOut = monitor_exit(WayID, WayPid, LiveOut, Exits),
        loop({ID, Info, Manifest, {{LiveIn, NewLiveOut}, {Entrances, Exits}}});
    Message = {'DOWN', _, process, _, _} ->
        NewLiveOut = handle_down(Message, LiveOut),
        loop({ID, Info, Manifest, {{LiveIn, NewLiveOut}, {Entrances, Exits}}});
    status ->
        note("Status:~n  ID: ~p~n  Name: ~p~n  Desc: ~p"
             "  Mobs: ~p~n  Objects: ~p~n"
             "  LiveIn: ~p~n  LiveOut: ~p~n"
             "  Entrances: ~p~n  Exits: ~p",
             [ID, Name, Desc, Mobs, Objs, LiveIn, LiveOut, Entrances, Exits]),
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

echo(Origin, Sound, Mobs) ->
    Event = string:join([Origin, Sound], " "),
    [MPid ! {observe, Event} || {_, MPid, _} <- Mobs].

reflect(Origin, Sound, Mobs) ->
    Event = string:join([Origin, Sound], " "),
    [MPid ! {observe, Event} || {_, MPid, _} <- Mobs].

monitor_exit(WayID, WayPid, LiveOut, Exits) ->
    LiveOutIDs = [ID || {_, ID, _} <- LiveOut],
    case lists:member(WayID, Exits) and not lists:member(WayID, LiveOutIDs) of
        true ->
            Mon = monitor(process, WayPid),
            [{WayID, WayPid, Mon} | LiveOut];
        false ->
            LiveOut
    end.

handle_down(Message = {_, Ref, _, _, _}, LiveOut) ->
    case lists:keyfind(Ref, 3, LiveOut) of
        Out = {_, _, _} ->
            lists:delete(Out, LiveOut);
        false ->
            note("Received ~p", [Message]),
            LiveOut
    end.

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

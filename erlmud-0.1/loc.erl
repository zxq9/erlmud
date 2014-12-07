-module(loc).
-export([start_link/1, code_change/1,
         event/2, look/1, action/3, depart/3, arrive/2, load/2, drop/2]).

% Entity = {Name, Pid, Aliases, BaseMod, IlkMod}

%% Interface
event(LocPid, Data) ->
    LocPid ! {event, Data}.

look(LocPid) ->
    em_lib:call(LocPid, look).

action(LocPid, Target, Event) ->
    em_lib:call(LocPid, action, {Target, Event}).

arrive(LocPid, Entity) ->
    em_lib:call(LocPid, arrive, Entity).

depart(LocPid, Entity, Exit) ->
    em_lib:call(LocPid, depart, {Entity, Exit}).

load(LocPid, Entity) ->
    em_lib:call(LocPid, load, Entity).

drop(LocPid, Entity) ->
    em_lib:call(LocPid, drop, Entity).

%% Startup
start_link(Conf) ->
    spawn_link(fun() -> init(Conf) end).

init(Conf = {ID, {Name, Desc}}) ->
    note("Initializing with ~p", [Conf]),
    Info = {Name, Desc},
    Manifest = [],
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
    Self = self(),
    [{Way, way:start_link(Self, Way)} || Way <- Entrances].

check(Exits) ->
    IDs = [{WayID, wayman:get_pid(WayID)} || WayID <- Exits],
    Alive = [{WayID, WayPid} || {WayID, {ok, WayPid}} <- IDs],
    [{way:front(WayID), WayID, WayPid, monitor(process, WayPid)} || {WayID, WayPid} <- Alive].

neighbors_monitor(LiveIn) ->
    Neighbors = [{locman:get_pid(way:in(WayID)), WayID, WayPid} || {WayID, WayPid} <- LiveIn],
    [LocPid ! {monitor, {way, WayID, WayPid}} || {{ok, LocPid}, WayID, WayPid} <- Neighbors],
    ok.

%% Service
loop(State = {ID,
              Info = {Name, Desc},
              Manifest,
              Ways = {{LiveIn, LiveOut}, {Entrances, Exits}}}) ->
  receive
    {event, Data} ->
        broadcast(Data, Manifest),
        loop(State);
    {From, Ref, look} ->
        From ! {Ref, State},
        loop(State);
    {From, Ref, {action, {Target, Event}}}->
        Result = arbitrate(Target, Event, Manifest),
        From ! {Ref, Result},
        loop(State);
    {From, Ref, {arrive, Entity}} ->
        {Result, NewManifest} = arrival(Entity, Manifest, ID),
        From ! {Ref, Result},
        loop({ID, Info, NewManifest, Ways});
    {From, Ref, {depart, {Entity, Exit}}} ->
        {Result, NewManifest} = departure(Entity, Exit, Manifest, LiveOut),
        From ! {Ref, Result},
        loop({ID, Info, NewManifest, Ways});
    {From, Ref, {load, Entity}} ->
        NewManifest = accept(Entity, Manifest),
        From ! {Ref, ok},
        loop({ID, Info, NewManifest, Ways});
    {From, Ref, {drop, Entity}} ->
        NewManifest = remove(Entity, Manifest),
        From ! {Ref, ok},
        loop({ID, Info, NewManifest, Ways});
    {monitor, {way, WayID, WayPid}} ->
        NewLiveOut = monitor_exit(WayID, WayPid, LiveOut, Exits),
        loop({ID, Info, Manifest, {{LiveIn, NewLiveOut}, {Entrances, Exits}}});
    Message = {'DOWN', _, process, _, _} ->
        NewLiveOut = handle_down(Message, LiveOut),
        loop({ID, Info, Manifest, {{LiveIn, NewLiveOut}, {Entrances, Exits}}});
    status ->
        note("Status:~n"
             "  ID: ~p~n  Name: ~p~n  Desc: ~p~n"
             "  Manifest: ~p~n"
             "  LiveIn: ~p~n  LiveOut: ~p~n"
             "  Entrances: ~p~n  Exits: ~p",
             [ID, Name, Desc, Manifest, LiveIn, LiveOut, Entrances, Exits]),
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
broadcast(Data, Manifest) ->
    Pids = pids(Manifest),
    em_lib:broadcast(Pids, Data),
    ok.

arbitrate(Target, Event, Manifest) ->
    case lists:keyfind(Target, 1, Manifest) of
        {_, Pid, _, Mod, _} -> Mod:incoming(Pid, Event);
        false               -> {error, absent}
    end.

arrival(Entity = {_, Pid, _, _, _}, Manifest, ID) ->
    link(Pid),
    {{ok, {ID, self()}}, [Entity | Manifest]}.

departure(Entity = {_, Pid, _, _, _}, ExitName, Manifest, LiveOut) ->
    case lists:keyfind(ExitName, 1, LiveOut) of
        {_, {_, {OutName, _}}, ExitPid, _} ->
            unlink(Pid),
            {{ok, ExitPid, OutName}, lists:delete(Entity, Manifest)};
        false ->
            {{error, noexit}, Manifest}
    end.

accept(Entity = {_, Pid, _, _, _}, Manifest) ->
    link(Pid),
    [Entity | Manifest].

remove(Entity = {_, Pid, _, _, _}, Manifest) ->
    unlink(Pid),
    lists:delete(Entity, Manifest).

monitor_exit(WayID, WayPid, LiveOut, Exits) ->
    LiveOutIDs = [ID || {_, ID, _, _} <- LiveOut],
    case lists:member(WayID, Exits) and not lists:member(WayID, LiveOutIDs) of
        true ->
            Mon = monitor(process, WayPid),
            [{way:front(WayID), WayID, WayPid, Mon} | LiveOut];
        false ->
            LiveOut
    end.

handle_down(Message = {_, Ref, _, _, _}, LiveOut) ->
    case lists:keyfind(Ref, 4, LiveOut) of
        Out = {_, _, _, _} ->
            lists:delete(Out, LiveOut);
        false ->
            note("Received ~p", [Message]),
            LiveOut
    end.

%% Magic
pids(Manifest) -> [Pid || {_, Pid, _, _, _} <- Manifest].

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

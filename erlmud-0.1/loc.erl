-module(loc).
-export([start_link/1, code_change/1,
         event/2, look/1, action/3, depart/3, arrive/2, load/2, drop/2]).

% Entity = {Pid, Names, Opaque}

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
    Inventory = [],
    Aliases = dict:new(),
    Manifest = {Inventory, Aliases},
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
              Manifest = {Inventory, Aliases},
              Ways = {{LiveIn, LiveOut}, {Entrances, Exits}}}) ->
  receive
    {event, Data} ->
        broadcast(Data, Inventory),
        loop(State);
    {From, Ref, look} ->
        From ! {Ref, State},
        loop(State);
    {From, Ref, {action, {Target, Event}}}->
        Result = arbitrate(From, Target, Event, Aliases),
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
broadcast(Data, Inventory) ->
    Pids = pids(Inventory),
    em_lib:broadcast(Pids, Data),
    ok.

arbitrate(Actor, Target, Event, Aliases) ->
    case acquire(Target, Aliases) of
        M = {error, _}         -> M;
        Pid when Pid =:= Actor -> {error, self};
        Pid                    -> em_lib:incoming(Pid, Event)
    end.

arrival(Entity, Manifest, ID) ->
    NewManifest = accept(Entity, Manifest),
    {{ok, {ID, self()}}, NewManifest}.

departure(Entity, ExitName, Manifest, LiveOut) ->
    case lists:keyfind(ExitName, 1, LiveOut) of
        {_, {_, {OutName, _}}, ExitPid, _} ->
            {{ok, ExitPid, OutName}, remove(Entity, Manifest)};
        false ->
            {{error, noexit}, Manifest}
    end.

accept(Entity = {Pid, Names, _}, {Inventory, Aliases}) ->
    link(Pid),
    {[Entity | Inventory], store_aliases(Names, Pid, Aliases)}.

remove(Entity = {Pid, _, _}, Manifest) ->
    unlink(Pid),
    scrub_manifest(Entity, Manifest).

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
pids(Inventory) -> [Pid || {Pid, _, _} <- Inventory].

acquire({Name, Index}, Aliases) ->
    case dict:find(Name, Aliases) of
        {ok, Pids} ->
            Length = length(Pids),
            if
                Length =:= 0    -> {error, absent};
                Length >= Index -> lists:nth(Index, Pids);
                Length <  Index -> lists:last(Pids)
            end;
        error ->
            {error, absent}
    end;
acquire(Target, Aliases) ->
    case dict:find(Target, Aliases) of
        {ok, Pids} -> hd(Pids);
        error      -> {error, absent}
    end.

store_aliases([], _, Aliases) ->
    Aliases;
store_aliases([Name | Names], Pid, Aliases) ->
    Updated = case dict:find(Name, Aliases) of
        {ok, Pids} -> dict:store(Name, [Pid | Pids], Aliases);
        error      -> dict:store(Name, [Pid], Aliases)
    end,
    store_aliases(Names, Pid, Updated).

scrub_manifest(Entity = {Pid, Names, _}, {Inventory, Aliases}) ->
    NewInventory = lists:delete(Entity, Inventory),
    NewAliases = scrub_aliases(Names, Pid, Aliases),
    {NewInventory, NewAliases}.

scrub_aliases([], _, Aliases) ->
    Aliases;
scrub_aliases([Name | Names], Pid, Aliases) ->
    Updated = case lists:delete(Pid, dict:fetch(Name, Aliases)) of
        []   -> dict:erase(Name, Aliases);   
        Pids -> dict:store(Name, Pids, Aliases)
    end,
    scrub_aliases(Names, Pid, Updated).

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

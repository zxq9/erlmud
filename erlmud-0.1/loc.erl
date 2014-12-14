-module(loc).
-export([start_link/1, code_change/1,
         event/2, look/1, action/3, depart/3, arrive/2, target/2, load/2, drop/2]).

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

target(LocPid, Name) ->
    em_lib:call(LocPid, target, Name).

load(LocPid, Entity) ->
    em_lib:call(LocPid, load, Entity).

drop(LocPid, Entity) ->
    em_lib:call(LocPid, drop, Entity).

%% Startup
start_link(Conf) ->
    spawn_link(fun() -> init(Conf) end).

init(Conf = {ID, {Name, Desc}}) ->
    random:seed(now()),
    note("Initializing with ~p", [Conf]),
    Info = {Name, Desc},
    Inventory = inventory:new(),
    Ways = init_ways(ID),
    loop({ID, Info, Inventory, Ways}).

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
              Inventory,
              Ways = {{LiveIn, LiveOut}, {Entrances, Exits}}}) ->
  receive
    {event, Data} ->
        broadcast(Data, Inventory),
        loop(State);
    {From, Ref, look} ->
        From ! {Ref, view(State)},
        loop(State);
    {From, Ref, {action, {Target, Event}}}->
        Result = arbitrate(From, Target, Event, Inventory),
        From ! {Ref, Result},
        loop(State);
    {From, Ref, {arrive, Entity}} ->
        {Result, NewInventory} = arrival(Entity, Inventory, ID),
        From ! {Ref, Result},
        loop({ID, Info, NewInventory, Ways});
    {From, Ref, {depart, {Entity, Exit}}} ->
        {Result, NewInventory} = departure(Entity, Exit, Inventory, LiveOut),
        From ! {Ref, Result},
        loop({ID, Info, NewInventory, Ways});
    {From, Ref, {target, Name}} ->
        From ! {Ref, inventory:find(Name, Inventory)},
        loop(State);
    {From, Ref, {load, Entity}} ->
        NewInventory = accept(Entity, Inventory),
        From ! {Ref, ok},
        loop({ID, Info, NewInventory, Ways});
    {From, Ref, {drop, Entity}} ->
        NewInventory = remove(Entity, Inventory),
        From ! {Ref, ok},
        loop({ID, Info, NewInventory, Ways});
    {From, Ref, {transfer, Order}} ->
        NewInventory = transfer(From, Ref, Order, Inventory),
        loop({ID, Info, NewInventory, Ways});
    {monitor, {way, WayID, WayPid}} ->
        NewLiveOut = monitor_exit(WayID, WayPid, LiveOut, Exits),
        loop({ID, Info, Inventory, {{LiveIn, NewLiveOut}, {Entrances, Exits}}});
    Message = {'DOWN', _, process, _, _} ->
        NewLiveOut = handle_down(Message, LiveOut),
        loop({ID, Info, Inventory, {{LiveIn, NewLiveOut}, {Entrances, Exits}}});
    status ->
        Inv = inventory:to_list(Inventory),
        note("Status:~n"
             "  ID: ~p~n  Name: ~p~n  Desc: ~p~n"
             "  Inventory: ~p~n"
             "  LiveIn: ~p~n  LiveOut: ~p~n"
             "  Entrances: ~p~n  Exits: ~p",
             [ID, Name, Desc, Inv, LiveIn, LiveOut, Entrances, Exits]),
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
    Pids = inventory:pids(Inventory),
    em_lib:broadcast(Pids, Data),
    ok.

view({_, {Name, Description}, Inventory, {{_, Exits}, _}}) ->
    {Name, Description, inventory:to_list(Inventory), Exits}.

arbitrate(Actor, Target, Event, Inventory) ->
    case inventory:find(Target, Inventory) of
        {ok, Pid} when Pid =:= Actor -> {error, self};
        {ok, Pid}                    -> em_lib:incoming(Pid, Event);
        M = {error, _}               -> M
    end.

arrival(Entity, Inventory, ID) ->
    NewInventory = accept(Entity, Inventory),
    {{ok, {ID, self()}}, NewInventory}.

departure(Entity, ExitName, Inventory, LiveOut) ->
    case lists:keyfind(ExitName, 1, LiveOut) of
        {_, {_, {OutName, _}}, ExitPid, _} ->
            {{ok, ExitPid, OutName}, remove(Entity, Inventory)};
        false ->
            {{error, noexit}, Inventory}
    end.

accept(Entity = {Pid, _, _}, Inventory) ->
    link(Pid),
    inventory:add(Entity, Inventory).

remove(Entity = {Pid, _, _}, Inventory) ->
    unlink(Pid),
    {ok, NewInventory} = inventory:drop(Entity, Inventory),
    NewInventory.

transfer(From, Ref, {Target, TRef}, Inventory) ->
    case inventory:find(Target, Inventory) of
        M = {ok, TPid}    ->
            From ! {Ref, M},
            receive
                {ok, TRef} -> inventory:drop_pid(TPid, Inventory)
                after 1000 -> exit({error, stalled_transfer})
            end;
        M = {error, _} ->
            From ! {Ref, M},
            Inventory
    end.

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

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

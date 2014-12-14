-module(mobman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/1,
         spawn_mob/1, relocate/2,
         species_index/0]).

%% interface
spawn_mob(MobData) -> call({spawn_mob, MobData}).

relocate(Mob, default) -> relocate(Mob, default_loc(Mob));
relocate(Mob, LocID)   -> call({jump, {Mob, LocID}}).

species_index() -> call(species).

call(Request) -> em_lib:call(?MODULE, Request).

%% Startup
start(Parent)            -> start(Parent, []).
start(Parent, Conf)      -> starter(fun spawn/1, Parent, Conf).
start_link(Parent)       -> start_link(Parent, []).
start_link(Parent, Conf) -> starter(fun spawn_link/1, Parent, Conf).

starter(Spawn, Parent, Conf) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = Spawn(fun() -> init(Parent, Conf) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init(Parent, Conf) ->
    random:seed(now()),
    process_flag(trap_exit, true),
    note("Initializing with ~p", [Conf]),
    Species = proplists:get_value(species, Conf),
    Live = [],
    loop({Parent, Live, Species}).

%% Service
loop(State = {Parent, Live, Species}) ->
  receive
    {Controller, Ref, {spawn_mob, MobData}} ->
        {MobPid, NewLive} = spawn_mob(Controller, MobData, Live),
        Controller ! {Ref, MobPid},
        loop({Parent, NewLive, Species});
    {From, Ref, {jump, {Mob, LocID}}} ->
        NewLoc = jump(Mob, LocID),
        From ! {Ref, NewLoc},
        loop(State);
    {From, Ref, species} ->
        From ! {Ref, Species},
        loop(State);
    {'EXIT', Parent, Reason} ->
        note("Parent~tp died with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]);
    Message = {'EXIT', _, _} ->
        NewLive = handle_exit(Live, Message),
        loop({Parent, NewLive, Species});
    status ->
        note("Status:~n  Parent: ~p~n  Live: ~p~n  Species: ~p",
             [Parent, Live, Species]),
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

%% Controller calls
spawn_mob(Controller, MobData = {{Mod, _}, _}, Live) ->
    Ilk = Mod:read(ilk, MobData),
    MobPid = mob:start_link(Controller, MobData),
    NewLive = [{MobPid, Ilk} | Live],
    {MobPid, NewLive}.

%% Mob wrangling
jump(Mob, LocID) ->
    case locman:get_pid(LocID) of
        {ok, LocPid} ->
            loc:load(LocPid, Mob),
            {LocID, LocPid};
        {error, _} ->
            jump(Mob, default_loc(Mob))
    end.

%% Magic
handle_exit(Live, Message = {_, Pid, _}) ->
    case lists:keyfind(Pid, 1, Live) of
        Mob = {_, _} ->
            note("~p exited with ~p", [Mob, Message]),
            lists:delete(Mob, Live);
        false ->
            note("Received ~p", [Message]),
            Live
    end.

default_loc(_) -> {0,0,0}.

%% Code changer
code_change(State = {_, Live, _}) ->
    note("Changing code."),
    [MobPid ! code_change || {MobPid, _} <- Live],
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

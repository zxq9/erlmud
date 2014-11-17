-module(mobman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/1,
         spawn_minion/1]).

%% interface
spawn_minion(MobData) -> call({spawn_minion, MobData}).

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

init(Parent, IlkMods) ->
    process_flag(trap_exit, true),
    note("Notional initialization with ~p", [IlkMods]),
    loop({Parent, IlkMods}).

%% Service
loop(State = {Parent, IlkMods}) ->
  receive
    {Controller, Ref, {spawn_minion, MobData}} ->
        MobPid = spawn_minion(Controller, MobData, IlkMods),
        Controller ! {Ref, MobPid},
        loop(State);
    {'EXIT', Parent, Reason} ->
        note("Parent~tp died with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]);
    status ->
        note("Status:~n  Parent: ~p~n  IlkMods: ~p", [Parent, IlkMods]),
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
spawn_minion(Controller, MobData = {_, {Ilk, _, _}}, IlkMods) ->
    Module = proplists:get_value(Ilk, IlkMods),
    Module:start_link(Controller, MobData).

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

-module(netman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/3]).

%% Startup
start(Parent)                -> start(Parent, []).
start(Parent, Services)      -> starter(fun spawn/1, Parent, Services).
start_link(Parent)           -> start_link(Parent, []).
start_link(Parent, Services) -> starter(fun spawn_link/1, Parent, Services).

starter(Spawn, Parent, Services) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = Spawn(fun() -> init(Parent, Services) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init(Parent, Services) ->
    process_flag(trap_exit, true),
    note("Starting services ~tp", [Services]),
    {ok, Running} = init_services(Services, []),
    loop(Parent, Running, Services).

init_services([], A) -> {ok, A};
init_services([{Module, Func, Args} | Rest], A)  ->
    note("Starting ~p", [Module]),
    {ok, Pid} = apply(Module, Func, [self() | Args]),
    init_services(Rest, [{Pid, Module} | A]).

%% Service
loop(Parent, Running, Services) ->
  receive
    status ->
        note("Active services: ~tp", [Running]),
        loop(Parent, Running, Services);
    {'EXIT', Parent, Reason} ->
        note("Parent~tp died with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]),
        shutdown(Running);
    {'EXIT', Pid, Reason} ->
        Service = proplists:get_value(Pid, Running),
        note("~tp died with ~tp", [Service, Reason]),
        init(Parent, Services);
    code_change ->
        ?MODULE:code_change(Parent, Running, Services);
    shutdown ->
        note("Shutting down."),
        Parent ! {netman, shutdown},
        shutdown(Running),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(Parent, Running, Services)
  end.

shutdown(Running) ->
    note("Shutting down subordinates..."),
    Pids = live_pids(Running),
    em_lib:broadcast(Pids, shutdown),
    ok.

%% Magic
live_pids(Running) ->
    [Pid || {Pid, _} <- Running].

%% Code changer
code_change(Parent, Running, Services) ->
    note("Changing code."),
    Pids = live_pids(Running),
    em_lib:broadcast(Pids, code_change),
    loop(Parent, Running, Services).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

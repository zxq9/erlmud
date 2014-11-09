-module(erlmud).
-export([start/0, code_change/2]).

start() ->
    true = register(erlmud, spawn(fun() -> init() end)).

init() ->
    process_flag(trap_exit, true),
    note("Starting up."),
    Services = [{chanman, start_link, []},
                {accman, start_link, []},
                {wayman, start_link, []},
                {locman, start_link, []},
                {objman, start_link, []},
                {mobman, start_link, []},
                {netman, start_link, [{telnet, start_link, [2222]}]}],
    {ok, Running} = init(Services, []),
    loop(Running, Services).

init([], A) -> {ok, A};
init([{Module, Func, Args} | Rest], A) ->
    {ok, Pid} = apply(Module, Func, [self(), Args]),
    init(Rest, [{Pid, Module} | A]).

%% Service
loop(Running, Services) ->
  receive
    {From, Ref, {info, running}} ->
        From ! {Ref, Running},
        loop(Running, Services);
    {From, Ref, {info, services}} ->
        From ! {Ref, Services},
        loop(Running, Services);
    status ->
        note("Active components: ~tp", [Running]),
        loop(Running, Services);
    code_change ->
        ?MODULE:code_change(Running, Services);
    shutdown ->
        note("Shutting down."),
        shutdown(Running),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(Running, Services)
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
code_change(Running, Services) ->
    note("Changing code."),
    Pids = live_pids(Running),
    em_lib:broadcast(Pids, code_change),
    loop(Running, Services).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

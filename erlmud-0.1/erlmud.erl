-module(erlmud).
-export([start/0]).

start() ->
    true = register(erlmud, spawn(fun() -> init() end)).

init() ->
    process_flag(trap_exit, true),
    io:format("~p erlmud: Starting up.~n", [self()]),
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

loop(Running, Services) ->
  receive
    {From, Ref, {info, running}} ->
        From ! {Ref, Running},
        loop(Running, Services);
    {From, Ref, {info, services}} ->
        From ! {Ref, Services},
        loop(Running, Services);
    status ->
        io:format("~p erlmud: Active components: ~tp~n", [self(), Running]),
        loop(Running, Services);
    shutdown ->
        io:format("~p erlmud: Shutting down.~n", [self()]),
        shutdown(Running),
        exit(shutdown);
    Any ->
        io:format("~p erlmud: Received ~tp~n", [self(), Any]),
        loop(Running, Services)
  end.

shutdown(Running) ->
    io:format("~p erlmud: Shutting down subordinates...~n", [self()]),
    [Pid ! shutdown || {Pid, _} <- Running],
    ok.

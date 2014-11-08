-module(netman).
-export([start/1]).

start(Services) ->
    true = register(netman, spawn(fun() -> init(Services) end)),
    ok.

init(Services) ->
    process_flag(trap_exit, true),
    {ok, Running} = init([], Services),
    loop(Running, Services).

init(A, []) -> {ok, A};
init(A, [{Module, Func, Args} | Rest])  ->
    io:format("~p netman: Starting ~p~n", [self(), Module]),
    {ok, Pid} = apply(Module, Func, Args),
    init([{Pid, Module} | A], Rest).

loop(Running, Services) ->
  receive
    status ->
        io:format("~p netman: Active services: ~tp~n", [self(), Running]),
        loop(Running, Services);
    {'EXIT', Pid, Reason} ->
        Service = proplists:get_value(Pid, Running),
        io:format("~p netman: ~tp died with ~tp~n", [self(), Service, Reason]),
        init(Services);
    shutdown ->
        shutdown(Running),
        io:format("~p netman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p netman: Received ~tp~n", [self(), Any]),
        loop(Running, Services)
  end.

shutdown(Running) ->
    io:format("~p netman: Shutting down subordinates...~n", [self()]),
    [Pid ! shutdown || {Pid, _} <- Running],
    ok.
        

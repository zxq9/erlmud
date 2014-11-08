-module(netman).
-export([start/1, start/2, start_link/1, start_link/2]).

start(Parent) -> start(Parent, []).

start(Parent, Services) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = spawn(fun() -> init(Parent, Services) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid -> 
            {ok, Pid}
    end.

start_link(Parent) -> start_link(Parent, []).

start_link(Parent, Services) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = spawn_link(fun() -> init(Parent, Services) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init(Parent, Services) ->
    process_flag(trap_exit, true),
    io:format("~p netman: Starting services ~tp~n", [self(), Services]),
    {ok, Running} = init_services(Services, []),
    loop(Parent, Running, Services).

init_services([], A) -> {ok, A};
init_services([{Module, Func, Args} | Rest], A)  ->
    io:format("~p netman: Starting ~p~n", [self(), Module]),
    {ok, Pid} = apply(Module, Func, [self() | Args]),
    init_services(Rest, [{Pid, Module} | A]).

loop(Parent, Running, Services) ->
  receive
    status ->
        io:format("~p netman: Active services: ~tp~n", [self(), Running]),
        loop(Parent, Running, Services);
    {'EXIT', Parent, Reason} ->
        io:format("~p netman: Parent ~tp died with ~tp~n", [self(), Parent, Reason]),
        io:format("~p netman: Following my leige!~nBlarg!~n", [self()]),
        shutdown(Running);
    {'EXIT', Pid, Reason} ->
        Service = proplists:get_value(Pid, Running),
        io:format("~p netman: ~tp died with ~tp~n", [self(), Service, Reason]),
        init(Parent, Services);
    shutdown ->
        Parent ! {netman, shutdown},
        shutdown(Running),
        io:format("~p netman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p netman: Received ~tp~n", [self(), Any]),
        loop(Parent, Running, Services)
  end.

shutdown(Running) ->
    io:format("~p netman: Shutting down subordinates...~n", [self()]),
    [Pid ! shutdown || {Pid, _} <- Running],
    ok.

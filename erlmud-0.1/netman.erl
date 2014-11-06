-module(netman).
-export([start/1]).

start(Services) ->
    true = register(netman, spawn(fun() -> init(Services) end)),
    ok.

init(Services) ->
    {ok, State} = init([], Services),
    loop(State).

init(A, []) -> {ok, A};
init(A, [{Module, Func, Args} | Rest])  ->
    io:format("~p netman: Starting ~p~n", [self(), Module]),
    true = apply(Module, Func, Args),
    init([Module | A], Rest).

loop(State) ->
  receive
    shutdown ->
        shutdown(State),
        io:format("~p netman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p netman: Received ~tp~n", [self(), Any]),
        loop(State)
  end.

shutdown(Services) ->
    io:format("~p netman: Shutting down subordinates...~n", [self()]),
    [S ! shutdown || S <- Services],
    ok.
        

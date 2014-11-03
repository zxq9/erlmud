-module(netman).
-export([start/1]).

start(Services) ->
    true = register(netman, spawn(fun() -> init(Services) end)),
    ok.

init(Services) ->
    [io:format("~p netman: Notional init of ~p~n", [self(), S]) || S <- Services],
    loop().

loop() ->
  receive
    shutdown ->
        io:format("~p netman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p netman: Received ~tp~n", [self(), Any]),
        loop()
  end.

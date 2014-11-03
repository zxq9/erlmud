-module(mobman).
-export([start/0]).

start() ->
    true = register(mobman, spawn(fun() -> init() end)),
    ok.

init() ->
    io:format("~p mobman: Notional initialization.~n", [self()]),
    loop().

loop() ->
  receive
    shutdown ->
        io:format("~p mobman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p mobman: Received ~tp~n", [self(), Any]),
        loop()
  end.


-module(accman).
-export([start/0]).

start() ->
    true = register(accman, spawn(fun() -> init() end)),
    ok.

init() ->
    io:format("~p accman: Notional initialization.~n", [self()]),
    loop().

loop() ->
  receive
    shutdown ->
        io:format("~p accman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p accman: Received ~tp~n", [self(), Any]),
        loop()
  end.


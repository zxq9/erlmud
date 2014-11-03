-module(chanman).
-export([start/0]).

start() ->
    true = register(chanman, spawn(fun() -> init() end)),
    ok.

init() ->
    io:format("~p chanman: Notional initialization.~n", [self()]),
    loop().

loop() ->
  receive
    shutdown ->
        io:format("~p chanman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p chanman: Received ~tp~n", [self(), Any]),
        loop()
  end.

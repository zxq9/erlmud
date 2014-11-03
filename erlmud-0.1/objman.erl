-module(objman).
-export([start/0]).

start() ->
    true = register(objman, spawn(fun() -> init() end)),
    ok.

init() ->
    io:format("~p objman: Notional initialization.~n", [self()]),
    loop().

loop() ->
  receive
    shutdown ->
        io:format("~p objman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p objman: Received ~tp~n", [self(), Any]),
        loop()
  end.

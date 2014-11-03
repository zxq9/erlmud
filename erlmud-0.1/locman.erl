-module(locman).
-export([start/0]).

start() ->
    true = register(locman, spawn(fun() -> init() end)),
    ok.

init() ->
    io:format("~p locman: Notional initialization.~n", [self()]),
    loop().

loop() ->
  receive
    shutdown ->
        io:format("~p locman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p locman: Received ~tp~n", [self(), Any]),
        loop()
  end.

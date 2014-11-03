-module(wayman).
-export([start/0]).

start() ->
    true = register(wayman, spawn(fun() -> init() end)),
    ok.

init() ->
    io:format("~p wayman: Notional initialization.~n", [self()]),
    loop().

loop() ->
  receive
    shutdown ->
        io:format("~p wayman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p wayman: Received ~tp~n", [self(), Any]),
        loop()
  end.

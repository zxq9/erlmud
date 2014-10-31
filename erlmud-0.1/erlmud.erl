-module(erlmud).
-export([start/0]).

start() ->
    register(erlmud, spawn(fun() -> init() end)).

init() ->
    io:format("~p erlmud: Starting up.", [self()]),
    loop().

loop() ->
  receive
    shutdown ->
        io:format("~p erlmud: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p erlmud: Received~n~n~tp~n~n", [self(), Any]),
        loop()
  end.

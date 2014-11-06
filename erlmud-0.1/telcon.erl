-module(telcon).
-export([start_link/1]).

start_link(Telcon) ->
    spawn_link(fun() -> loop(Telcon) end).

loop(Telcon) ->
  receive
    shutdown ->
        io:format("~p telcon: Shutting down.~n", [self()]);
    Any ->
        io:format("~p telcon: Received ~tp~n", [self(), Any]),
        loop(Telcon)
  end.


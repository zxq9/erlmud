-module(con_user).
-export([start_link/0]).

start_link() ->
    spawn_link(fun() -> loop() end).

loop() ->
  receive
    shutdown ->
        io:format("~p controller: Shutting down.~n", [self()]);
    Any ->
        io:format("~p controller: Received ~tp~n", [self(), Any])
  end.


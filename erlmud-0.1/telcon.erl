-module(telcon).
-export([start_link/1]).

start_link(Talker) ->
    spawn_link(fun() -> loop(Talker) end).

loop(Talker) ->
  receive
    {received, String} ->
        io:format("~p telcon: Received ~tp~n", [self(), String]),
        Reply = "Got: " ++ String,
        Talker ! {send, Reply},
        loop(Talker);
    shutdown ->
        io:format("~p telcon: Shutting down.~n", [self()]);
    Any ->
        io:format("~p telcon: Received ~tp~n", [self(), Any]),
        loop(Talker)
  end.


-module(telcon).
-export([start_link/1]).

start_link(Talker) ->
    spawn_link(fun() -> loop(Talker) end).

loop(Talker) ->
  receive
    {received, Bin} ->
        Message = binary_to_list(binary:replace(Bin, <<"\r\n">>, <<>>)),
        Reply = handle(Message) ++ "\r\n" ++ prompt(),
        Talker ! {send, Reply},
        loop(Talker);
    shutdown ->
        io:format("~p telcon: Shutting down.~n", [self()]);
    Any ->
        io:format("~p telcon: Received ~tp~n", [self(), Any]),
        loop(Talker)
  end.

handle(Message) -> "Got: " ++ Message.

prompt() -> "(Some Prompt)$ ".

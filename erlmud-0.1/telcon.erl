-module(telcon).
-export([start_link/1]).

start_link(Talker) ->
    spawn_link(fun() -> welcome(Talker) end).

welcome(Talker) ->
    Greeting = greet(),
    Talker ! {send, Greeting},
    Handle = receive {received, Bin} -> stringify(Bin) end,
    Salutation = "Welcome " ++ Handle ++ "\r\n" ++ prompt(),
    Talker ! {send, Salutation},
    loop(Talker, Handle).

loop(Talker, Handle) ->
  receive
    {received, Bin} ->
        Message = stringify(Bin),
        Reply = handle(Message) ++ "\r\n" ++ prompt(),
        Talker ! {send, Reply},
        loop(Talker, Handle);
    shutdown ->
        io:format("~p telcon: Shutting down.~n", [self()]);
    Any ->
        io:format("~p telcon: Received ~tp~n", [self(), Any]),
        loop(Talker, Handle)
  end.

handle(Message) -> "Got: " ++ Message.

prompt() -> "(Some Prompt)$ ".

stringify(Bin) -> binary_to_list(binary:replace(Bin, <<"\r\n">>, <<>>)).

greet() ->
    "Welcome to ErlMUD\r\n\r\n"
    "By what name do you wish to be known?\r\n"
    "$ ".

-module(telnet).
-export([start/0, start/1]).

start() -> start(23).

start(Port) ->
    register(telnet, spawn(fun() -> init(Port) end)).

init(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    io:format("~p telnet: Starting up on port ~p.~n", [self(), Port]),
    loop(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->
        io:format("~p telnet: Received ~tp~n", [self(), Bin]),
        Str = binary_to_list(Bin),
        io:format("~p telnet: Unpacked ~tp~n", [self(), Str]),
        Reply = "You: " ++ Str,
        gen_tcp:send(Socket, Reply),
        loop(Socket);
    {send, Message} ->
        M = "#system: " ++ Message ++ "\r\n",
        gen_tcp:send(Socket, M),
        loop(Socket);
    {tcp_closed, Socket} ->
        io:format("~p telnet: Socket closed. Retiring.~n", [self()]),
        exit(tcp_closed);
    shutdown ->
        io:format("~p telnet: Shutting down hard.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p telnet: Received ~tp~n", [self(), Any]),
        loop(Socket)
  end.

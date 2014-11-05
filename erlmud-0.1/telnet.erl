-module(telnet).
-export([start/0, start/1]).

%% Telnet service
start() -> start(23).

start(Port) ->
    register(telnet, spawn(fun() -> init(Port) end)).

init(Port) ->
    io:format("~p telnet: Starting up on port ~p.~n", [self(), Port]),
    start_listening(Port),
    Connections = orddict:new(),
    loop(Port, Connections).

loop(Port, Connections) ->
  receive
    start_listening ->
        start_listening(Port),
        loop(Port, Connections);
    stop_listening ->
        stop_listening(),
        loop(Port, Connections);
    {new_connection, Pid} ->
        Ref = monitor(process, Pid),
        loop(Port, orddict:store(Ref, Pid));
    {'DOWN', Ref, process, _Pid, _Reason} ->
        loop(Port, orddict:erase(Ref, Connections));
    shutdown ->
        stop_listening(),
        [Pid ! shutdown || {_, Pid} <- orddict:to_list(Connections)],
        exit(shutdown)
  end.

%% Listener
start_listening(Port) ->
    case whereis(telnet_listener) of
        undefined -> register(telnet_listener, spawn(fun() -> init_listener(Port) end));
        _Pid      -> true
    end.

stop_listening() ->
    case whereis(telnet_listener) of
        undefined -> ok;
        Pid       -> Pid ! shutdown
    end,
    ok.

init_listener(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}]),
    listen(Socket).

listen(Socket) ->
    {ok, Conn} = gen_tcp:accept(Socket),
    Talker = spawn(fun() -> talk(Conn) end),
    gen_tcp:controlling_process(Conn, Talker),
    listen(Socket).

%% Talkers
talk(Socket) ->
    inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, Bin} ->
        io:format("~p telnet: Received ~tp~n", [self(), Bin]),
        Str = binary_to_list(Bin),
        io:format("~p telnet: Unpacked ~tp~n", [self(), Str]),
        Reply = "You: " ++ Str,
        gen_tcp:send(Socket, Reply),
        talk(Socket);
    {send, Message} ->
        M = "#system: " ++ Message ++ "\r\n",
        gen_tcp:send(Socket, M),
        talk(Socket);
    {tcp_closed, Socket} ->
        io:format("~p telnet: Socket closed. Retiring.~n", [self()]),
        exit(tcp_closed);
    shutdown ->
        io:format("~p telnet: Shutting down hard.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p telnet: Received ~tp~n", [self(), Any]),
        talk(Socket)
  end.

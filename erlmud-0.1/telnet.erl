-module(telnet).
-export([start/0, start/1]).

%% Telnet service
start() -> start(23).

start(Port) ->
    register(telnet, spawn(fun() -> init(Port) end)).

init(Port) ->
    io:format("~p telnet: Starting up on port ~p.~n", [self(), Port]),
    Socket = start_listening(Port),
    Connections = orddict:new(),
    accepting(Port, Socket, Connections).

accepting(Port, Socket, Connections) ->
  receive
    stop_listening ->
        gen_tcp:close(Socket),
        Listener = whereis(telnet_listener),
        unlink(Listener),
        exit(Listener, stop_listening),
        refusing(Port, Connections);
    {new_connection, Pid} ->
        Ref = monitor(process, Pid),
        accepting(Port, Socket, orddict:store(Ref, Pid));
    {'DOWN', Ref, process, _Pid, _Reason} ->
        accepting(Port, Socket, orddict:erase(Ref, Connections));
    shutdown ->
        gen_tcp:close(Socket),
        [Pid ! shutdown || {_, Pid} <- orddict:to_list(Connections)],
        exit(shutdown);
    Any ->
        io:format("~p telnet: received ~tp.~n", [self(), Any]),
        accepting(Port, Socket, Connections)
  end.

refusing(Port, Connections) ->
  receive
    start_listening ->
        Socket = start_listening(Port),
        accepting(Port, Socket, Connections);
    {'DOWN', Ref, process, _Pid, _Reason} ->
        refusing(Port, orddict:erase(Ref, Connections));
    shutdown ->
        [Pid ! shutdown || {_, Pid} <- orddict:to_list(Connections)],
        exit(shutdown);
    Any ->
        io:format("~p telnet: received ~tp.~n", [self(), Any]),
        refusing(Port, Connections)
  end.

start_listening(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {reuseaddr, true}, {active, false}]),
    true = register(telnet_listener, spawn_link(fun() -> listen(Socket) end)),
    Socket.

%% Listener
listen(Socket) ->
    io:format("~p telnet_listener: Listening on ~p~n", [self(), Socket]),
    {ok, Conn} = gen_tcp:accept(Socket),
    io:format("~p telnet_listener: Accepted on ~p~n", [self(), Conn]),
    Talker = spawn(fun() -> talk(Conn) end),
    gen_tcp:controlling_process(Conn, Talker),
    listen(Socket).

%% Connection handlers
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

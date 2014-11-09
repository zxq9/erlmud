-module(telnet).
-export([start/1, start/2, start_link/1, start_link/2]).

%% Telnet service
start(Parent) -> start(Parent, 23).

start(Parent, PortNum) ->
    starter(fun spawn/1, Parent, PortNum).

start_link(Parent) -> start_link(Parent, 23).

start_link(Parent, PortNum) ->
    starter(fun spawn_link/1, Parent, PortNum).

starter(Spawn, Parent, PortNum) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = Spawn(fun() -> init(Parent, PortNum) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init(Parent, PortNum) ->
    io:format("~p telnet: Starting up on port ~p.~n", [self(), PortNum]),
    process_flag(trap_exit, true),
    {Port, Listener} = start_listening(PortNum),
    Connections = orddict:new(),
    accepting(Parent, PortNum, Connections, Port, Listener).

accepting(Parent, PortNum, Connections, Port, Listener) ->
  receive
    {new_connection, Socket} ->
        io:format("~p telnet: Accepted on ~p~n", [self(), Port]),
        Talker = spawn_link(fun() -> start_talking(Socket) end),
        gen_tcp:controlling_process(Socket, Talker),
        UpdateConn = orddict:store(Talker, Socket, Connections),
        accepting(Parent, PortNum, UpdateConn, Port, Listener);
    {end_connection, Talker} ->
        io:format("~p telnet: Talker (~p) connection ended.~n", [self(), Talker]),
        UpdateConn = orddict:erase(Talker, Connections),
        accepting(Parent, PortNum, UpdateConn, Port, Listener);
    stop_listening ->
        gen_tcp:close(Port),
        refusing(Parent, PortNum, Connections);
    status ->
        io:format("~p telnet: accepting connections~n", [self()]),
        io:format("  PortNum: ~p~n  Connections: ~p~n  Port: ~p~n  Listener: ~p~n",
                  [PortNum, Connections, Port, Listener]),
        accepting(Parent, PortNum, Connections, Port, Listener);
    {'EXIT', Listener, Reason} ->
        io:format("~p telnet: Listener (~p) exited with ~p~n",
                  [self(), Listener, Reason]),
        gen_tcp:close(Port),
        {NewPort, NewListener} = start_listening(PortNum),
        accepting(Parent, PortNum, Connections, NewPort, NewListener);
    {'EXIT', Parent, Reason} ->
        io:format("~p telnet: Parent ~tp died with ~tp~n", [self(), Parent, Reason]),
        io:format("~p telnet: Following my leige!~nBlarg!~n", [self()]),
        close_all(Connections);
    {'EXIT', Pid, Reason} ->
        io:format("~p telnet: ~p exited with ~p~n", [self(), Pid, Reason]),
        accepting(Parent, PortNum, Connections, Port, Listener);
    shutdown ->
        io:format("~p telnet: Shutting down subordinates...~n", [self()]),
        gen_tcp:close(Port),
        close_all(Connections);
    Any ->
        io:format("~p telnet: received ~tp.~n", [self(), Any]),
        accepting(Parent, PortNum, Connections, Port, Listener)
  end.

refusing(Parent, PortNum, Connections) ->
  receive
    {end_connection, Talker} ->
        io:format("~p telnet: Talker (~p) connection ended.~n", [self(), Talker]),
        UpdateConn = orddict:erase(Talker, Connections),
        refusing(Parent, PortNum, UpdateConn);
    start_listening ->
        {Port, Listener} = start_listening(PortNum),
        accepting(Parent, PortNum, Connections, Port, Listener);
    status ->
        io:format("~p telnet: refusing connections~n", [self()]),
        io:format("  PortNum: ~p~n  Connections: ~p~n", [PortNum, Connections]),
        refusing(Parent, PortNum, Connections);
    {'EXIT', Parent, Reason} ->
        io:format("~p telnet: Parent ~tp died with ~tp~n", [self(), Parent, Reason]),
        io:format("~p telnet: Following my leige!~nBlarg!~n", [self()]),
        close_all(Connections);
    {'EXIT', Pid, Reason} ->
        io:format("~p telnet: ~p exited with ~p~n", [self(), Pid, Reason]),
        refusing(Parent, PortNum, Connections);
    shutdown ->
        io:format("~p telnet: Shutting down subordinates...~n", [self()]),
        close_all(Connections);
    Any ->
        io:format("~p telnet: received ~tp.~n", [self(), Any]),
        refusing(Parent, PortNum, Connections)
  end.

close_all(Connections) ->
    [Talker ! shutdown || {Talker, _} <- orddict:to_list(Connections)].

%% Listener
start_listening(PortNum) ->
    {ok, Port} = gen_tcp:listen(PortNum, [binary, {reuseaddr, true}, {active, false}]),
    Listener = spawn_link(fun() -> listen(Port) end),
    {Port, Listener}.

listen(Port) ->
    io:format("~p telnet: Listening on ~p~n", [self(), Port]),
    case gen_tcp:accept(Port) of
        {ok, Socket} ->
            gen_tcp:controlling_process(Socket, whereis(telnet)),
            telnet ! {new_connection, Socket},
            listen(Port);
        {error, closed} ->
            io:format("~p telnet: Listening port ~p closed~n", [self(), Port])
    end.

%% Connection handlers
start_talking(Socket) ->
    Telcon = telcon:start_link(self()),
    talk(Socket, Telcon).

talk(Socket, Telcon) ->
    inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, Bin} ->
        Telcon ! {received, Bin},
        talk(Socket, Telcon);
    {send, Message} ->
        gen_tcp:send(Socket, Message),
        talk(Socket, Telcon);
    {tcp_closed, Socket} ->
        io:format("~p telnet: Socket closed. Retiring.~n", [self()]),
        telnet ! {end_connection, self()};
    shutdown ->
        Telcon ! shutdown,
        io:format("~p telnet: Talker shutting down.~n", [self()]);
    Any ->
        io:format("~p telnet: Received ~tp~n", [self(), Any]),
        talk(Socket, Telcon)
  end.

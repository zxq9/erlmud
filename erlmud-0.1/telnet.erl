-module(telnet).
-export([start/1, start/2, start_link/1, start_link/2, code_change/3]).

%% Startup
start(Parent)               -> start(Parent, 23).
start(Parent, PortNum)      -> starter(fun spawn/1, Parent, PortNum).
start_link(Parent)          -> start_link(Parent, 23).
start_link(Parent, PortNum) -> starter(fun spawn_link/1, Parent, PortNum).

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
    note("Starting up on port ~p", [PortNum]),
    process_flag(trap_exit, true),
    {Port, Listener} = tcplistener:start(self(), PortNum),
    Connections = dict:new(),
    accepting(Parent, PortNum, Connections, Port, Listener).

%% Service states
accepting(Parent, PortNum, Connections, Port, Listener) ->
  receive
    {new_connection, Socket} ->
        note("Accepted on ~p", [Port]),
        Talker = teltalker:start_link(self(), Socket),
        gen_tcp:controlling_process(Socket, Talker),
        UpdateConn = dict:store(Talker, Socket, Connections),
        accepting(Parent, PortNum, UpdateConn, Port, Listener);
    {end_connection, Talker} ->
        note("Talker (~p) connection ended.", [Talker]),
        UpdateConn = dict:erase(Talker, Connections),
        accepting(Parent, PortNum, UpdateConn, Port, Listener);
    stop_listening ->
        gen_tcp:close(Port),
        refusing(Parent, PortNum, Connections);
    status ->
        note("Accepting connections.~n"
             "  PortNum: ~p~n  Connections: ~p~n  Port: ~p~n  Listener: ~p",
             [PortNum, dict:to_list(Connections), Port, Listener]),
        accepting(Parent, PortNum, Connections, Port, Listener);
    {'EXIT', Listener, Reason} ->
        note("Listener (~p) exited with ~p", [Listener, Reason]),
        gen_tcp:close(Port),
        {NewPort, NewListener} = tcplistener:start(self(), PortNum),
        accepting(Parent, PortNum, Connections, NewPort, NewListener);
    {'EXIT', Parent, Reason} ->
        note("Parent~tp died with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]),
        close_all(Connections);
    Message = {'EXIT', _, _} ->
        NewConnections = handle_exit(Message, Connections),
        accepting(Parent, PortNum, NewConnections, Port, Listener);
    {monitor, Pid} ->
        monitor(process, Pid),
        accepting(Parent, PortNum, Connections, Port, Listener);
    code_change ->
        ?MODULE:code_change(accepting,
                            {Parent, PortNum, Connections, Port},
                            Connections);
    shutdown ->
        note("Shutting down subordinates..."),
        gen_tcp:close(Port),
        close_all(Connections);
    Any ->
        note("Received ~tp", [Any]),
        accepting(Parent, PortNum, Connections, Port, Listener)
  end.

refusing(Parent, PortNum, Connections) ->
  receive
    {end_connection, Talker} ->
        note("Talker (~p) connection ended.", [Talker]),
        UpdateConn = dict:erase(Talker, Connections),
        refusing(Parent, PortNum, UpdateConn);
    start_listening ->
        {Port, Listener} = tcplistener:start(self(), PortNum),
        accepting(Parent, PortNum, Connections, Port, Listener);
    status ->
        note("Refusing connections.~n  PortNum: ~p~n  Connections: ~p~n",
             [PortNum, dict:to_list(Connections)]),
        refusing(Parent, PortNum, Connections);
    {'EXIT', Parent, Reason} ->
        note("Parent~tp died with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]),
        close_all(Connections);
    Message = {'EXIT', _, _} ->
        NewConnections = handle_exit(Message, Connections),
        refusing(Parent, PortNum, NewConnections);
    code_change ->
        ?MODULE:code_change(refusing,
                            {Parent, PortNum, Connections},
                            Connections);
    shutdown ->
        note("Shutting down subordinates..."),
        close_all(Connections);
    Any ->
        io:format("~p telnet: received ~tp.~n", [self(), Any]),
        refusing(Parent, PortNum, Connections)
  end.

%% Magic
close_all(Connections) ->
    Pids = live_pids(Connections),
    em_lib:broadcast(Pids, shutdown),
    ok.

handle_exit({_, Pid, Reason}, Connections) ->
    case dict:is_key(Pid, Connections) of
        true  ->
            note("Talker (~p) connection ended.", [Pid]),
            dict:erase(Pid, Connections);
        false ->
            note("~p exited with ~p", [Pid, Reason]),
            Connections
    end.

live_pids(Connections) ->
    [Pid || {Pid, _} <- dict:to_list(Connections)].

%% Code changer
code_change(Continue, Args, Connections) ->
    note("Changing code."),
    Pids = live_pids(Connections),
    em_lib:broadcast(Pids, code_change),
    do_change(Continue, Args).

do_change(accepting, {Parent, PortNum, Connections, Port}) ->
    gen_tcp:close(Port),
    timer:sleep(1000),  % Give the OS time to react
    {NewPort, Listener} = tcplistener:start(self(), PortNum),
    accepting(Parent, PortNum, Connections, NewPort, Listener);
do_change(refusing, {Parent, PortNum, Connections}) ->
    refusing(Parent, PortNum, Connections).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

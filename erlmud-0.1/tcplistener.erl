-module(tcplistener).
-export([start/2]).

%% Startup
start(Server, PortNum) ->
    {ok, Port} = gen_tcp:listen(PortNum, [binary, {reuseaddr, true}, {active, false}]),
    Listener = spawn_link(fun() -> listen(Server, Port) end),
    {Port, Listener}.

%% Service
listen(Server, Port) ->
    note("Listening on ~p", [Port]),
    case gen_tcp:accept(Port) of
        {ok, Socket} ->
            gen_tcp:controlling_process(Socket, Server),
            Server ! {new_connection, Socket},
            listen(Server, Port);
        {error, closed} ->
            note("~p closed.", [Port])
    end.

%% System
note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

-module(teltalker).
-export([start_link/2, code_change/3]).

%% Startup
start_link(Server, Socket) ->
    spawn_link(fun() -> init(Server, Socket) end).

init(Server, Socket) ->
    Telcon = telcon:start_link(self()),
    talk(Server, Socket, Telcon).

%% Service
talk(Server, Socket, Telcon) ->
    inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, Bin} ->
        Telcon ! {received, Bin},
        talk(Server, Socket, Telcon);
    {send, Message} ->
        gen_tcp:send(Socket, Message),
        talk(Server, Socket, Telcon);
    {tcp_closed, Socket} ->
        note("Socket closed. Retiring.");
    code_change ->
        ?MODULE:code_change(Server, Socket, Telcon);
    shutdown ->
        Telcon ! shutdown,
        note("Talker shutting down.");
    Any ->
        note("Received ~tp", [Any]),
        talk(Server, Socket, Telcon)
  end.

%% Code changer
code_change(Server, Socket, Telcon) ->
    note("Changing code."),
    Telcon ! code_change,
    talk(Server, Socket, Telcon).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

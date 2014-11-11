-module(wayman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/2]).

%% Startup
start(Parent)            -> start(Parent, []).
start(Parent, Conf)      -> starter(fun spawn/1, Parent, Conf).
start_link(Parent)       -> start_link(Parent, []).
start_link(Parent, Conf) -> starter(fun spawn_link/1, Parent, Conf).

starter(Spawn, Parent, Conf) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = Spawn(fun() -> init(Parent, Conf) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init(Parent, Conf) ->
    note("Notional initialization with ~p.", [Conf]),
    loop(Parent, Conf).

%% Service
loop(Parent, Conf) ->
  receive
    code_change ->
        ?MODULE:code_change(Parent, Conf);
    shutdown ->
        note("Shutting down."),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(Parent, Conf)
  end.

%% Code changer
code_change(Parent, Conf) ->
    note("Changing code."),
    loop(Parent, Conf).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

-module(conman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/2]).

%% Startup
start(Parent)            -> start(Parent, none).
start(Parent, Conf)      -> starter(fun spawn/1, Parent, Conf).
start_link(Parent)       -> start_link(Parent, none).
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
    note("Notional initialization with ~tp.", [Conf]),
    Controllers = case Conf of
        none -> orddict:new();
        _    -> init_registry(Conf)
    end,
    loop(Parent, Controllers).

init_registry(Conf) -> orddict:from_list(Conf).

%% Service
loop(Parent, Controllers) ->
  receive
    {From, Ref, {lookup, Handle}} ->
        Pid = lookup(Handle, Controllers),
        From ! {Ref, Pid},
        loop(Parent, Controllers);
    code_change ->
        ?MODULE:code_change(Parent, Controllers);
    shutdown ->
        note("Shutting down."),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(Parent, Controllers)
  end.

%% Magic
lookup(Name, Controllers) ->
    case orddict:find(Name, Controllers) of
        error  -> {error, absent};
        ConPid -> {ok, ConPid}
    end.


%% Code changer
code_change(Parent, Controllers) ->
    note("Changing code."),
    loop(Parent, Controllers).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

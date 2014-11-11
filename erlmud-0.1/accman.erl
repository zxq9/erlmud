-module(accman).
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
    note("Notional initialization with ~p", [Conf]),
    Registry = case Conf of
        [] -> orddict:new();
        _  -> init_registry(Conf)
    end,
    loop(Parent, Registry).

init_registry(_) -> orddict:new().

%% Service
loop(Parent, Registry) ->
  receive
    {From, Ref, {check, Handle}} ->
        Status = check(Handle, Registry),
        From ! {Ref, Status},
        loop(Parent, Registry);
    {From, Ref, {verify, Acc}} ->
        Response = verify(Acc, Registry),
        From ! {Ref, Response},
        loop(Parent, Registry);
    {From, Ref, {create, Acc}} ->
        case create(Acc, Registry) of
            Error = {error, _} ->
                From ! {Ref, Error},
                loop(Parent, Registry);
            Updated ->
                From ! {Ref, ok},
                loop(Parent, Updated)
        end;
    status ->
        note("Registry ~p", [Registry]),
        loop(Parent, Registry);
    code_change ->
        ?MODULE:code_change(Parent, Registry);
    shutdown ->
        note("Shutting down"),
        exit(shutdown);
    Any ->
        note("Received ~p", [Any]),
        loop(Parent, Registry)
  end.

%% Magic
check(Handle, Registry) ->
    case orddict:is_key(Handle, Registry) of
        true  -> registered;
        false -> unregistered
    end.

create({Handle, PW}, Registry) ->
    case check(Handle, Registry) of
        unregistered -> orddict:store(Handle, PW, Registry);
        registered   -> {error, handle_in_use}
    end.

verify({Handle, PW}, Registry) ->
    case check(Handle, Registry) of
        registered   ->
            case string:equal(orddict:fetch(Handle, Registry), PW) of
                true  -> verified;
                false -> badpass
            end;
        unregistered -> unregistered
    end.

%% Code changer
code_change(Parent, Registry) ->
    note("Changing code."),
    loop(Parent, Registry).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

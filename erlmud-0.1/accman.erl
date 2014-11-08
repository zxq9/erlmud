-module(accman).
-export([start/1, start/2, start_link/1, start_link/2]).

start(Parent) -> start(Parent, []).

start(Parent, Conf) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = spawn(fun() -> init(Parent, Conf) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid -> 
            {ok, Pid}
    end.

start_link(Parent) -> start_link(Parent, []).

start_link(Parent, Conf) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = spawn_link(fun() -> init(Parent, Conf) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init(Parent, Conf) ->
    io:format("~p accman: Notional initialization with ~p.~n", [self(), Conf]),
    Registry = case Conf of
        [] -> orddict:new();
        _  -> init_registry(Conf)
    end,
    loop(Parent, Registry).

init_registry(_) -> orddict:new().

loop(Parent, Registry) ->
  receive
    {From, Ref, {lookup, Handle}} ->
        Status = lookup(Handle, Registry),
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
        io:format("~p accman: Registry ~p~n", [self(), Registry]),
        loop(Parent, Registry);
    shutdown ->
        io:format("~p accman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p accman: Received ~tp~n", [self(), Any]),
        loop(Parent, Registry)
  end.

% Magic
lookup(Handle, Registry) ->
    case orddict:is_key(Handle, Registry) of
        true  -> registered;
        false -> unregistered
    end.

create({Handle, PW}, Registry) ->
    case lookup(Handle, Registry) of
        unregistered -> orddict:store(Handle, PW, Registry);
        registered   -> {error, handle_in_use}
    end.

verify({Handle, PW}, Registry) ->
    case lookup(Handle, Registry) of
        registered   ->
            case string:equal(orddict:fetch(Handle, Registry), PW) of
                true  -> verified;
                false -> badpass
            end;
        unregistered -> unregistered
    end.

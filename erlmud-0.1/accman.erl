-module(accman).
-export([start/0]).

start() ->
    true = register(accman, spawn(fun() -> init() end)),
    ok.

init() ->
    io:format("~p accman: Notional initialization.~n", [self()]),
    Registry = orddict:new(),
    loop(Registry).

loop(Registry) ->
  receive
    {From, Ref, {lookup, Handle}} ->
        Status = lookup(Handle, Registry),
        From ! {Ref, Status},
        loop(Registry);
    {From, Ref, {verify, Acc}} ->
        Response = verify(Acc, Registry),
        From ! {Ref, Response},
        loop(Registry);
    {From, Ref, {create, Acc}} ->
        case create(Acc, Registry) of
            Error = {error, _} ->
                From ! {Ref, Error},
                loop(Registry);
            Updated ->
                From ! {Ref, ok},
                loop(Updated)
        end;
    status ->
        io:format("~p accman: Registry ~p~n", [self(), Registry]),
        loop(Registry);
    shutdown ->
        io:format("~p accman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p accman: Received ~tp~n", [self(), Any]),
        loop(Registry)
  end.

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

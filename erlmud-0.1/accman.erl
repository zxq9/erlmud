-module(accman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/2,
         salthash/1, salthash/2, checkhash/2, check/1, verify/2, create/2]).

%% Interface

salthash(Bin) ->
    Salt = salt(),
    Hash = hash(<<Salt/binary, Bin/binary>>),
    {Salt, Hash}.

salthash(Salt, Bin) ->
    Hash = hash(<<Salt/binary, Bin/binary>>),
    {Salt, Hash}.

checkhash({Salt, Hash}, Bin) ->
    {_, Test} = salthash(Salt, Bin),
    Test =:= Hash.

check(Handle) -> call({check, Handle}).

verify(Handle, PW) ->
    case call({get_hash, Handle}) of
        {ok, PassHash} ->
            case checkhash(PassHash, PW) of
                true  -> verified;
                false -> badpass
            end;
        Error ->
            Error
    end.

create(Handle, PassHash) -> call({create, {Handle, PassHash}}).

call(Request) -> em_lib:call(?MODULE, Request).

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
        [] -> dict:new();
        _  -> init_registry(Conf)
    end,
    loop(Parent, Registry).

init_registry(_) -> dict:new().

%% Service
loop(Parent, Registry) ->
  receive
    {From, Ref, {check, Handle}} ->
        Status = do_check(Handle, Registry),
        From ! {Ref, Status},
        loop(Parent, Registry);
    {From, Ref, {get_hash, Handle}} ->
        Response = get_hash(Handle, Registry),
        From ! {Ref, Response},
        loop(Parent, Registry);
    {From, Ref, {create, Acc}} ->
        case do_create(Acc, Registry) of
            {ok, NewRegistry} ->
                From ! {Ref, ok},
                loop(Parent, NewRegistry);
            Error ->
                From ! {Ref, Error},
                loop(Parent, Registry)
        end;
    status ->
        List = dict:to_list(Registry),
        note("Registry ~p", [List]),
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
do_check(Handle, Registry) ->
    case dict:is_key(Handle, Registry) of
        true  -> registered;
        false -> unregistered
    end.

do_create({Handle, PassHash}, Registry) ->
    case do_check(Handle, Registry) of
        unregistered -> {ok, dict:store(Handle, PassHash, Registry)};
        registered   -> {error, handle_in_use}
    end.

get_hash(Handle, Registry) ->
    case dict:find(Handle, Registry) of
        {ok, PassHash} -> {ok, PassHash};
        error          -> {fail, unregistered}
    end.

salt() -> crypto:rand_bytes(64).

hash(Bin) -> crypto:hash(sha512, Bin).

%% Code changer
code_change(Parent, Registry) ->
    note("Changing code."),
    loop(Parent, Registry).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

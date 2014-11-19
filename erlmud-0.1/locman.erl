-module(locman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/1,
         get_pid/1]).

%% Interface
get_pid(LocID) -> call({get_pid, LocID}).

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
    process_flag(trap_exit, true),
    note("Initializing with ~p", [Conf]),
    Live = genesis(Conf),
    loop({Parent, Live, Conf}).

genesis(Conf) -> [{Id, loc:start_link(Z)} || Z = {Id, _} <- Conf].

%% Service
loop(State = {Parent, Live, Conf}) ->
  receive
    {From, Ref, {get_pid, LocID}} ->
        LocPid = get_pid(LocID, Live),
        From ! {Ref, LocPid},
        loop(State);
    {'EXIT', Parent, Reason} ->
        note("Parent~tp died with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]);
    Message = {'EXIT', _, _} ->
        NewLive = handle_exit(Message, Live, Conf),
        loop({Parent, NewLive, Conf});
    status ->
        note("Status:~n  Live: ~p~n  Conf: ~p", [Live, Conf]),
        loop(State);
    code_change ->
        ?MODULE:code_change(State);
    shutdown ->
        note("Shutting down."),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(State)
  end.

%% Request handlers
get_pid(LocID, Live) ->
    case lists:keyfind(LocID, 1, Live) of
        {_, Pid} -> {ok, Pid};
        false    -> {error, noloc}
    end.

%% Magic
handle_exit(Message = {_, Pid, _}, Live, Conf) ->
    case lists:keyfind(Pid, 2, Live) of
        Loc = {LocID, _} ->
            note("~p exited with ~p", [LocID, Message]),
            ScrubLive = lists:delete(Loc, Live),
            case lists:keyfind(LocID, 1, Conf) of
                LocConf = {_, _} -> [{LocID, loc:start_link(LocConf)} | ScrubLive];
                false            -> ScrubLive
            end;
        false ->
            note("Received ~p", [Message]),
            Live
    end.

%% Code changer
code_change(State = {_, Live, _}) ->
    note("Changing code."),
    [LocPid ! code_change || {_, LocPid} <- Live],
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

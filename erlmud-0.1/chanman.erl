-module(chanman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/2,
         list/0, get_pid/1, acquire/1]).

%% Interface

list() ->
    call(list).

get_pid(Channel) ->
    call({get_pid, Channel}).

acquire(Channel) ->
    call({acquire, Channel}).

call(Request) ->
    em_lib:call(?MODULE, Request).

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
    process_flag(trap_exit, true),
    note("Notional initialization with ~tp.", [Conf]),
    Channels = [],
    loop(Parent, Channels).

%% Service
loop(Parent, Channels) ->
  receive
    {From, Ref, {acquire, Channel}} ->
        {ChanPid, NewChannels} = acquire(Channel, Channels),
        From ! {Ref, ChanPid},
        loop(Parent, NewChannels);
    {From, Ref, list} ->
        List = [Name || {Name, _} <- Channels],
        From ! {Ref, List},
        loop(Parent, Channels);
    {From, Ref, {get_pid, Channel}} ->
        Result = lookup(Channel, Channels),
        From ! {Ref, Result},
        loop(Parent, Channels);
    {'EXIT', Parent, Reason} ->
        note("Parent~tp died with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]),
        exit(parent_died);
    Message = {'EXIT', _, _} ->
        NewChannels = handle_exit(Channels, Message),
        loop(Parent, NewChannels);
    status ->
        note("Channels ~p", [Channels]),
        loop(Parent, Channels);
    code_change ->
        ?MODULE:code_change(Parent, Channels);
    shutdown ->
        note("Shutting down."),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(Parent, Channels)
  end.

%% Magic
lookup(Channel, Channels) ->
    case lists:keyfind(Channel, 1, Channels) of
        {_, Pid} -> {ok, Pid};
        false    -> {error, unregistered}
    end.

acquire(Channel, Channels) ->
    case lists:keyfind(Channel, 1, Channels) of
        {_, Pid} ->
            {Pid, Channels};
        false ->
            Conf = {Channel, [], []},
            Pid = channel:start_link(self(), Conf),
            NewChannels = [{Channel, Pid} | Channels],
            {Pid, NewChannels}
    end.

handle_exit(Channels, Message = {_, Pid, _}) ->
    case lists:keyfind(Pid, 2, Channels) of
        Channel = {_, _} ->
            note("~p sent 'DOWN'", [Channel]),
            lists:delete(Channel, Channels);
        false ->
            note("Received ~p", [Message]),
            Channels
    end.

%% Code changer
code_change(Parent, Channels) ->
    note("Changing code."),
    loop(Parent, Channels).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

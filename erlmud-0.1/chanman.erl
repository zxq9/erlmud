-module(chanman).
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
        List = [Name || {Name, _, _} <- Channels],
        From ! {Ref, List},
        loop(Parent, Channels);
    Message = {'DOWN', _, process, _, _} ->
        NewChannels = handle_down(Channels, Message),
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
acquire(Channel, Channels) ->
    case lists:keyfind(Channel, 1, Channels) of
        {_, Pid, _} ->
            {Pid, Channels};
        false ->
            Conf = {Channel, [], []},
            {Pid, Ref} = channel:start_monitor(self(), Conf),
            NewChannels = [{Channel, Pid, Ref} | Channels],
            {Pid, NewChannels}
    end.

handle_down(Channels, Message = {_, Ref, _, _, _}) ->
    case lists:keyfind(Ref, 3, Channels) of
        false ->
            note("Received ~p", [Message]),
            Channels;
        Channel ->
            note("~p sent 'DOWN'", [Channel]),
            lists:delete(Channel, Channels)
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

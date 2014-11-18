-module(wayman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/1,
         get_pid/1, get_entrances/1, get_exits/1, reg_live/1]).

%% Interface
get_pid(WayID) -> call({pid, WayID}).

get_entrances(LocID) -> call({entrances, LocID}).

get_exits(LocID) -> call({exits, LocID}).

reg_live(Way) -> ?MODULE ! {register, {live, Way}}.

call(Request) -> em_lib:call(?MODULE, Request).

%reg_new(WayID) -> ?MODULE ! {register, {new, WayID}}.

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

init(Parent, Ways) ->
    note("Initializing with ~p.", [Ways]),
    Live = [],
    loop({Parent, Live, Ways}).

%% Service
loop(State = {Parent, Live, Ways}) ->
  receive
    {From, Ref, {pid, WayID}} ->
        Response = pid(Live, WayID),
        From ! {Ref, Response},
        loop(State);
    {From, Ref, {entrances, LocID}} ->
        In = ins(Ways, LocID),
        From ! {Ref, In},
        loop(State);
    {From, Ref, {exits, LocID}} ->
        Out = outs(Ways, LocID),
        From ! {Ref, Out},
        loop(State);
    {register, {live, Way}} ->
        NewLive = reg(Live, Way),
        loop({Parent, NewLive, Ways});
    Message = {'DOWN', _, process, _, _} ->
        NewLive = handle_down(Live, Message),
        loop({Parent, NewLive, Ways});
    status ->
        note("Status:~n  Live: ~p~n  Ways: ~p", [Live, Ways]),
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

%% Way registry
reg(Live, Way = {WayID, WayPid}) ->
    case lists:keyfind(WayID, 1, Live) of
        false ->
            RegWay = {WayID, WayPid, monitor(process, WayPid)},
            [RegWay | Live];
        RegWay ->
            note("Funny business:~n  Tried to register: ~p~n  Already registered: ~p",
                 [Way, RegWay]),
            Live
    end.

pid(Live, WayID) ->
    case lists:keyfind(WayID, 1, Live) of
        {_, Pid, _} -> {ok, Pid};
        false       -> {error, unregistered}
    end.

ins(Ways, LocID) ->
    Find = fun(WayID) -> way:out(WayID) == LocID end,
    [Z || Z <- lists:filter(Find, Ways)].

outs(Ways, LocID) ->
    Find = fun(WayID) -> way:in(WayID) == LocID end,
    [Z || Z <- lists:filter(Find, Ways)].

handle_down(Live, Message = {_, Ref, _, _, _}) ->
    case lists:keyfind(Ref, 3, Live) of
        Way = {_, _, _} ->
            lists:delete(Way, Live);
        false ->
            note("Received ~p", [Message]),
            Live
    end.

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

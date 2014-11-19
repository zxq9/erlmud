-module(mob).
-export([start_link/2, code_change/1,
         get_actions/1]).

%% Interface

get_actions(MobPid) -> em_lib:call(MobPid, actions).

%% Startup
start_link(Con, Conf) ->
    spawn_link(fun() -> init(Con, Conf) end).

init(ConPid, Conf = {Name, {Ilk, Desc, LastLocID}}) ->
    note("Initializing with ~p", [Conf]),
    ConRef = monitor(process, ConPid),
    Me = {Name, self(), Ilk},
    locless(Me, {{ConPid, ConRef}, Name, Ilk, Desc}, LastLocID).

locless(Me, {Con, Name, Ilk, Desc}, none) ->
    Loc = mobman:relocate(Me, default),
    enter_world({Con, Name, Ilk, Desc, Loc});
locless(Me, {Con, Name, Ilk, Desc}, LastLocID) ->
    Loc = mobman:relocate(Me, LastLocID),
    enter_world({Con, Name, Ilk, Desc, Loc}).

enter_world({Con = {ConPid, _}, Name, Ilk, Desc, Loc = {_, LocPid}}) ->
    look(LocPid, ConPid),
    loop({Con, Name, Ilk, Desc, Loc}).

loop(State = {Con = {ConPid, ConRef}, Name, Ilk, Desc, Loc}) ->
  receive
    {From, Ref, short_stat} ->
        SS = Name ++ " (* Healthy Fresh)",
        From ! {Ref, SS},
        loop(State);
    {observe, Event} ->
        ConPid ! {observation, Event},
        loop(State);
    {action, {Keyword, String}} ->
        NewState = evaluate(Keyword, String, State),
        loop(NewState);
    {From, Ref, actions} ->
        From ! {Ref, actions()},
        loop(State);
    {ConPid, divorce} ->
        divorce(State);
    Message = {'DOWN', ConRef, process, ConPid, _} ->
        con_down(Message, State);
    status ->
        note("Status:~n"
             "  Controller: ~p~n  Name: ~p~n"
             "  Ilk: ~p~n  Description: ~p~n  Loc: ~p",
             [Con, Name, Ilk, Desc, Loc]),
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

%% Action evaluation
evaluate("say", String, State = {{_, _}, Name, _, _, {_, LocPid}}) ->
    Origin = Name,
    Sound = "says,\"" ++ String ++ "\"",
    LocPid ! {audible, Origin, Sound},
    State;
evaluate("look", "", State = {{ConPid, _}, _, _, _, {_, LocPid}}) ->
    look(LocPid, ConPid),
    State;
evaluate("look", String, State = {{ConPid, _}, _, _, _, {_, LocPid}}) ->
    {Target, _} = head(String),
    View = loc:look(LocPid, Target),
    ConPid ! {look, target, View},
    State;
evaluate("go", String, {Con = {ConPid, _}, Name, Ilk, Desc, {_, LocPid}}) ->
    {Target, _} = head(String),
    Me = {Name, self(), Ilk},
    NewLoc = depart(LocPid, Me, Target, ConPid),
    {Con, Name, Ilk, Desc, NewLoc};
evaluate(Keyword, String, State = {{ConPid, _}, Name, _, _, _}) ->
    Message = io_lib:format("~p received {~p,~p}", [Name, Keyword, String]),
    ConPid ! {notice, Message},
    State.

%% Action functions
look(LocPid, ConPid) ->
    View = loc:look(LocPid),
    ConPid ! {look, loc, View},
    ok.

depart(LocPid, Me, Target, ConPid) ->
    case loc:depart(LocPid, Me, Target) of
        {ok, WayPid} ->
            {ok, NewLoc = {_, NewLocPid}} = way:enter(WayPid, Me),
            look(NewLocPid, ConPid),
            NewLoc;
        {error, noexit} ->
            loc:bounce(LocPid, Me);
        Res = {fail, _} ->
            note("Received ~p from loc:depart/3", [Res]),
            mobman:relocate(Me)
    end.

%% Magic
actions() ->
    [{"say", "Say something out loud."},
     {"look", "View your surroundings."},
     {"look Target", "Look at Target, if present."}].

head(Line) ->
    Stripped = string:strip(Line),
    {Head, Tail} = head([], Stripped),
    {lists:reverse(Head), Tail}.

head(Word, []) ->
    {Word, []};
head(Word, [H|T]) ->
    case H of
        $\s -> {Word, T};
        Z   -> head([Z|Word], T)
    end.

divorce(State = {_, Name, Ilk, _, {_, LocPid}}) ->
    note("Controller cut ties. Retiring."),
    ok = loc:mob_vanish(LocPid, {Name, self(), Ilk}),
    ok = charman:save(State).

con_down(Message, State = {_, Name, Ilk, _, {_, LocPid}}) ->
    note("Controller died with ~p~n  ...I'm dead.", [Message]),
    ok = loc:mob_vanish(LocPid, {Name, self(), Ilk}),
    ok = charman:save(State).
%   NewState = spawn_controller(State),
%   loop(NewState);

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

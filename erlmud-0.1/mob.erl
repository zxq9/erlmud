-module(mob).
-export([start_link/2, code_change/1,
         get_actions/1]).

%% Interface

get_actions(MobPid) -> em_lib:call(MobPid, actions).

%% Startup
start_link(Con, Conf) ->
    spawn_link(fun() -> init(Con, Conf) end).

init(Con, Conf = {Name, {Ilk, Desc, LocID}}) ->
    note("Initializing with ~p", [Conf]),
    Ref = monitor(process, Con),
    {ok, LocPid} = locman:get_pid(LocID),
    ok = loc:mob_jump(LocPid, {Name, self(), Ilk}),
    loop({{Con, Ref}, Name, Ilk, Desc, {LocID, LocPid}}).

loop(State = {Con = {ConPid, ConRef}, Name, Ilk, Desc, Loc}) ->
  receive
    {From, Ref, short_stat} ->
        SS = Name ++ " (* Healthy Fresh)",
        From ! {Ref, SS},
        loop(State);
    {observe, String} ->
        ConPid ! {notice, String},
        loop(State);
    {action, {Keyword, String}} ->
        NewState = evaluate(Keyword, String, State),
        loop(NewState);
    {From, Ref, actions} ->
        From ! {Ref, actions()},
        loop(State);
    Message = {'DOWN', ConRef, process, ConPid, _} ->
        note("Controller died with ~p~n  ...I'm dead.", [Message]);
%       NewState = spawn_controller(State),
%       loop(NewState);
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
evaluate(Keyword, String, State = {{ConPid, _}, Name, _, _, _}) ->
    Message = io_lib:format("~p received {~p,~p}", [Name, Keyword, String]),
    ConPid ! {notice, Message},
    State.

%% Magic
actions() ->
    [{"say", "Say something out loud."}].

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

-module(mob).
-export([start_link/2, code_change/1,
         condition/1, incoming/2, get_actions/2]).

%% Interface
condition(MobPid) ->
    em_lib:call(MobPid, condition).

incoming(MobPid, Event) ->
    em_lib:call(MobPid, incoming, Event).

get_actions(MobPid, Form) ->
    em_lib:call(MobPid, actions, Form).

%% Startup
start_link(Con, Conf) ->
    spawn_link(fun() -> init(Con, Conf) end).

% Info = {Name, [Aliases], Ilk, Class, Desc, Sex}
% Stat = {Condition = {Health = {CurHP, MaxHP}, Stamina = {CurSP, MaxSP}},
%         Inventory = {{Weight, Equipped}, {Weight, Carried}},
%         Effects   = [{Name, Magnitude}],
%         Skills    = {Passive = [{PSkill, Proficiency}], Active = [{ASkill, Proficiency}]},
%         Score     = {Level, Exp = {Point, Next}},
%         Stats     = {Str, Int, Wil, Dex, Con, Speed, Agg, Chaos, Law},
%   ...or change the internals with the Ilk
init(ConPid, Conf = {Info = {Name, Aliases, Ilk, _, _, _}, LastStat, LastLocID}) ->
    note("Initializing with ~p", [Conf]),
    ConRef = monitor(process, ConPid),
    Me = {Name, self(), Aliases, ?MODULE, Ilk},
    Con = {ConPid, ConRef},
    locless(Me, {Con, Info, LastStat}, LastLocID).

locless(Me, {Con, Info, Stat}, none) ->
    Loc = mobman:relocate(Me, default),
    enter_world({Con, Info, Stat, Loc});
locless(Me, {Con, Info, Stat}, LastLocID) ->
    Loc = mobman:relocate(Me, LastLocID),
    enter_world({Con, Info, Stat, Loc}).

enter_world(State = {_, {Name, _, _, _, _, _}, _, {_, LocPid}}) ->
    loc:event(LocPid, {observation, {10000, {warp, Name, success}}}),
    self() ! {action, {unobservable, {look, []}}},
    loop(State).

loop(State = {{ConPid, ConRef}, {_, _, Ilk, _, _, _}, Stat, _}) ->
  receive
    {From, Ref, condition} ->
        {Condition, _, _, _, _, _} = Stat,
        From ! {Ref, Condition},
        loop(State);
    {observation, {Magnitude, Event}} ->
        NewState = Ilk:observe(Magnitude, Event, State),
        loop(NewState);
    {action, {Nature, Data}} ->
        NewState = Ilk:evaluate(Nature, Data, State),
        loop(NewState);
    {From, Ref, {incoming, Event}} ->
        {Result, NewState} = Ilk:react(Event, State),
        From ! {Ref, Result},
        loop(NewState);
    {From, Ref, {actions, Form}} ->
        From ! {Ref, Ilk:actions(Form)},
        loop(State);
    {ConPid, divorce} ->
        divorce(State);
    Message = {'DOWN', ConRef, process, ConPid, _} ->
        con_down(Message, State);
    code_change ->
        ?MODULE:code_change(State);
    shutdown ->
        note("Shutting down."),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(State)
  end.

%% Magic
divorce(State) ->
    note("Controller cut ties. Retiring."),
    retire(State).

con_down(Message, State) ->
    note("Controller died with ~p~n  ...I'm dead.", [Message]),
    retire(State).
%   NewState = spawn_controller(State),
%   loop(NewState);

retire({_, Info = {Name, Aliases, Ilk, _, _, _}, Stat, {LocID, LocPid}}) ->
    loc:event(LocPid, {observation, {10000, {poof, Name, success}}}),
    ok = loc:drop(LocPid, {Name, self(), Aliases, ?MODULE, Ilk}),
    ok = charman:save({Name, {Info, Stat, LocID}}).

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

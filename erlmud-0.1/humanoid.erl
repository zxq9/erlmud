-module(humanoid).
-export([observe/3, evaluate/3, react/2,
         level/1, actions/1, physique/3, capital/1]).

% Interface
observe(Magnitude, Event, State) ->
    ConPid = mob:read(con_pid, State),
    Name = mob:read(name, State),
    perceive(Magnitude, Event, ConPid, Name, State).

perceive(Magnitude, {{Verb, Name}, Name, Outcome}, ConPid, Name, State) ->
    case detect(Magnitude, State) of
        true  -> ConPid ! {observation, {{Verb, self}, self, Outcome}};
        false -> ok
    end,
    State;
perceive(Magnitude, {Action, Name, Outcome}, ConPid, Name, State) ->
    case detect(Magnitude, State) of
        true  -> ConPid ! {observation, {Action, self, Outcome}};
        false -> ok
    end,
    State;
perceive(Magnitude, {{Verb, Name}, Actor, Outcome}, ConPid, Name, State) ->
    case detect(Magnitude, State) of
        true  -> ConPid ! {observation, {{Verb, self}, Actor, Outcome}};
        false -> ok
    end,
    State;
perceive(Magnitude, {Action, Actor, Outcome}, ConPid, _, State) ->
    case detect(Magnitude, State) of
        true  -> ConPid ! {observation, {Action, Actor, Outcome}};
        false -> ok
    end,
    State.

evaluate(observable, {Verb, Data}, State) ->
    observable(Verb, Data, State);
evaluate(unobservable, {Verb, Data}, State) ->
    unobservable(Verb, Data, State).

react({glance, _}, State) ->
    Info = mob:read(info, State),
    Status = mob:read(status, State),
    {{ok, {Info, Status}}, State};
react(Event, State) ->
    note("Received ~p", [Event]),
    {{ok, "You got me."}, State}.

%% Magic
detect(Magnitude, _) -> Magnitude > 1.

observable(glance, Data, State) ->
    ConPid = mob:read(con_pid, State),
    Name = mob:read(name, State),
    LocPid = mob:read(loc_pid, State),
    case head(Data) of
        {Name, _} ->
            emit(LocPid, State, {glance, Name}, Name, failure);
        {Target, _} -> 
            case loc:action(LocPid, Target, {glance, Name}) of
                {ok, View} ->
                    emit(LocPid, State, {glance, Target}, Name, success),
                    ConPid ! {observation, {{glance, Target}, self, View}};
                {error, _} ->
                    emit(LocPid, State, {glance, Target}, Name, failure)
            end
    end,
    State;
observable(say, Data, State) ->
    Name = mob:read(name, State),
    LocPid = mob:read(loc_pid, State),
    emit(LocPid, State, {say, Data}, Name, success),
    State;
observable(go, Data, State) ->
    ConPid = mob:read(con_pid, State),
    Name = mob:read(name, State),
    Aliases = mob:read(aliases, State),
    Loc = mob:read(loc, State),
    LocPid = mob:read(loc_pid, State),
    {Target, _} = head(Data),
    Me = {Name, self(), Aliases, mob, ?MODULE},
    NewLoc = case go(Target, Me, LocPid) of
        {{ok, New = {_, NewLocPid}}, OutName} ->
            emit(LocPid, State, {depart, Target}, Name, success),
            emit(NewLocPid, State, {arrive, OutName}, Name, success),
            View = loc:look(NewLocPid),
            ConPid ! {observation, {look, self, View}},
            New;
        {error, noexit} ->
            emit(LocPid, State, {depart, Target}, Name, failure),
            Loc;
        Res = {fail, {_, New = {_, NewLocPid}}} ->
            note("Received ~p from loc:depart/3", [Res]),
            emit(NewLocPid, State, jump, Name, success),
            New
    end,
    mob:edit(loc, NewLoc, State).

unobservable(look, _, State) ->
    View = loc:look(mob:read(loc_pid, State)),
    mob:read(con_pid, State) ! {observation, {look, self, View}},
    State;
unobservable(status, _, State) ->
    mob:read(con_pid, State) ! {observation, {status, self, State}},
    State.

emit(LocPid, State, Action, Actor, Outcome) ->
    Magnitude = magnitude(Action, State),
    Signal = {Magnitude, {Action, Actor, Outcome}},
    loc:event(LocPid, {observation, Signal}).

magnitude(_, _) -> 10000.

go(Target, Me, LocPid) ->
    case loc:depart(LocPid, Me, Target) of
        {ok, WayPid, OutName} ->
            {way:enter(WayPid, Me), OutName};
        Error = {error, _} ->
            Error;
        Res = {fail, _} ->
            note("Received ~p from loc:depart/3", [Res]),
            {fail, mobman:relocate(Me)}
    end.

%% Definitions

level(Exp) ->
    Levels = [1, 50, 100, 200, 400, 800, 1600],
    length(lists:takewhile(fun(X) -> Exp > X end, Levels)).

actions(text) ->
    [{"go", go, observable,
      "go Exit", "Move to a new location through Exit"},
     {"say", say, observable,
      "say Text", "Say something out loud"},
     {"status", status, unobservable,
      "status", "Check your character's current status"},
     {"look", look, unobservable,
      "look", "View your surroundings"},
     {"glance", glance, observable,
      "glance Target", "Look at Target"}].

physique("human", "Lampas", "male") ->
    {{185, 195}, {35, 35, 15}, {110, 110, 90, 90, 110, 100}};
physique(_Species, _Homeland, _Sex) ->
    {{175, 175}, {25, 20, 40}, {90, 90, 110, 110, 90, 100}}.

capital("Lampas")  -> {1,0,0};
capital(_Homeland) -> {0,0,0}.

%% Binary & String handling
head(Line) ->
    Stripped = string:strip(Line),
    {Head, Tail} = head([], Stripped),
    {lists:reverse(Head), Tail}.

head(Word, []) ->
    {Word, []};
head([], [$\s|T]) ->
    head([], T);
head(Word, [H|T]) ->
    case H of
        $\s -> {Word, T};
        Z   -> head([Z|Word], T)
    end.

%% System
note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

% {observable, untargeted}
%   - Cast to location
%   stand/sit/sleep/wake say emote
% {observable, targeted}
%   - Cast to location
%   eat sheath mend light hold wear remove wield drink
%   - Call through location to target, cast result(s) to location
%   kill [emote Target] get pick drop steal palm take put go prac 
% {unobservable, untargeted}
%   - Silent
%   score status level
%   - Locally silent, cast to all affected area locations
%   shout (to a zone)
%   yell (to current an adjacent locations -- direction is known)
%   bellow (to current and X-hops-away locations -- direction is known)
% {unobservable, targeted}
%   - Silent
%   tell whisper

% Observations:
%   {observation, {Magnitude, Action, Who, Outcome}}
%   "Susan appears out of nowhere."
%   {observation, {Magnitude, {Verb, Target}, Who, Outcome}}
%   "Susan arrives from the west."
%   "Brian looks at Susan."
%   "Susan smashes Brian's head into bloody fragments!"
%   "Susan picks up a dagger."
%   "Brian tries to bash Susan, but stumbles and falls!"
%   {observation, {Magnitude, {Verb, DO, IO}, Who, Outcome}}
%   "Brian gives Susan a gold coin."
%   "Susan gets a dirty old pair of pants from a barrel."
%   "Mishmash rips a bloody leg from the corpse of Brian."
%   "Susan pokes Brian's eye with a stick."

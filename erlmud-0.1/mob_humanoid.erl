-module(mob_humanoid).
-export([observe/3, evaluate/3, react/2,
         level/1, species/0, class/0,
         con_ext/1]).

%% Interface
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

con_ext(text) -> telcon_humanoid.

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

species() ->
    [{"human",
      [{"sex",
        [{"male",
          [{height, {160, 178, 202}}, {weight, {60000, 70000, 120000}},
           {max_hp, {30, 35, 45}}, {max_sp, {95, 100, 125}}, {max_mp, {15, 20, 30}},
           {description, "A man."}, {aliases, ["man", "male", "human"]},
           {str, {110, 150, 190}}, {int, {90, 110, 190}},  {wil, {90, 120, 190}},
           {dex, {110, 150, 190}}, {con, {110, 150, 190}}, {spd, {100, 150, 190}},
           {morality, {10, 25, 40}}, {chaos, {10, 15, 20}}, {law, {10, 25, 40}}]},
         {"female",
          [{height, {145, 164, 187}}, {weight, {45000, 63000, 90000}},
           {max_hp, {20, 25, 42}}, {max_sp, {85, 90, 115}}, {max_mp, {25, 35, 45}},
           {description, "A woman."}, {aliases, ["woman", "female", "human"]},
           {str, {90, 130, 190}},  {int, {110, 140, 190}}, {wil, {110, 150, 190}},
           {dex, {100, 150, 190}}, {con, {90, 140, 190}},  {spd, {100, 150, 190}},
           {morality, {10, 25, 40}}, {chaos, {10, 15, 20}}, {law, {10, 25, 40}}]}]},
       {"homeland",
        [{"Altenia",
          [{aliases, ["Altenian"]},
           {height, 0}, {weight, 0},
           {max_hp, 0}, {max_sp, 0}, {max_mp, 0},
           {str, 0}, {int, 0}, {wil, 0}, {dex, 0}, {con, 0}, {spd, 0},
           {morality, 15}, {chaos, 0}, {law, 0}]},
         {"Lua",
          [{aliases, ["Luite"]},
           {height, 0}, {weight, 0},
           {max_hp, 0}, {max_sp, 0}, {max_mp, 0},
           {str, 0}, {int, 0}, {wil, 0}, {dex, 0}, {con, 0}, {spd, 0},
           {morality, 0}, {chaos, 0}, {law, 0}]}]},
       {"loc", [{"Circle of Light", {0,0,1}}]}]},
     {"kinolc",
      [{"sex",
        [{"male",
          [{height, {150, 190, 210}}, {weight, {80000, 100000, 140000}},
           {max_hp, {25, 30, 40}}, {max_sp, {130, 140, 150}}, {max_mp, {25, 30, 40}},
           {description, "A male kinolc."}, {aliases, ["kinolc", "male"]},
           {str, {130, 160, 200}}, {int, {70, 110, 140}}, {wil, {60, 100, 130}},
           {dex, {100, 150, 190}}, {con, {90, 140, 190}}, {spd, {80, 130, 180}},
           {morality, {10, 25, 40}}, {chaos, {10, 15, 20}}, {law, {10, 25, 40}}]},
         {"female",
          [{height, {150, 200, 220}}, {weight, {90000, 110000, 160000}},
           {max_hp, {35, 45, 50}}, {max_sp, {140, 160, 180}}, {max_mp, {10, 13, 20}},
           {description, "A female kinolc."}, {aliases, ["kinolc", "female"]},
           {str, {140, 170, 200}}, {int, {110, 140, 190}}, {wil, {110, 150, 190}},
           {dex, {110, 150, 190}}, {con, {90, 140, 190}},  {spd, {90, 140, 180}},
           {morality, {10, 25, 40}}, {chaos, {10, 15, 20}}, {law, {10, 25, 40}}]}]},
       {"ilk",
        [{"Shaik",
          [{aliases, ["shaikin"]},
           {height, 0}, {weight, 0},
           {max_hp, 0}, {max_sp, 0}, {max_mp, 0},
           {str, 0}, {int, 0}, {wil, 0}, {dex, 0}, {con, 0}, {spd, 0},
           {morality, 0}, {chaos, 0}, {law, 0}]}]},
       {"loc", [{"Pit of Despair", {0,0,-1}}]}]}].

class() ->
    [{"ranger",
      [{aliases, ["ranger", "walker", "snakeater"]},
       {height, 0}, {weight, 0},
       {max_hp, 0}, {max_sp, 0}, {max_mp, 0},
       {str, 0}, {int, 0}, {wil, 0}, {dex, 0}, {con, 0}, {spd, 0},
       {morality, 5}, {chaos, 5}, {law, -10}]},
     {"warrior",
      [{aliases, ["warrior", "brute", "knuckledragger"]},
       {height, 5}, {weight, 10},
       {max_hp, 10}, {max_sp, 0}, {max_mp, -10},
       {str, 10}, {int, -10}, {wil, -10}, {dex, 0}, {con, 10}, {spd, 0},
       {morality, -20}, {chaos, 10}, {law, 10}]},
     {"rogue",
      [{aliases, ["rogue", "scoundrel", "thief"]},
       {height, 0}, {weight, 0},
       {max_hp, 0}, {max_sp, 0}, {max_mp, 0},
       {str, -10}, {int, 0}, {wil, 10}, {dex, 10}, {con, -10}, {spd, 0},
       {morality, 20}, {chaos, -10}, {law, -10}]},
     {"mage",
      [{aliases, ["mage", "potter", "nerd", "wuss"]},
       {height, -5}, {weight, -10},
       {max_hp, -10}, {max_sp, 0}, {max_mp, 10},
       {str, -10}, {int, 10}, {wil, 10}, {dex, 0}, {con, -10}, {spd, 0},
       {morality, 15}, {chaos, -20}, {law, 5}]}].

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

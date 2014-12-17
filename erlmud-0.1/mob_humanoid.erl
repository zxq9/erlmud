-module(mob_humanoid).
-export([observe/3, perform/3, react/2,
         level/1, species/0,
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

perform(go, Exit, State) ->
    go(Exit,
       mob:read(loc, State),
       mob:read(loc_pid, State),
       mob:read(name, State),
       mob:read(con_pid, State),
       mob:me(State),
       State);
perform(say, Words, State) ->
    say(Words, mob:read(name, State), mob:read(loc_pid, State), State),
    State;
perform(status, self, State) ->
    status(mob:read(con_pid, State), State),
    State;
perform(look, loc, State) ->
    View = loc:look(mob:read(loc_pid, State)),
    mob:read(con_pid, State) ! {observation, {look, self, View}},
    State;
perform(look, Target, State) ->
    look(Target,
         mob:read(name, State),
         mob:read(con_pid, State),
         mob:read(loc_pid, State),
         State),
    State;
perform(take, Target, State) ->
    take(Target, self(), mob:read(name, State), mob:read(loc_pid, State)),
    State;
perform(inventory, self, State) ->
    mob:read(con_pid, State) ! {observation, {inventory, self, mob:read(held, State)}},
    State;
perform(equipment, self, State) ->
    mob:read(con_pid, State) ! {observation, {equipment, self, mob:read(worn, State)}},
    State.

% NOTE: A few different ways 
%       Compare with telcon_humanoid:render_look/1
react({look, _}, State) ->
    View = {mob:read(species, State),
            mob:read(class, State),
            mob:read(homeland, State),
            mob:read(description, State),
            mob:read(hp, State),
            mob:read(worn, State),
            mob:read(held, State)},
    {{ok, View}, State};

%   Visible = [species, class, homeland, description, hp, worn, held],
%   View = lists:foldl(fun(V, Acc) -> [{V, mob:read(V, State)} | Acc] end, [], Visible),
%   {{ok, View}, State};

%   Visible = [species, class, homeland, description, hp, worn, held],
%   View = lists:foldl(fun(V, Acc) -> [mob:read(V, State) | Acc] end, [], Visible),
%   {{ok, list_to_tuple(lists:reverse(View))}, State};
react(Event, State) ->
    note("Received ~p", [Event]),
    {{ok, "You got me."}, State}.

%% Action implementations
go(Exit, Loc, LocPid, Name, ConPid, Me, State) ->
    NewLoc = case depart(Exit, Me, LocPid) of
        {{ok, New = {_, NewLocPid}}, OutName} ->
            emit(LocPid, State, {depart, Exit}, Name, success),
            emit(NewLocPid, State, {arrive, OutName}, Name, success),
            View = loc:look(NewLocPid),
            ConPid ! {observation, {look, self, View}},
            New;
        {error, noexit} ->
            emit(LocPid, State, {depart, Exit}, Name, failure),
            Loc;
        Res = {fail, {_, New = {_, NewLocPid}}} ->
            note("Received ~p from loc:depart/3", [Res]),
            emit(NewLocPid, State, jump, Name, success),
            New
    end,
    mob:edit(loc, NewLoc, State).

say(Words, Name, LocPid, State) ->
    emit(LocPid, State, {say, Words}, Name, success).

status(ConPid, State) ->
    ConPid ! {observation, {status, self, State}}.

look(Target, Name, ConPid, LocPid, State) ->
    case loc:action(LocPid, Target, {look, Name}) of
        {ok, View} ->
            emit(LocPid, State, {look, Target}, Name, success),
            ConPid ! {observation, {{look, Target}, self, View}};
        {error, self} ->
            emit(LocPid, State, {look, Name}, Name, failure);
        {error, _} ->
            emit(LocPid, State, {look, Target}, Name, failure)
    end.

take(Target, Self, Name, LocPid) ->
    spawn_link(fun() -> hand(Name, Target, LocPid, Self) end).

%% Magic
con_ext(text) -> telcon_humanoid.

detect(Magnitude, _) -> Magnitude > 1.

emit(LocPid, State, Action, Actor, Outcome) ->
    Magnitude = magnitude(Action, State),
    Signal = {Magnitude, {Action, Actor, Outcome}},
    loc:event(LocPid, {observation, Signal}).

magnitude(_, _) -> 10000.

depart(Target, Me, LocPid) ->
    case loc:depart(LocPid, Me, Target) of
        {ok, WayPid, OutName} ->
            {way:enter(WayPid, Me), OutName};
        Error = {error, _} ->
            Error;
        Res = {fail, _} ->
            note("Received ~p from loc:depart/3", [Res]),
            {fail, mobman:relocate(Me)}
    end.

hand(Name, Target, HolderPid, RecipientPid) ->
    TRef = make_ref(),
    case em_lib:call(HolderPid, transfer, {Target, TRef}) of
        {ok, TPid} ->
            link(TPid),
            TEntity = em_lib:call(TPid, {move, RecipientPid}),
            ok = em_lib:call(RecipientPid, load, TEntity),
            unlink(TPid),
            HolderPid ! {ok, TRef},
            HolderPid ! {event, {observation, {10000, {take, Name, success}}}};
        M = {error, _} ->
            M
    end.

%% Definitions
level(Exp) ->
    Levels = [1, 50, 100, 200, 400, 800, 1600],
    length(lists:takewhile(fun(X) -> Exp > X end, Levels)).

species() ->
    [{"human",
      {[{aliases,   {set, ["human"]}},
        {ilk,       {set, mob_humanoid}},
        {species,   {set, "human"}},
        {height,    {roll, {145, 171, 202}}},
        {weight,    {roll, {45000, 65000, 120000}}},
        {max_hp,    {roll, {20, 30, 45}}},
        {max_sp,    {roll, {85, 100, 125}}},
        {max_mp,    {roll, {15, 25, 45}}},
        {str,       {roll, {90, 140, 190}}},
        {int,       {roll, {90, 140, 190}}},
        {wil,       {roll, {90, 140, 190}}},
        {dex,       {roll, {90, 140, 190}}},
        {con,       {roll, {90, 140, 190}}},
        {spd,       {roll, {90, 140, 190}}},
        {morality,  {roll, {10, 25, 40}}},
        {chaos,     {roll, {10, 15, 20}}},
        {law,       {roll, {10, 15, 40}}},
        {loc_id,    {set, {0,0,1}}}],
       [{"sex",
         [{"male",
           [{sex,           {set, "male"}},
            {description,   {set, "A man."}},
            {aliases,       {append, ["man", "male"]}},
            {height,        {add, 7}},
            {weight,        {add, 16000}},
            {max_hp,        {add, 10}},
            {max_sp,        {add, 10}},
            {max_mp,        {add, -5}}]},
          {"female",
           [{sex,           {set, "female"}},
            {description,   {set, "A woman."}},
            {aliases,       {append, ["woman", "female"]}},
            {height,        {add, -7}},
            {weight,        {add, -16000}},
            {max_hp,        {add, -5}},
            {max_sp,        {add, -5}},
            {max_mp,        {add, 10}}]}]},
        {"homeland",
         [{"Altenia",
           [{homeland,      {set, "Altenia"}},
            {aliases,       {append, ["Altenian"]}},
            {morality,      {add, 15}}]},
          {"Lua",
           [{homeland,      {set, "Lua"}},
            {aliases,       {append, ["Luite"]}},
            {chaos,         {add, 15}}]}]},
        {"Starting Location",
         [{"Circle of Light",
           [{loc_id,        {set, {0,0,1}}}]}]},
        {"class",
         proplists:get_value(definitions, class())}]}},
     {"kinolc",
      {[{aliases,   {set, ["kinolc"]}},
        {ilk,       {set, mob_humanoid}},
        {species,   {set, "kinolc"}},
        {height,    {roll, {170, 190, 230}}},
        {weight,    {roll, {130000, 160000, 190000}}},
        {max_hp,    {roll, {25, 40, 50}}},
        {max_sp,    {roll, {130, 160, 180}}},
        {max_mp,    {roll, {15, 20, 30}}},
        {str,       {roll, {130, 170, 200}}},
        {int,       {roll, {50, 80, 120}}},
        {wil,       {roll, {50, 80, 120}}},
        {dex,       {roll, {100, 140, 190}}},
        {con,       {roll, {90, 140, 190}}},
        {spd,       {roll, {80, 130, 190}}},
        {morality,  {roll, {-40, -25, -15}}},
        {chaos,     {roll, {-15, 0, 15}}},
        {law,       {roll, {-15, 0, 15}}}],
       [{"sex",
         [{"male",
           [{sex,           {set, "male"}},
            {description,   {set, "A male kinolc."}},
            {aliases,       {append, ["male"]}},
            {height,        {add, -10}},
            {weight,        {add, -20000}},
            {max_hp,        {add, -10}},
            {max_sp,        {add, -10}},
            {max_mp,        {add, 5}}]},
          {"female",
           [{sex,           {set, "female"}},
            {description,   {set, "A female kinolc."}},
            {aliases,       {append, ["female"]}},
            {height,        {add, 10}},
            {weight,        {add, 20000}},
            {max_hp,        {add, 10}},
            {max_sp,        {add, 10}},
            {max_mp,        {add, -5}}]}]},
        {"homeland",
         [{"Shaik",
           [{homeland,      {set, "Shaik"}},
            {aliases,       {append, ["shaikin"]}}]}]},
        {"Starting Location",
         [{"Pit of Despair",
           [{loc_id,        {set, {0,0,-1}}}]}]},
        {"class",
         proplists:get_value(definitions, class())}]}}].

class() ->
    [{definitions,
      [{"ranger",
        [{class,      {set, "ranger"}},
         {aliases,    {append, ["ranger", "walker", "snakeater"]}},
         {max_sp,     {add, 10}},
         {morality,   {add, 5}},
         {chaos,      {add, 5}},
         {law,        {add, -10}}]},
       {"warrior",
        [{class,      {set, "warrior"}},
         {aliases,    {append, ["warrior", "brute", "knuckledragger"]}},
         {height,     {add, 5}},
         {weight,     {add, 7000}},
         {max_hp,     {add, 10}},
         {max_sp,     {add, -5}},
         {max_mp,     {add, -10}},
         {str,        {add, 10}},
         {int,        {add, -10}},
         {wil,        {add, -10}},
         {con,        {add, 10}},
         {morality,   {add, -20}},
         {chaos,      {add, 10}},
         {law,        {add, 10}}]},
       {"rogue",
        [{class,      {set, "rogue"}},
         {aliases,    {append, ["rogue", "scoundrel", "thief"]}},
         {max_sp,     {add, 10}},
         {str,        {add, -10}},
         {wil,        {add, 10}},
         {dex,        {add, 10}},
         {con,        {add, -10}},
         {spd,        {add, 10}},
         {morality,   {add, 20}},
         {chaos,      {add, -10}},
         {law,        {add, -20}}]},
       {"mage",
        [{class,      {set, "mage"}},
         {aliases,    {append, ["mage", "potter", "nerd", "wuss"]}},
         {height,     {add, -5}},
         {weight,     {add, -10000}},
         {max_hp,     {add, -10}},
         {max_sp,     {add, -5}},
         {max_mp,     {add, 15}},
         {str,        {add, -10}},
         {int,        {add, 10}},
         {wil,        {add, 10}},
         {con,        {add, -10}},
         {morality,   {add, 15}},
         {chaos,      {add, -20}},
         {law,        {add, 5}}]}]}].

%% System
note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

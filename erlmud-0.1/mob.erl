-module(mob).
-export([start_link/2, code_change/1,
         new/1, read/2, edit/3, me/1,
         check/2,
         roll/2, reroll/1, topoff/1]).

%% Type interface
new(Name) ->
    DOB = calendar:universal_time(),
    {Ilk, Aliases, Description} = {none, [], ""},
    {Species, Class, Homeland, Sex} = {"mob", "mob", "Guf", "nh"},
    {{Height, Weight}, {BaseHP, BaseSP, BaseMP}, Stats} =
        {{1, 1}, {1, 1, 1}, {1, 1, 1, 1, 1, 1}},
    Alignment = {0, 0, 0}, % {Morality, Chaos, Law}
    Vis = 10000,
    OB = 10,
    PB = 10,
    DB = 10,
    Abs = 0,
    Health = {{BaseHP, BaseHP}, {BaseSP, BaseSP}, {BaseMP, BaseMP}},
    Condition = {Health, Vis, OB, PB, DB, Abs},
    WornInv = [],
    WornAli = dict:new(),
    Worn = {WornInv, WornAli},
    WornWeight = em_lib:calc_weight(WornInv),
    Carried = inventory:new(),
    Inventory = {{WornWeight, Worn}, Carried},
    Effects = [],
    PassiveSkills = [],
    ActiveSkills = [],
    Skills = {PassiveSkills, ActiveSkills},
    Level = 1,
    Exp = 1,
    Score = {Level, Exp},
    LocID = {0, 0, 0},
    {{?MODULE, 0},
     {{none, none},
      {Name, Aliases},
      {Ilk, Species, Class, Homeland, DOB, Height, Weight, Sex, Stats},
      {Description, Condition, Inventory, Effects, Skills, Score, Alignment},
      {LocID, none}}}.

read(mod, {Mod, _}) -> Mod;
read(module, Mob)  -> element(1, read(mod, Mob));
read(version, Mob) -> element(2, read(mod, Mob));

read(controller, {_, {Con, _, _, _, _}}) -> Con;
read(con_pid, Mob) -> element(1, read(controller, Mob));
read(con_ref, Mob) -> element(2, read(controller, Mob));

read(names, {_, {_, Names, _, _, _}}) -> Names;
read(name, Mob)    -> element(1, read(names, Mob));
read(aliases, Mob) -> element(2, read(names, Mob));
read(id, Mob)      -> read(name, Mob);

read(info, {_, {_, _, Info, _, _}}) -> Info;
read(ilk, Mob)      -> element(1, read(info, Mob));
read(species, Mob)  -> element(2, read(info, Mob));
read(class, Mob)    -> element(3, read(info, Mob));
read(homeland, Mob) -> element(4, read(info, Mob));
read(dob, Mob)      -> element(5, read(info, Mob));
read(height, Mob)   -> element(6, read(info, Mob));
read(weight, Mob)   -> element(7, read(info, Mob));
read(sex, Mob)      -> element(8, read(info, Mob));
read(stats, Mob)    -> element(9, read(info, Mob));

read(str, Mob) -> element(1, read(stats, Mob));
read(int, Mob) -> element(2, read(stats, Mob));
read(wil, Mob) -> element(3, read(stats, Mob));
read(dex, Mob) -> element(4, read(stats, Mob));
read(con, Mob) -> element(5, read(stats, Mob));
read(spd, Mob) -> element(6, read(stats, Mob));

read(status, {_, {_, _, _, Status, _}}) -> Status;
read(description, Mob) -> element(1, read(status, Mob));
read(condition, Mob)   -> element(2, read(status, Mob));
read(inventory, Mob)   -> element(3, read(status, Mob));
read(effects, Mob)     -> element(4, read(status, Mob));
read(skills, Mob)      -> element(5, read(status, Mob));
read(score, Mob)       -> element(6, read(status, Mob));
read(alignment, Mob)   -> element(7, read(status, Mob));

read(equip, Mob)       -> element(1, read(inventory, Mob));
read(worn_weight, Mob) -> element(1, read(equip, Mob));
read(worn, Mob)        -> element(2, read(equip, Mob));
read(worn_inv, Mob)    -> element(1, read(worn, Mob));
read(worn_ali, Mob)    -> element(2, read(worn, Mob));

read(held, Mob)        -> inventory:to_list(element(2, read(inventory, Mob)));
read(held_weight, Mob) -> inventory:weight(element(2, read(inventory, Mob)));

read(passive_skills, Mob) -> element(1, read(skills, Mob));
read(active_skills, Mob)  -> element(2, read(skills, Mob));

read(level, Mob) -> element(1, read(score, Mob));
read(exp, Mob)   -> element(2, read(score, Mob));

read(morality, Mob) -> element(1, read(alignment, Mob));
read(chaos, Mob)    -> element(2, read(alignment, Mob));
read(law, Mob)      -> element(3, read(alignment, Mob));

read(health, Mob) -> element(1, read(condition, Mob));
read(hp, Mob)     -> element(1, read(health, Mob));
read(cur_hp, Mob) -> element(1, read(hp, Mob));
read(max_hp, Mob) -> element(2, read(hp, Mob));

read(sp, Mob)     -> element(2, read(health, Mob));
read(cur_sp, Mob) -> element(1, read(sp, Mob));
read(max_sp, Mob) -> element(2, read(sp, Mob));

read(mp, Mob)     -> element(3, read(health, Mob));
read(cur_mp, Mob) -> element(1, read(mp, Mob));
read(max_mp, Mob) -> element(2, read(mp, Mob));

read(vis, Mob) -> element(2, read(condition, Mob));
read(ob, Mob)  -> element(3, read(condition, Mob));
read(pb, Mob)  -> element(4, read(condition, Mob));
read(db, Mob)  -> element(5, read(condition, Mob));
read(abs, Mob) -> element(6, read(condition, Mob));

read(total_weight, Mob) ->
    read(weight, Mob) + read(worn_weight, Mob) + read(held_weight, Mob);

read(loc, {_, {_, _, _, _, Loc}}) -> Loc;
read(loc_id, Mob)  -> element(1, read(loc, Mob));
read(loc_pid, Mob) -> element(2, read(loc, Mob)).

edit(controller, ConPid, {Type, {_, Names, Info, Status, Loc}}) ->
    ConRef = monitor(process, ConPid),
    {Type, {{ConPid, ConRef}, Names, Info, Status, Loc}};
edit(names, Names, {Type, {Con, _, Info, Status, Loc}}) ->
    {Type, {Con, Names, Info, Status, Loc}};
edit(name, Name, Mob) ->
    edit(names, {Name, read(aliases, Mob)}, Mob);
edit(aliases, Aliases, Mob) ->
    edit(names, {read(name, Mob), Aliases}, Mob);
edit(info, Info, {Type, {Con, Names, _, Status, Loc}}) ->
    {Type, {Con, Names, Info, Status, Loc}};
edit(ilk, Ilk, Mob) ->
    edit(info, setelement(1, read(info, Mob), Ilk), Mob);
edit(species, Species, Mob) ->
    edit(info, setelement(2, read(info, Mob), Species), Mob);
edit(class, Class, Mob) ->
    edit(info, setelement(3, read(info, Mob), Class), Mob);
edit(homeland, Homeland, Mob) ->
    edit(info, setelement(4, read(info, Mob), Homeland), Mob);
edit(dob, DOB, Mob) ->
    edit(info, setelement(5, read(info, Mob), DOB), Mob);
edit(height, Height, Mob) ->
    edit(info, setelement(6, read(info, Mob), Height), Mob);
edit(weight, Weight, Mob) ->
    edit(info, setelement(7, read(info, Mob), Weight), Mob);
edit(sex, Sex, Mob) ->
    edit(info, setelement(8, read(info, Mob), Sex), Mob);
edit(stats, Stats, Mob) ->
    edit(info, setelement(9, read(info, Mob), Stats), Mob);
edit(str, Str, Mob) ->
    edit(stats, setelement(1, read(stats, Mob), Str), Mob);
edit(int, Int, Mob) ->
    edit(stats, setelement(2, read(stats, Mob), Int), Mob);
edit(wil, Wil, Mob) ->
    edit(stats, setelement(3, read(stats, Mob), Wil), Mob);
edit(dex, Dex, Mob) ->
    edit(stats, setelement(4, read(stats, Mob), Dex), Mob);
edit(con, Con, Mob) ->
    edit(stats, setelement(5, read(stats, Mob), Con), Mob);
edit(spd, Spd, Mob) ->
    edit(stats, setelement(6, read(stats, Mob), Spd), Mob);
edit(status, Status, {Type, {Con, Names, Info, _, Loc}}) ->
    {Type, {Con, Names, Info, Status, Loc}};
edit(description, Desc, Mob) ->
    {_, Condition, Inv, Eff, Sk, Sc, Align} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Sc, Align}, Mob);
edit(condition, Condition, Mob) ->
    {Desc, _, Inv, Eff, Sk, Sc, Align} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Sc, Align}, Mob);
edit(health, Health, Mob) ->
    edit(condition, setelement(1, read(condition, Mob), Health), Mob);
edit(hp, HP, Mob) ->
    edit(health, setelement(1, read(health, Mob), HP), Mob);
edit(cur_hp, CurHP, Mob) ->
    edit(hp, {CurHP, read(max_hp, Mob)}, Mob);
edit(max_hp, MaxHP, Mob) ->
    edit(hp, {read(cur_hp, Mob), MaxHP}, Mob);
edit(sp, SP, Mob) ->
    edit(health, setelement(2, read(health, Mob), SP), Mob);
edit(cur_sp, CurSP, Mob) ->
    edit(sp, {CurSP, read(max_sp, Mob)}, Mob);
edit(max_sp, MaxSP, Mob) ->
    edit(sp, {read(cur_sp, Mob), MaxSP}, Mob);
edit(mp, MP, Mob) ->
    edit(health, setelement(3, read(health, Mob), MP), Mob);
edit(cur_mp, CurMP, Mob) ->
    edit(mp, {CurMP, read(max_hp, Mob)}, Mob);
edit(max_mp, MaxMP, Mob) ->
    edit(mp, {read(cur_mp, Mob), MaxMP}, Mob);
edit(vis, Vis, Mob) ->
    edit(condition, setelement(2, read(condition, Mob), Vis), Mob);
edit(ob, OB, Mob) ->
    edit(condition, setelement(3, read(condition, Mob), OB), Mob);
edit(pb, PB, Mob) ->
    edit(condition, setelement(4, read(condition, Mob), PB), Mob);
edit(db, DB, Mob) ->
    edit(condition, setelement(5, read(condition, Mob), DB), Mob);
edit(abs, Abs, Mob) ->
    edit(condition, setelement(6, read(condition, Mob), Abs), Mob);
edit(inventory, Inv, Mob) ->
    {Desc, Condition, _, Eff, Sk, Sc, Align} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Sc, Align}, Mob);
edit(worn_inv, WornInv, Mob) ->
    edit(inventory,
         {{em_lib:calc_weight(WornInv), {WornInv, em_lib:index_aliases(WornInv)}},
          read(carried, Mob)},
         Mob);
edit(held_inv, HeldInv, Mob) ->
    edit(inventory,
         {read(equip, Mob),
          {em_lib:calc_weight(HeldInv), {HeldInv, em_lib:index_aliases(HeldInv)}}},
         Mob);
edit(effects, Eff, Mob) ->
    {Desc, Condition, Inv, _, Sk, Sc, Align} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Sc, Align}, Mob);
edit(skills, Sk, Mob) ->
    {Desc, Condition, Inv, Eff, _, Sc, Align} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Sc, Align}, Mob);
edit(passive_skills, PassiveSkills, Mob) ->
    edit(skills, {PassiveSkills, read(active_skills, Mob)}, Mob);
edit(active_skills, ActiveSkills, Mob) ->
    edit(skills, {read(passive_skills, Mob), ActiveSkills}, Mob);
edit(score, Score, Mob) ->
    {Desc, Condition, Inv, Eff, Sk, _, Align} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Score, Align}, Mob);
edit(exp, Amount, Mob) ->
    Exp = read(exp, Mob) + Amount,
    Ilk = read(ilk, Mob),
    Level = Ilk:level(Exp),
    edit(score, {Level, Exp}, Mob);
edit(alignment, Align, Mob) ->
    edit(status, setelement(7, read(status, Mob), Align), Mob);
edit(morality, Moral, Mob) ->
    edit(alignment, setelement(1, read(alignment, Mob), Moral), Mob);
edit(chaos, Chaos, Mob) ->
    edit(alignment, setelement(2, read(alignment, Mob), Chaos), Mob);
edit(law, Law, Mob) ->
    edit(alignment, setelement(3, read(alignment, Mob), Law), Mob);
edit(loc, Loc, {Type, {Con, Names, Info, Status, _}}) ->
    {Type, {Con, Names, Info, Status, Loc}};
edit(loc_id, LocID, Mob) ->
    edit(loc, setelement(1, read(loc, Mob), LocID), Mob).

me(State) -> em_lib:entity(State).

%% Interface
check(Attribute, MobPid) -> em_lib:call(MobPid, check, Attribute).

roll(Name, Influences) ->
    lists:foldl(fun(I, M) -> tweak(I, M) end, new(Name), Influences).

reroll(Mob) ->
    Mod = read(ilk, Mob),
    Species = read(species, Mob),
    Sex = read(sex, Mob),
    Homeland = read(sex, Mob),
    Class = read(class, Mob),
    {RollConf, Opts} = proplists:get_value(Species, Mod:species()),
    Influences = RollConf
                 ++ proplists:get_value(Sex, proplists:get_value("sex", Opts))
                 ++ proplists:get_value(Homeland, proplists:get_value("homeland", Opts))
                 ++ proplists:get_value(Class, Mod:class()),
    lists:foldl(fun(I, M) -> tweak(I, M) end, Mob, Influences).

tweak({Attribute, {set, Value}}, Mob) ->
    edit(Attribute, Value, Mob);
tweak({Attribute, {roll, MMM}}, Mob) ->
    edit(Attribute, em_lib:roll(MMM), Mob);
tweak({Attribute, {add, Value}}, Mob) ->
    edit(Attribute, (read(Attribute, Mob) + Value), Mob);
tweak({Attribute, {append, Value}}, Mob) ->
    edit(Attribute, (Value ++ read(Attribute, Mob)), Mob).

topoff(Mob) ->
    MaxHP = read(max_hp, Mob),
    MaxSP = read(max_sp, Mob),
    MaxMP = read(max_mp, Mob),
    edit(health, {{MaxHP, MaxHP}, {MaxSP, MaxSP}, {MaxMP, MaxMP}}, Mob).

%% Startup
start_link(Con, MobData) ->
    spawn_link(fun() -> init(Con, MobData) end).

init(ConPid, MobData) ->
    random:seed(now()),
    note("Initializing with ~p", [MobData]),
    locless(me(MobData), edit(controller, ConPid, MobData)).

locless(Me, MobData) ->
    Oriented = case read(loc, MobData) of
        {none, none} -> edit(loc, mobman:relocate(Me, default), MobData);
        {LocID, _}   -> edit(loc, mobman:relocate(Me, LocID), MobData)
    end,
    enter_world(Oriented).

enter_world(State) ->
    loc:event(read(loc_pid, State),
              {observation, {read(vis, State), {warp, read(name, State), success}}}),
    self() ! {action, {unobservable, {look, []}}},
    loop(State).

%% Life!
loop(State) ->
    {ConPid, ConRef} = read(controller, State),
  receive
    {From, Ref, {check, Attribute}} ->
        From ! {Ref, assess(Attribute, State)},
        loop(State);
    {observation, {Magnitude, Event}} ->
        Ilk = read(ilk, State),
        NewState = Ilk:observe(Magnitude, Event, State),
        loop(NewState);
    {action, {Nature, Data}} ->
        Ilk = read(ilk, State),
        NewState = Ilk:evaluate(Nature, Data, State),
        loop(NewState);
    {From, Ref, {incoming, Event}} ->
        Ilk = read(ilk, State),
        {Result, NewState} = Ilk:react(Event, State),
        From ! {Ref, Result},
        loop(NewState);
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

retire(State) ->
    LocPid = read(loc_pid, State),
    Name = read(name, State),
    loc:event(LocPid, {observation, {read(vis, State), {poof, Name, success}}}),
    ok = loc:drop(LocPid, me(State)),
    ok = charman:save({Name, State}).

assess(weight, State) ->
    read(total_weight, State);
assess(self, State) ->
    me(State);
assess(Attribute, State) ->
    read(Attribute, State).

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

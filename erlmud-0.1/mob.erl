-module(mob).
-export([start_link/2, code_change/1,
         new/5, read/2, edit/3,
         state/1, check_condition/1, check_weight/1, incoming/2, get_actions/2]).

%% Type interface
new(Name, Species, Class, Homeland, Sex) ->
    DOB = calendar:universal_time(),
    {Ilk, Aliases, Description} = resolve_species(Species, Sex),
    {{Height, Weight}, {BaseHP, BaseSP, BaseMP}, Stats} =
        Ilk:physique(Species, Homeland, Sex),
    Alignment = {0, 0, 0}, % {Morality, Chaos, Law}
    Condition = {{BaseHP, BaseHP}, {BaseSP, BaseSP}, {BaseMP, BaseMP}},
    Worn = [],
    WornWeight = em_lib:calc_weight(Worn),
    Held = [],
    HeldWeight = em_lib:calc_weight(Held),
    Inventory = {{WornWeight, Worn}, {HeldWeight, Held}},
    Effects = [],
    PassiveSkills = [],
    ActiveSkills = [],
    Skills = {PassiveSkills, ActiveSkills},
    Level = 1,
    Exp = 1,
    Score = {Level, Exp},
    Location = Ilk:capital(Homeland),
    {{?MODULE,0},
     {{none, none},
      {Name, Aliases},
      {Ilk, Species, Class, Homeland, DOB, Height, Weight, Sex, Stats},
      {Description, Condition, Inventory, Effects, Skills, Score, Alignment},
      {Location, none}}}.

read(con, {_, {Con, _, _, _, _}}) -> Con;
read(con_pid, Mob) -> element(1, read(con, Mob));
read(con_ref, Mob) -> element(2, read(con, Mob));

read(names, {_, {_, Names, _, _, _}}) -> Names;
read(name, Mob)    -> element(1, read(names, Mob));
read(aliases, Mob) -> element(2, read(names, Mob));

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

read(carried, Mob)     -> element(2, read(inventory, Mob));
read(held_weight, Mob) -> element(1, read(carried, Mob));
read(held, Mob)        -> element(2, read(carried, Mob));

read(passive_skills, Mob) -> element(1, read(skills, Mob));
read(active_skills, Mob)  -> element(2, read(skills, Mob));

read(level, Mob) -> element(1, read(score, Mob));
read(exp, Mob)   -> element(2, read(score, Mob));

read(morality, Mob) -> element(1, read(alignment, Mob));
read(chaos, Mob)    -> element(2, read(alignment, Mob));
read(law, Mob)      -> element(3, read(alignment, Mob));

read(hp, Mob)     -> element(1, read(condition, Mob));
read(cur_hp, Mob) -> element(1, read(hp, Mob));
read(max_hp, Mob) -> element(2, read(hp, Mob));

read(sp, Mob)     -> element(2, read(condition, Mob));
read(cur_sp, Mob) -> element(1, read(sp, Mob));
read(max_sp, Mob) -> element(2, read(sp, Mob));

read(mp, Mob)     -> element(3, read(condition, Mob));
read(cur_mp, Mob) -> element(1, read(mp, Mob));
read(max_mp, Mob) -> element(2, read(mp, Mob));

read(total_weight, Mob) ->
    read(weight, Mob) + read(worn_weight, Mob) + read(held_weight, Mob);

read(loc, {_, {_, _, _, _, Loc}}) -> Loc;
read(loc_id, Mob)  -> element(1, read(loc, Mob));
read(loc_pid, Mob) -> element(2, read(loc, Mob)).

edit(con, ConPid, {Type, {_, Names, Info, Status, Loc}}) ->
    ConRef = monitor(process, ConPid),
    {Type, {{ConPid, ConRef}, Names, Info, Status, Loc}};
edit(names, Names, {Type, {Con, _, Info, Status, Loc}}) ->
    {Type, {Con, Names, Info, Status, Loc}};
edit(name, Name, Mob) ->
    edit(names, {Name, read(aliases, Mob)}, Mob);
edit(info, Info, {Type, {Con, Names, _, Status, Loc}}) ->
    {Type, {Con, Names, Info, Status, Loc}};
edit(stats, Stats, Mob) ->
    {Ilk, Spec, Cl, HL, DOB, Height, Weight, Sex, _} = read(info, Mob),
    edit(info, {Ilk, Spec, Cl, HL, DOB, Height, Weight, Sex, Stats}, Mob);
edit(status, Status, {Type, {Con, Names, Info, _, Loc}}) ->
    {Type, {Con, Names, Info, Status, Loc}};
edit(description, Desc, Mob) ->
    {_, Condition, Inv, Eff, Sk, Sc, Align} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Sc, Align}, Mob);
edit(condition, Condition, Mob) ->
    {Desc, _, Inv, Eff, Sk, Sc, Align} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Sc, Align}, Mob);
edit(hp, HP, Mob) ->
    {_, SP, MP} = read(condition, Mob),
    edit(condition, {HP, SP, MP}, Mob);
edit(cur_hp, CurHP, Mob) ->
    edit(hp, {CurHP, read(max_hp, Mob)}, Mob);
edit(max_hp, MaxHP, Mob) ->
    edit(hp, {read(cur_hp, Mob), MaxHP}, Mob);
edit(sp, SP, Mob) ->
    {HP, _, MP} = read(condition, Mob),
    edit(condition, {HP, SP, MP}, Mob);
edit(cur_sp, CurSP, Mob) ->
    edit(sp, {CurSP, read(max_sp, Mob)}, Mob);
edit(max_sp, MaxSP, Mob) ->
    edit(sp, {read(cur_sp, Mob), MaxSP}, Mob);
edit(mp, MP, Mob) ->
    {HP, SP, _} = read(condition, Mob),
    edit(condition, {HP, SP, MP}, Mob);
edit(cur_mp, CurMP, Mob) ->
    edit(mp, {CurMP, read(max_hp, Mob)}, Mob);
edit(max_mp, MaxMP, Mob) ->
    edit(mp, {read(cur_mp, Mob), MaxMP}, Mob);
edit(inventory, Inv, Mob) ->
    {Desc, Condition, _, Eff, Sk, Sc, Align} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Sc, Align}, Mob);
edit(worn, Worn, Mob) ->
    edit(inventory, {{em_lib:calc_weight(Worn), Worn}, read(carried, Mob)}, Mob);
edit(held, Held, Mob) ->
    edit(inventory, {read(equip, Mob), {em_lib:calc_weight(Held), Held}}, Mob);
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
    {Desc, Condition, Inv, Eff, Sk, Sc, _} = read(status, Mob),
    edit(status, {Desc, Condition, Inv, Eff, Sk, Sc, Align}, Mob);
edit(loc, Loc, {Type, {Con, Names, Info, Status, _}}) ->
    {Type, {Con, Names, Info, Status, Loc}}.

%% Interface
state(MPid) ->
    em_lib:call(MPid, state).

check_condition(MobPid) ->
    em_lib:call(MobPid, check_condition).

check_weight(MobPid) ->
    em_lib:call(MobPid, check_weight).

incoming(MobPid, Event) ->
    em_lib:call(MobPid, incoming, Event).

get_actions(MobPid, Form) ->
    em_lib:call(MobPid, actions, Form).

%% Startup
start_link(Con, MobData) ->
    spawn_link(fun() -> init(Con, MobData) end).

init(ConPid, MobData) ->
    note("Initializing with ~p", [MobData]),
    Me = {read(name, MobData), self(), read(aliases, MobData), ?MODULE, read(ilk, MobData)},
    locless(Me, edit(con, ConPid, MobData)).

locless(Me, MobData) ->
    Oriented = case read(loc, MobData) of
        {none, none} -> edit(loc, mobman:relocate(Me, default), MobData);
        {LocID, _}   -> edit(loc, mobman:relocate(Me, LocID), MobData)
    end,
    enter_world(Oriented).

enter_world(State) ->
    loc:event(read(loc_pid, State),
              {observation, {10000, {warp, read(name, State), success}}}),
    self() ! {action, {unobservable, {look, []}}},
    loop(State).

loop(State) ->
    {ConPid, ConRef} = read(con, State),
  receive
    {From, Ref, check_condition} ->
        From ! {Ref, read(condition, State)},
        loop(State);
    {From, Ref, check_weight} ->
        From ! {Ref, {ok, read(weight, State)}},
        loop(State);
    {From, Ref, state} ->
        From ! {Ref, State},
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
    {From, Ref, {actions, Form}} ->
        Ilk = read(ilk, State),
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
resolve_species("human", "male") ->
    {humanoid, ["human", "man"], "A man"};
resolve_species("human", "female") ->
    {humanoid, ["human", "woman"], "A woman"}.

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
    Aliases = read(aliases, State),
    loc:event(LocPid, {observation, {10000, {poof, Name, success}}}),
    ok = loc:drop(LocPid, {Name, self(), Aliases, ?MODULE, read(ilk, State)}),
    ok = charman:save({Name, State}).

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

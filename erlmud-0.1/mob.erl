-module(mob).
-export([start_link/2, code_change/1,
         new/1, new/2, read/2, edit/3, me/1,
         state/1, check_condition/1, check_weight/1,
         roll/2, reroll/2, shift/2, adjust/2, topoff/1]).

%% Type interface
new(Name) ->
    DOB = calendar:universal_time(),
    {Ilk, Aliases, Description} = {none, [], ""},
    {Species, Class, Homeland, Sex} = {"mob", "mob", "mob", "mob"},
    {{Height, Weight}, {BaseHP, BaseSP, BaseMP}, Stats} =
        {{1, 1}, {1, 1, 1}, {1, 1, 1, 1, 1, 1}},
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
    Location = {0, 0, 0},
    {{?MODULE,0},
     {{none, none},
      {Name, Aliases},
      {Ilk, Species, Class, Homeland, DOB, Height, Weight, Sex, Stats},
      {Description, Condition, Inventory, Effects, Skills, Score, Alignment},
      {Location, none}}}.

new(Name, Data) ->
    adjust(mob:new(Name), Data).

read(controller, {_, {Con, _, _, _, _}}) -> Con;
read(con_pid, Mob) -> element(1, read(controller, Mob));
read(con_ref, Mob) -> element(2, read(controller, Mob));

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

me(State) ->
    Name = read(name, State),
    {self(), [Name | read(aliases, State)], {Name, mob, visibility(State)}}.

%% Interface
state(MPid) ->
    em_lib:call(MPid, state).

check_condition(MobPid) ->
    em_lib:call(MobPid, check_condition).

check_weight(MobPid) ->
    em_lib:call(MobPid, check_weight).

roll(Name, Influences) ->
    new(Name, resolve(Influences)).

reroll(Mob, Influences) ->
    adjust(resolve(Influences), Mob).

shift(Mob, Influences) ->
    tweak(Mob, resolve(Influences)).

tweak(Mob, []) ->
    Mob;
tweak(Mob, [{aliases, List} | Influences]) ->
    tweak(edit(aliases, lists:append(List, read(aliases, Mob)), Mob), Influences);
tweak(Mob, [{Key, Magnitude} | Influences]) ->
    tweak(edit(Key, (read(Key, Mob) + Magnitude), Mob), Influences).

resolve(Influences) ->
    resolve([], lists:flatten(Influences)).

resolve(Data, []) ->
    Data;
resolve(Data, [{Key, {Min, Mean, Max}} | Influences]) ->
    resolve([{Key, em_lib:roll(Min, Mean, Max)} | Data], Influences);
resolve(Data, [I | Influences]) ->
    resolve([I | Data], Influences).

adjust(Mob, []) ->
    Mob;
adjust(Mob, [{aliases, List} | Influences]) ->
    adjust(edit(aliases, lists:append(List, read(aliases, Mob)), Mob), Influences);
adjust(Mob, [{Key, Value} | Data]) ->
    adjust(edit(Key, Value, Mob), Data).

topoff(Mob) ->
    MaxHP = read(max_hp, Mob),
    MaxSP = read(max_sp, Mob),
    MaxMP = read(max_mp, Mob),
    edit(condition, {{MaxHP, MaxHP}, {MaxSP, MaxSP}, {MaxMP, MaxMP}}, Mob).

%% Startup
start_link(Con, MobData) ->
    spawn_link(fun() -> init(Con, MobData) end).

init(ConPid, MobData) ->
    note("Initializing with ~p", [MobData]),
%   Me = {self(), [read(name, MobData) | read(aliases, MobData)], {}},
    locless(me(MobData), edit(controller, ConPid, MobData)).

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
    {ConPid, ConRef} = read(controller, State),
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
    loc:event(LocPid, {observation, {10000, {poof, Name, success}}}),
    ok = loc:drop(LocPid, me(State)),
    ok = charman:save({Name, State}).

visibility(_) -> 1000.

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

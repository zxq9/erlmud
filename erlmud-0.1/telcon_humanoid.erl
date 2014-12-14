-module(telcon_humanoid).
-export([observe/2, prompt/1, actions/0, alias/0, solicit_char/4]).

%% Semantic event -> text translation
observe(Event, Minion) ->
    case Event of
        {look, self, View} ->
            render_location(View, Minion);
        {{say, Line}, self, success} ->
            "You say,\"" ++ Line ++ "\"";
        {{say, Line}, Speaker, success} ->
            Speaker ++ " says,\"" ++ Line ++ "\"";
        {status, self, MobState} ->
            render_status(MobState);
        {{arrive, Direction}, self, success} ->
            "You arrive from " ++ Direction ++ ".";
        {{arrive, Direction}, Actor, success} ->
            Actor ++ " arrives from " ++ Direction;
        {{depart, _}, self, failure} ->
            "You can't manage to leave!";
        {{depart, Direction}, Actor, failure} ->
            Actor ++ " tried to go " ++ Direction ++ ", and failed.";
        {{depart, Direction}, Actor, success} ->
            Actor ++ " departs " ++ Direction;
        {{take, ObjName}, self, success} ->
            "You get a " ++ ObjName;
        {{take, ObjName}, self, failure} ->
            "You can't take the " ++ ObjName ++ ".";
        {{take, ObjName}, Actor, success} ->
            Actor ++ " gets a " ++ ObjName;
        {{glance, self}, self, _} ->
            "Feeling a bit vain today?";
        {{glance, Actor}, Actor, _} ->
            Actor ++ " dreams of greatness.";
        {{glance, _}, self, failure} ->
            "That isn't here.";
        {{glance, self}, Actor, success} ->
            Actor ++ " glances at you.";
        {{glance, _}, self, success} ->
            silent;
        {{glance, _}, self, View} ->
            render_glance(View);
        {{glance, Target}, Actor, success} ->
            Actor ++ " glances at " ++ Target;
        {warp, self, _} ->
            "You suddenly find yourself, existing.";
        {warp, Actor, _} ->
            "A quantum fluctuation suddenly manifests " ++ Actor ++ " nearby.";
        {poof, Actor, _} ->
            Actor ++ " disappears in a puff of smoke!";
        {Action, Actor, Outcome} ->
            note("Observed: ~p ~p ~p", [Action, Actor, Outcome]),
            silent
    end.

render_location({Name, Description, Inventory, Exits},
                {_, MPid, _, _}) ->
    ExitNames = string:join([N || {N, _, _, _} <- Exits], " "),
    Stuff = string:join(render_inventory(MPid, Inventory), "\r\n"),
    io_lib:format(telcon:cyan("~ts\r\n") ++
                  telcon:gray("~ts\r\n[ obvious exits:") ++
                  telcon:white(" ~ts ") ++
                  telcon:gray("]\r\n") ++
                  telcon:green("~ts"),
                  [Name, Description, ExitNames, Stuff]).

render_inventory(MPid, Inventory) ->
    render_inventory(MPid, Inventory, []).

render_inventory(_, [], Stuff) ->
    Stuff;
render_inventory(MPid, [{MPid, _, _} | Inv], Stuff) ->
    render_inventory(MPid, Inv, Stuff);
render_inventory(MPid, [{_, _, {Name, mob, _}} | Inv], Stuff) ->
    render_inventory(MPid, Inv, [io_lib:format("~ts is standing here.", [Name]) | Stuff]);
render_inventory(MPid, [{_, _, {Name, obj, _}} | Inv], Stuff) ->
    render_inventory(MPid, Inv, [io_lib:format("~ts is here.", [Name]) | Stuff]).

render_status(Mob) ->
    {Str, Int, Wil, Dex, Con, Speed} = mob:read(stats, Mob),
    {Moral, Chaos, Law} = mob:read(alignment, Mob),
    {Level, Exp} = mob:read(score, Mob),
    {{CurHP, MaxHP}, {CurSP, MaxSP}, {CurMP, MaxMP},
     Vis, OB, PB, DB, Abs} = mob:read(condition, Mob),
    io_lib:format("You are ~s, a ~s ~s ~s from ~s.\r\n"
                  "You are wearing ~p and carrying ~p.\r\n"
                  "STR: ~p INT: ~p WIL: ~p DEX: ~p CON: ~p SPD: ~p\r\n"
                  "Morality: ~p  Chaos: ~p Lawfulness: ~p\r\n"
                  "Level:  ~p Experience: ~p\r\n"
                  "Health: (~p/~p) Stamina: (~p/~p) Magika (~p/~p)\r\n"
                  "Vis: ~w OB: ~w PB: ~w DB: ~w Abs: ~w%",
                  [mob:read(name, Mob), mob:read(sex, Mob),
                   mob:read(species, Mob), mob:read(class, Mob), mob:read(homeland, Mob),
                   mob:read(worn_weight, Mob), mob:read(held_weight, Mob),
                   Str, Int, Wil, Dex, Con, Speed,
                   Moral, Chaos, Law,
                   Level, Exp,
                   CurHP, MaxHP, CurSP, MaxSP, CurMP, MaxMP,
                   Vis, OB, PB, DB, Abs]).

render_glance({{_, Species, Class, Homeland, _, _, _, _, _},
               {Desc, {HP, _, _}, {{_, Equip}, {_, Inv}}, _, _, _, _}}) ->
    io_lib:format("You see a ~ts ~ts from ~ts.\r\n"
                  "~ts\r\n"
                  "Wearing: ~tp\r\n"
                  "Carrying: ~tp\r\n"
                  "Health: ~ts",
                  [Species, Class, Homeland, Desc, Equip, Inv, health(HP)]);
render_glance({obj, Name, Description}) ->
    io_lib:format("You look at the ~ts and see: ~ts", [Name, Description]).

prompt(Pid) ->
    {HP, SP, MP} = mob:check(health, Pid),
    io_lib:format("(~ts, ~ts, ~ts) $ ", [health(HP), stamina(SP), magika(MP)]).

health(HP) ->
    case bracket(HP) of
        5 -> "Healthy";
        4 -> "Scratched";
        3 -> "Hurt";
        2 -> "Wounded";
        1 -> "Beaten";
        0 -> "Critical"
    end.

stamina(SP) ->
    case bracket(SP) of
        5 -> "Fresh";
        4 -> "Strong";
        3 -> "Tiring";
        2 -> "Winded";
        1 -> "Haggard";
        0 -> "Bonked"
    end.

magika(MP) ->
    case bracket(MP) of
        5 -> "Enflow";
        4 -> "Focused";
        3 -> "Distracted";
        2 -> "Headachy";
        1 -> "Migrane";
        0 -> "Zonked"
    end.

bracket({Current, Max}) ->
    Z = 5,
    (Current * Z) div ((Max * Z) div Z).

actions() ->
    [{"go", go, observable,
      "go Exit", "Move to a new location through Exit"},
     {"say", say, observable,
      "say Text", "Say something out loud"},
     {"status", status, unobservable,
      "status", "Check your character's current status"},
     {"look", look, unobservable,
      "look", "View your surroundings"},
     {"glance", glance, observable,
      "glance Target", "Look at Target"},
     {"take", take, observable,
      "take Target", "Get somehing from the ground."}].

alias() ->
    [{"n", "go north"},
     {"s", "go south"},
     {"e", "go east"},
     {"w", "go west"},
     {"d", "go down"},
     {"u", "go up"},
     {"l", "look"},
     {"k", "kill"},
     {"8", "go north"},
     {"2", "go south"},
     {"6", "go east"},
     {"4", "go west"},
     {"3", "go down"},
     {"9", "go up"},
     {"5", "look"},
     {"7", "status"},
     {"55", "kill"},
     {"st", "status"},
     {"stat", "status"},
     {"north", "go north"},
     {"south", "go south"},
     {"east", "go east"},
     {"west", "go west"},
     {"down", "go down"},
     {"up", "go up"}].

solicit_char(Name, Species, Picker, State) ->
    Detail = proplists:get_value(Species, mob_humanoid:species()),
    {Sex, Morph} = Picker("Sex?", proplists:get_value("sex", Detail), State),
    {Homeland, HI} = Picker("Homeland?", proplists:get_value("homeland", Detail), State),
    {_, LocID} = Picker("Starting Location?", proplists:get_value("loc", Detail), State),
    {Class, CI} = Picker("Class?", mob_humanoid:class(), State),
    PersData = [{ilk, mob_humanoid}, {species, "human"},
                {sex, Sex}, {homeland, Homeland}, {loc_id, LocID}, {class, Class}],
    Base = mob:roll(Name, Morph),
    Persona = mob:adjust(Base, PersData),
    Mob = mob:shift(Persona, [HI, CI]),
    mob:topoff(Mob).

%% System
note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

-module(telcon_humanoid).
-export([observe/2, prompt/1, perform/4, help/0, alias/0]).

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
        {inventory, self, InvList} ->
            render_inventory(InvList);
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
        {{look, _}, self, failure} ->
            "That isn't here.";
        {{look, self}, Actor, success} ->
            Actor ++ " looks at you.";
        {{look, _}, self, success} ->
            silent;
        {{look, _}, self, View} ->
            render_look(View);
        {{look, Target}, Actor, success} ->
            Actor ++ " looks at " ++ Target;
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
                {_, _, MPid, _, _}) ->
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
render_inventory(MPid, [{_, _, {Name, mob, _, _}} | Inv], Stuff) ->
    render_inventory(MPid, Inv, [io_lib:format("~ts is standing here.", [Name]) | Stuff]);
render_inventory(MPid, [{_, _, {Name, obj, _, _}} | Inv], Stuff) ->
    render_inventory(MPid, Inv, [io_lib:format("~ts is here.", [Name]) | Stuff]).

render_status(Mob) ->
    {Str, Int, Wil, Dex, Con, Speed} = mob:read(stats, Mob),
    {Moral, Chaos, Law} = mob:read(alignment, Mob),
    {Level, Exp} = mob:read(score, Mob),
    {{{CurHP, MaxHP}, {CurSP, MaxSP}, {CurMP, MaxMP}},
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

render_inventory([]) ->
    "You aren't holding anything.";
render_inventory(InvList) ->
    "You are carrying:\r\n  " ++
    string:join(lists:reverse(lists:foldl(fun render_entity/2, [], InvList)), "\r\n  ").

render_look({Species, Class, Homeland, Desc, HP, Equip, Inv}) ->
    io_lib:format("You see a ~ts ~ts from ~ts.\r\n"
                  "~ts\r\n"
                  "Wearing: ~tp\r\n"
                  "Carrying: ~tp\r\n"
                  "Appears to be ~ts",
                  [Species, Class, Homeland, Desc, Equip, Inv, health(HP)]);
render_look({obj, Name, Description}) ->
    io_lib:format("You look at the ~ts and see: ~ts", [Name, Description]).

render_entity({_, _, {Name, _, _, _}}) ->
    io_lib:format("~ts", [Name]).

render_entity(Entity, Acc) ->
    [render_entity(Entity) | Acc].

prompt(Pid) ->
    {HP, SP, MP} = mob:check(health, Pid),
    io_lib:format("(~ts, ~ts, ~ts) $ ", [health(HP), stamina(SP), magika(MP)]).

health({Current, Max}) ->
    Ratings = ["critical", "beaten", "wounded", "hurt", "scratched", "healthy"],
    rate(Current, Max, Ratings).

stamina({Current, Max}) ->
    Ratings = ["bonked", "haggard", "winded", "tired", "strong", "fresh"],
    rate(Current, Max, Ratings).

magika({Current, Max}) ->
    Ratings = ["zonked", "migrane", "headachy", "distracted", "focused", "enflow"],
    rate(Current, Max, Ratings).

rate(Index, Range, Ratings) ->
    lists:nth(em_lib:bracket(Index, Range, length(Ratings)), Ratings).

perform(Keyword, Data, Name, MPid) ->
    case do(Keyword, Data, Name) of
        {none, Message} ->
            {ok, Message};
        {ToDo, Message} ->
            MPid ! {action, ToDo},
            {ok, Message};
        bargle ->
            bargle
    end.

do("go", "", _) ->
    {none, "Go which way?"};
do("go", String, _) ->
    {ok, Target} = parse(single, String),
    {{go, Target}, none};

do("say", "", _) ->
    {{say, "..."}, none};
do("say", String, _) ->
    {{say, String}, none};

do("status", _, _) ->
    {{status, self}, none};

do("look", "", _) ->
    {{look, loc}, none};
do("look", Name, Name) ->
    {none, "Am I beautiful? Yes. Yes, I am beautiful."};
do("look", String, _) ->
    {ok, Target} = parse(single, String),
    {{look, Target}, none};

do("take", "", _) ->
    {none, "Take what?"};
do("take", Name, Name) ->
    {none, "Not in public! What's wrong with you..."};
do("take", String, _) ->
    {ok, Target} = parse(multiple, String),
    {{take, Target}, none};

do("inventory", _, _) ->
    {{inventory, self}, none};

do("equipment", _, _) ->
    {{equipment, self}, none};

do(_, _, _) ->
    bargle.

help() ->
    telcon:white("  Mob commands:\r\n") ++
    telcon:gray( "    go Exit                - Move to a new location through Exit\r\n"
                 "    say Text               - Say something out loud\r\n"
                 "    status                 - Check your character's current status\r\n"
                 "    look                   - View your surroundings\r\n"
                 "    look Target            - Look at a target\r\n"
                 "    take Target            - Get something from the ground\r\n"
                 "    inventory              - Check your carried inventory\r\n"
                 "    equipment              - Check your equipped items\r\n").

alias() ->
    [{"n", "go north"},
     {"s", "go south"},
     {"e", "go east"},
     {"w", "go west"},
     {"d", "go down"},
     {"u", "go up"},
     {"l", "look"},
     {"k", "kill"},
     {"inv", "inventory"},
     {"eq", "equipment"},
     {"equip", "equipment"},
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

%% Binary & String handling
parse(_, String) ->
    {Target, _} = head(String),
    {ok, Target}.

head(Line) ->
    Stripped = string:strip(Line),
    case head([], Stripped) of
        Z = {{_, _}, _} -> Z;
        {Head, Tail}   -> {lists:reverse(Head), Tail}
    end.

head(Word, []) ->
    {Word, []};
head([], [$\s|T]) ->
    head([], T);
head(Word, [H|T]) ->
    case H of
        $\s ->
            {Word, T};
%       $. when is_number(Word) ->
        $. ->
            Index = list_to_integer(lists:reverse(Word)),
            {Target, Rest} = head(T),
            {{Index, Target}, Rest};
        Z ->
            head([Z|Word], T)
    end.

%% System
note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

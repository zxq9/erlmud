-module(telcon_humanoid).
-export([observe/1, prompt/1, actions/0]).

%% Semantic event -> text translation
observe(Event) ->
    case Event of
        {look, self, View} ->
            io_lib:format("You see: ~p", [View]);
        {{say, Line}, self, success} ->
            "You say,\"" ++ Line ++ "\"";
        {{say, Line}, Speaker, success} ->
            Speaker ++ " says,\"" ++ Line ++ "\"";
        {status, self, MobState} ->
            render_status(MobState);
        {{arrive, Direction}, self, success} ->
            "You arrive from the " ++ Direction ++ ".";
        {{arrive, Direction}, Actor, success} ->
            Actor ++ " arrives from the " ++ Direction;
        {{depart, Direction}, Actor, success} ->
            Actor ++ " departs to the " ++ Direction;
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
            io_lib:format("~p", [View]);
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

render_status(Mob) ->
    {Str, Int, Wil, Dex, Con, Speed} = mob:read(stats, Mob),
    {Moral, Chaos, Law} = mob:read(alignment, Mob),
    {Level, Exp} = mob:read(score, Mob),
    {{CurHP, MaxHP}, {CurSP, MaxSP}, {CurMP, MaxMP}} = mob:read(condition, Mob),
    io_lib:format("You are ~s, a ~s ~s ~s.\r\n"
                  "You are wearing ~p and carrying ~p.\r\n"
                  "Stats     - STR: ~p INT: ~p WIL: ~p DEX: ~p CON: ~p SPD: ~p\r\n"
                  "Alignment - Morality: ~p  Chaos: ~p Lawfulness: ~p\r\n"
                  "Level:  ~p Experience: ~p\r\n"
                  "Health: (~p/~p) Stamina: (~p/~p) Magika (~p/~p)",
                  [mob:read(name, Mob), mob:read(sex, Mob),
                   mob:read(ilk, Mob), mob:read(class, Mob),
                   mob:read(worn_weight, Mob), mob:read(held_weight, Mob),
                   Str, Int, Wil, Dex, Con, Speed,
                   Moral, Chaos, Law,
                   Level, Exp,
                   CurHP, MaxHP, CurSP, MaxSP, CurMP, MaxMP]).

prompt(Pid) ->
    {{CurHP, MaxHP}, {CurSP, MaxSP}, _} = mob:check_condition(Pid),
    io_lib:format("(~s, ~s) $ ", [health(CurHP, MaxHP), stamina(CurSP, MaxSP)]).

health(CurHP, MaxHP) ->
    case (CurHP div (MaxHP div 5)) of
        5 -> "Healthy";
        4 -> "Scratched";
        3 -> "Bloodied";
        2 -> "Hurt";
        1 -> "Wounded";
        0 -> "Critical"
    end.

stamina(CurSP, MaxSP) ->
    case (CurSP div (MaxSP div 5)) of
        5 -> "Fresh";
        4 -> "Strong";
        3 -> "Tiring";
        2 -> "Winded";
        1 -> "Haggard";
        0 -> "Bonked"
    end.

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
      "glance Target", "Look at Target"}].

%% System
%note(String) ->
%    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

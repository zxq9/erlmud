-module(obj).
-export([start_link/2, code_change/1,
         new/1, read/2, edit/3,
         check/2]).

%% Type Interface
new(ObjID = {Category, Type, Name}) ->
    Vis = 10000,
    {Aliases, Weight, Description} = resolve(ObjID),
    {{?MODULE, 0},
     {{Name, Aliases},
      {Category, Type, Weight, Description, none},
      {Vis},
      none}}.

read(mod, {Mod, _}) -> Mod;
read(module, Obj)  -> element(1, read(mod, Obj));
read(version, Obj) -> element(2, read(mod, Obj));

read(names, {_, {Names, _, _, _}}) -> Names;
read(name, Obj)    -> element(1, read(names, Obj));
read(aliases, Obj) -> element(2, read(names, Obj));
read(id, Obj)      -> read(name, Obj);

read(info, {_, {_, Info, _, _}}) -> Info;
read(category, Obj)    -> element(1, read(info, Obj));
read(type, Obj)        -> element(2, read(info, Obj));
read(weight, Obj)      -> element(3, read(info, Obj));
read(description, Obj) -> element(4, read(info, Obj));
read(stats, Obj)       -> element(5, read(info, Obj));

read(status, {_, {_, _, Status, _}}) -> Status;
read(vis, Obj) -> element(1, read(status, Obj));

read(pos_pid, {_, {_, _, _, PosPid}}) -> PosPid;

read(total_weight, Obj) -> read(weight, Obj).

edit(names, Names, {Mod, {_, Info, Status, Pos}}) ->
    {Mod, {Names, Info, Status, Pos}};
edit(name, Name, Obj) ->
    edit(names, setelement(1, read(names, Obj), Name), Obj);
edit(aliases, Aliases, Obj) ->
    edit(aliases, setelement(2, read(names, Obj), Aliases), Obj);
edit(info, Info, {Mod, {Names, _, Status, Pos}}) ->
    {Mod, {Names, Info, Status, Pos}};
edit(category, Cat, Obj) ->
    edit(info, setelement(1, read(info, Obj), Cat), Obj);
edit(type, Type, Obj) ->
    edit(info, setelement(2, read(info, Obj), Type), Obj);
edit(weight, Weight, Obj) ->
    edit(info, setelement(3, read(info, Obj), Weight), Obj);
edit(desc, Desc, Obj) ->
    edit(info, setelement(4, read(info, Obj), Desc), Obj);
edit(effects, Effects, Obj) ->
    edit(info, setelement(5, read(info, Obj), Effects), Obj);
edit(status, Status, {Mod, {Names, Info, _, Pos}}) ->
    {Mod, {Names, Info, Status, Pos}};
edit(pos_pid, Pos, {Mod, {Names, Info, Status, _}}) ->
    {Mod, {Names, Info, Status, Pos}}.

%% Interface
check(Attribute, ObjPid) -> em_lib:call(ObjPid, check, Attribute).

%% Startup
start_link(PosPid, ObjData) ->
    spawn_link(fun() -> init(PosPid, ObjData) end).

init(PosPid, ObjData) ->
    random:seed(now()),
    link(PosPid),
    note("Instantiating object ~p at ~p", [ObjData, PosPid]),
    State = edit(pos_pid, PosPid, ObjData),
    ok = em_lib:call(PosPid, load, em_lib:entity(State)),
    Event = {observation, {read(vis, State), {warp, read(description, State), success}}},
    PosPid ! {event, Event},
    loop(State).

%% Sweet existence!
loop(State) ->
  receive
    {From, Ref, {check, Attribute}} ->
        From ! {Ref, assess(Attribute, State)},
        loop(State);
    {From, Ref, {move, PosPid}} ->
        NewState = edit(pos_pid, PosPid, State),
        From ! {Ref, em_lib:entity(NewState)},
        loop(NewState);
    {From, Ref,{incoming, {look, _}}} ->
        From ! {Ref, {ok, {obj, read(name, State), read(description, State)}}},
        loop(State);
    {From, Ref, {incoming, _Event}} ->
        From ! {Ref, {ok, "You got me"}},
        loop(State);
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
assess(weight, State) ->
    read(total_weight, State);
assess(self, State) ->
    em_lib:entity(State);
assess(Attribute, State) ->
    read(Attribute, State).

resolve(_) -> {["stick", "wood"], 1200, "A small stick of wood"}.

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

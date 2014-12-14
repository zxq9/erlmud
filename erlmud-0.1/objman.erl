-module(objman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/1,
         load/2,
         type_index/0]).

%% interface
load(ObjData, PosPid) -> call(load_obj, {ObjData, PosPid}).

type_index() -> call(types).

call(Request, Data) -> call({Request, Data}).

call(Request) -> em_lib:call(?MODULE, Request).

%% Startup
start(Parent)            -> start(Parent, []).
start(Parent, Conf)      -> starter(fun spawn/1, Parent, Conf).
start_link(Parent)       -> start_link(Parent, []).
start_link(Parent, Conf) -> starter(fun spawn_link/1, Parent, Conf).

starter(Spawn, Parent, Conf) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = Spawn(fun() -> init(Parent, Conf) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init(Parent, Conf) ->
    random:seed(now()),
    process_flag(trap_exit, true),
    note("Initializing with ~p.", [Conf]),
    Types = proplists:get_value(types, Conf),
    Extant = [],
    loop({Parent, Extant, Types}).

%% Service
loop(State = {Parent, Extant, Types}) ->
  receive
    {Requester, Ref, {load_obj, {ObjData, PosPid}}} ->
        {ObjPid, NewExtant} = load(ObjData, PosPid, Extant),
        Requester ! {Ref, ObjPid},
        loop({Parent, NewExtant, Types});
    {From, Ref, types} ->
        From ! {Ref, Types},
        loop(State);
    {'EXIT', Parent, Reason} ->
        note("Parent ~tp dies with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]);
    Message = {'EXIT', _, _} ->
        NewExtant = handle_exit(Extant, Message),
        loop({Parent, NewExtant, Types});
    status ->
        note("Status:~n  Parent: ~p~n  Extant: ~p~n, Types: ~p",
             [Parent, Extant, Types]),
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

%% Request Calls
load(ObjData = {{Mod, _}, _}, PosPid, Extant) ->
    Category = Mod:read(category, ObjData),
    ObjPid = obj:start_link(PosPid, ObjData),
    NewExtant = [{ObjPid, Category} | Extant],
    {ObjPid, NewExtant}.

%% Magic
handle_exit(Extant, Message = {_, Pid, _}) ->
    case lists:keyfind(Pid, 1, Extant) of
        Obj = {_, _} ->
            note("~p exited with ~p", [Obj, Message]),
            lists:delete(Obj, Extant);
        false ->
            note("Received ~p", [Message]),
            Extant
    end.

%% Code changer
code_change(State = {_, Extant, _}) ->
    note("Changing code."),
    [ObjPid ! code_change || {ObjPid, _} <- Extant],
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

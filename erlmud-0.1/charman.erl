-module(charman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/1,
         save/1, list/1, load/2, make/2, drop/2]).

%% Interface
save(CharState) -> call(save, CharState).

list(Acc) -> call(get_chars, Acc).

load(Acc, Name) -> call(load_char, {Acc, Name}).

make(Acc, Char) -> call(make_char, {Acc, Char}).

drop(Acc, Name) -> call(drop_char, {Acc, Name}).

call(Verb, Data) -> call({Verb, Data}).
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
    note("Initializing with ~p.", [Conf]),
    Accs = load_accounts(),
    Chars = load_characters(),
    loop({Parent, Conf, Accs, Chars}).

load_accounts() -> dict:new().

load_characters() -> dict:new().

%% Service
loop(State = {Parent, Conf, Accs, Chars}) ->
  receive
    {From, Ref, {save, CharState}} ->
        NewChars = remember(CharState, Chars),
        From ! {Ref, ok},
        loop({Parent, Conf, Accs, NewChars});
    {From, Ref, {get_chars, Acc}} ->
        Result = get_chars(Acc, Accs),
        From ! {Ref, Result},
        loop(State);
    {From, Ref, {load_char, {Acc, Name}}} ->
        Result = load_char(Accs, Chars, Acc, Name),
        From ! {Ref, Result},
        loop(State);
    {From, Ref, {make_char, {Acc, Char}}} ->
        {Result, NewState} = make_char(State, Acc, Char),
        From ! {Ref, Result},
        loop(NewState);
    {From, Ref, {drop_char, {Acc, Name}}} ->
        {Result, NewState} = drop_char(State, Acc, Name),
        From ! {Ref, Result},
        loop(NewState);
    {'EXIT', Parent, Reason} ->
        note("Parent~tp died with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]);
    status ->
        note("Status:~n  Conf: ~p~n  Accs: ~p~n  Chars: ~p~n",
             [Conf, dict:to_list(Accs), dict:to_list(Chars)]),
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

%% Registry functions
remember({_, Name, Ilk, Desc, {LocID, _}}, Chars) ->
    dict:store(Name, {Ilk, Desc, LocID}, Chars).

get_chars(Acc, Accs) ->
    case dict:find(Acc, Accs) of
        {ok, Result} -> Result;
        error        -> []
    end.

load_char(Accs, Chars, Acc, Name) ->
    case dict:find(Acc, Accs) of
        {ok, List} ->
            case lists:member(Name, List) of
                true  -> {ok, {Name, dict:fetch(Name, Chars)}};
                false -> {error, Name ++ " is not one of your characters."}
            end;
        error ->
            {error, Name ++ " is not one of your characters."}
    end.

make_char(State = {Parent, Conf, Accs, Chars}, Acc, {Name, Data}) ->
    case dict:is_key(Name, Chars) of
        true  ->
            Response = Name ++ " already exists.",
            {Response, State};
        false ->
            NewAccs = dict:append(Acc, Name, Accs),
            NewChars = dict:store(Name, Data, Chars),
            {ok, {Parent, Conf, NewAccs, NewChars}}
    end.

drop_char(State = {Parent, Conf, Accs, Chars}, Acc, Name) ->
    case dict:find(Acc, Accs) of
        {ok, List} ->
            case lists:member(Name, List) of
                true ->
                    NewAccs = dict:store(Acc, lists:delete(Name, List), Accs),
                    NewChars = dict:erase(Name, Chars),
                    {ok, {Parent, Conf, NewAccs, NewChars}};
                false ->
                    Response = Name ++ " is not one of your characters.",
                    {Response, State}
            end;
        false ->
            Response = Name ++ " is not one of your characters.",
            {Response, State}
    end.

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

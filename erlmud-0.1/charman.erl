-module(charman).
-export([start/1, start/2, start_link/1, start_link/2, code_change/1,
         who/0, list/1, load/2, make/2]).

%% Interface
who() -> {error, not_implemented}.

list(Acc) -> call({get_chars, Acc}).

make(Acc, Char) -> call({make_char, {Acc, Char}}).

load(_Acc, _Name) -> {error, not_implemented}.

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
    note("Notional initialization with ~p.", [Conf]),
    Accs = load_accounts(),
    Chars = load_characters(),
    loop({Parent, Conf, Accs, Chars}).

load_accounts() -> dict:new().

load_characters() -> dict:new().

%% Service
loop(State = {Parent, _Conf, Accs, _Chars}) ->
  receive
    {From, Ref, {get_chars, Acc}} ->
        Result = get_chars(Acc, Accs),
        From ! {Ref, Result},
        loop(State);
    {From, Ref, {make_char, {Acc, Char}}} ->
        {Result, NewState} = make_char(State, Acc, Char),
        From ! {Ref, Result},
        loop(NewState);
    {'EXIT', Parent, Reason} ->
        note("Parent~tp died with ~tp~nFollowing my leige!~n...Blarg!", [Parent, Reason]);
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
get_chars(Acc, Accs) ->
    case dict:find(Acc, Accs) of
        {ok, Result} -> Result;
        error        -> []
    end.

make_char(State = {Parent, Conf, Accs, Chars}, Acc, {Name, Data}) ->
    case dict:is_key(Name, Chars) of
        true  ->
            Response = Name ++ " is already one of your characters.",
            {Response, State};
        false ->
            NewAccs = dict:append(Acc, Name, Accs),
            NewChars = dict:append(Name, Data, Chars),
            {ok, {Parent, Conf, NewAccs, NewChars}}
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

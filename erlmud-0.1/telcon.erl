-module(telcon).
-export([start_link/1, code_change/1]).

start_link(Talker) ->
    spawn_link(fun() -> welcome(Talker) end).

%% Authentication & Registration
welcome(Talker) ->
    Greeting = greet(),
    Talker ! {send, Greeting},
    Handle = receive {received, Bin} -> topline(Bin) end,
    case check(Handle) of
        unregistered   -> register_acc(Talker, Handle);
        registered     -> authenticate(Talker, Handle);
        {fail, Reason} ->
            note("check_registry_for/1 failed with ~p~n", [Reason]),
            Talker ! {send, "Something went wrong, let's try that again...\r\n"},
            welcome(Talker)
    end.

register_acc(Talker, Handle) ->
    Talker ! {send, "\r\nLooks like you're new here.\r\nEnter a passphrase: "},
    P1 = receive {received, P1Bin} -> topline(P1Bin) end,
    Talker ! {send, "Re-enter to confirm: "},
    P2 = receive {received, P2Bin} -> topline(P2Bin) end,
    case string:equal(P1, P2) of
        true  ->
            case create_acc(Handle, P1) of
                ok  ->
                    M = "\r\nWelcome to ErlMUD, " ++ Handle ++ "!\r\n" ++
                        "Enjoy your stay, and don't feed the trolls.\r\n" ++ prompt(Handle),
                    Talker ! {send, M},
                    init(Talker, Handle);
                {error, handle_is_in_use} -> 
                    M = "\r\nSomeone else nabbed your handle!\r\n"
                        "Let's try this again...\r\n",
                    Talker ! {send, M},
                    welcome(Talker);
                {fail, Reason} ->
                    note("create_acc/2 failed with ~p~n", [Reason]),
                    Talker ! {send, "Something went wrong, let's try that again...\r\n"},
                    welcome(Talker)
            end;
        false ->
            Talker ! {send, "OK, for real this time...\r\n"},
            register_acc(Talker, Handle)
     end.

authenticate(Talker, Handle) ->
    Talker ! {send, "Passphrase: "},
    PW = receive {received, Bin} -> topline(Bin) end,
    case verify(Handle, PW) of
        verified ->
            Salutation = "Welcome back, " ++ Handle ++ "!\r\n" ++ prompt(Handle),
            Talker ! {send, Salutation},
            init(Talker, Handle);
        badpass ->
            timer:sleep(7000),
            authenticate(Talker, Handle);
        {fail, Reason} ->
            note("check_password/2 failed with ~p~n", [Reason]),
            Talker ! {send, "Something went wrong, let's try that again...\r\n"},
            authenticate(Talker, Handle)
    end.

init(Talker, Handle) ->
    Minion = none,
    Actions = init_actions(Minion),
    Channels = init_channels(Handle),
    State = {Talker, Handle, {Minion, none, Actions}, Channels},
    loop(State).

init_channels(_) -> dict:new().

init_actions(Minion) ->
    case Minion of
        none  -> dict:new();
        Pid   -> em_lib:call(Pid, get_actions)
    end.

%% Service
loop(State = {Talker, Handle, _Minion = {Pid, Ref, _Actions}, Channels}) ->
  receive
    {received, Bin} ->
        ok = evaluate(Bin, State),
        loop(State);
    {notice, Message} ->
        Talker ! {send, Message ++ "\r\n"};
    {output, Data} ->
        Talker ! {send, Data},
        loop(State);
    {control_minion, Pid} ->
        NewMinion = acquire_minion(Pid),
        loop({Talker, Handle, NewMinion, Channels});
    {'DOWN', Ref, process, Pid, _Reason} ->
        Talker ! {send, "Minion disconnected.\r\n"},
        Actions = init_actions(none),
        loop({Talker, Handle, {none, none, Actions}, Channels});
    code_change ->
        ?MODULE:code_change(State);
    shutdown ->
        note("Shutting down.~n");
    Any ->
        note("Received ~tp~n", [Any]),
        loop(State)
  end.

%% Input evaluation
evaluate(Bin, State = {Talker, Handle, _, _}) ->
    Line = topline(Bin),
    Expansion = rewrite(Line),
    Result = interpret(Expansion, State),
    Reply = case Result of
        none -> prompt(Handle);
        ok   -> "ok" ++ "\r\n" ++ prompt(Handle);
        Any  -> Any ++ "\r\n" ++ prompt(Handle)
    end,
    Talker ! {send, Reply},
    ok.

interpret(Expansion, State = {Talker, Handle, Minion, Channels}) ->
    {Keyword, Line} = head(Expansion),
    Action = case Keyword of
        "chat"  -> {command, fun chat/1, {Handle, Channels, Line}};
        "sys"   -> {command, fun sys/1, {State, Line}};
        "world" -> {command, fun world/1, {State, Line}};
        "echo"  -> {command, fun echo/1, Line};
        "quit"  -> {command, fun quit/1, {Talker, Handle}};
        "help"  -> {command, fun help/1, Minion};
        ""      -> none;
        _       -> {action, Keyword, Line}
    end,
    Result = case Action of
        {command, Fun, Args}   -> Fun(Args);
        {action, Verb, String} -> perform(Verb, String, Minion);
        none                   -> none;
        _                      -> bargle()
    end,
    Result.

perform(Keyword, String, {Pid, _, Actions}) ->
    case Pid of
        none   ->
            bargle();
        Minion ->
            case dict:find(Keyword, Actions) of
                error ->
                    bargle();
                Verb  -> 
                    Args  = string:tokenize(String),
                    Minion ! {action, {Verb, Args}},
                    ok
            end
    end.

%% Binary & string handling
rewrite([]) -> [];
rewrite(Line = [H|T]) ->
    case H of
        $#  -> "chat " ++ T;
        $\\ -> "sys " ++ T;
        $@  -> "world " ++ T;
        $?  -> "help " ++ T;
        _   -> Line
    end.

head(Line) ->
    {Head, Tail} = head([], Line),
    {lists:reverse(Head), Tail}.

head(Word, []) ->
    {Word, []};
head(Word, [H|T]) ->
    case H of
        $\s -> {Word, T};
        Z   -> head([Z|Word], T)
    end.

topline(Bin) ->
    case stringify(Bin) of
        [Top | _] -> Top;
        []        -> ""
    end.

stringify(Bin) -> string:tokens(binary_to_list(Bin), "\r\n").

%% Controller actions
echo(String) -> String.

bargle() -> "Arglebargle, glop-glyf!?!".

quit({Talker, Handle}) ->
    Message = "Goodbye, " ++ Handle ++ "!\r\n",
    Talker ! {send, Message},
    exit(quit).

sys({_State, Line}) ->
    {Keyword, String} = head(Line),
    {Channel, _} = head(String),
    Result = case Keyword of
        "list"  -> list();
        "join"  -> join(Channel);
        "leave" -> leave(Channel);
        _       -> bargle()
    end,
    Result.

world({_State, _Line}) ->
    "SYSTEM: World commands are not yet implemented".

help({_, _, Actions}) ->
    Sys = sys_help(),
    A = case dict:to_list(Actions) of
        []        -> "none (no minion currently under control)";
        Available -> [String ++ "\r\n" || {String, _} <- Available]
    end,
    Sys ++ A.

%% Chat
list() ->
    "I'm sorry, I can't let you do that, Dave.".

join([]) -> bargle();
join(Channel) ->
    "Think you're good enough to be in " ++ Channel ++ ", eh?".

leave([]) -> bargle();
leave(Channel) ->
    "Think you're too good for " ++ Channel ++ ", eh?".

chat({Handle, Channels, Line}) ->
    {Channel, Message} = head(Line),
    case dict:find(Channel, Channels) of
        error ->
            "You aren't in #" ++ Channel;
        Pid   ->
            Pid ! {chat, {Handle, Message}},
            none
    end.

%% Magic
prompt(Handle) -> Handle ++ " $ ".

greet() ->
    "\r\nWelcome to ErlMUD\r\n\r\n"
    "By what name do you wish to be known?\r\n"
    "$ ".

sys_help() ->
    "Available commands:\r\n"
    "------------------------------------------------------------------------\r\n"
    "chat Channel Text\r\n"
    "echo Text\r\n"
    "sys Command [Args]\r\n"
    "  where Command is:\r\n"
    "    join Channel\r\n"
    "    leave Channel\r\n"
    "    list\r\n"
    "quit\r\n"
    "\r\n"
    "Shortcuts:\r\n"
    "------------------------------------------------------------------------\r\n"
    "#Channel Text        OR   chat Channel Text\r\n"
    "\\Command [Args]      OR   sys Command [Args]\r\n"
    "?                    OR   help\r\n"
    "\r\n"
    "\r\n"
    "Available Actions:\r\n"
    "------------------------------------------------------------------------\r\n".

%% Minion interactions
acquire_minion(Pid) ->
    Ref = monitor(process, Pid),
    Actions = init_actions(Pid),
    {Pid, Ref, Actions}.

%% Accman interactions
check(Handle) ->
    em_lib:call(accman, check, Handle).

verify(Handle, PW) ->
    em_lib:call(accman, verify, {Handle, PW}).

create_acc(Handle, PW) ->
    em_lib:call(accman, create, {Handle, PW}).

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

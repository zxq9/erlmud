-module(telcon).
-export([start_link/1, code_change/1]).

%% Startup
start_link(Talker) ->
    spawn_link(fun() -> welcome(Talker) end).

%% Authentication & Registration
welcome(Talker) ->
    Greeting = greet(),
    Talker ! {send, Greeting},
    Handle = receive {received, Bin} -> topline(Bin) end,
    case accman:check(Handle) of
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
            case accman:create(Handle, P1) of
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
    case accman:verify(Handle, PW) of
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
    Actions = init_actions(none),
    Minion = {none, none, Actions},
    Channels = init_channels(Handle),
    State = {Talker, Handle, Minion, Channels},
    loop(State).

init_channels(_) -> [].

init_actions(Minion) ->
    case Minion of
        none  -> dict:new();
        Pid   -> em_lib:call(Pid, get_actions)
    end.

%% Service
loop(State = {Talker, Handle, Minion = {MPid, MRef, _Actions}, Channels}) ->
  receive
    {received, Bin} ->
        NewState = evaluate(Bin, State),
        loop(NewState);
    {chat, Message} ->
        unprompted(Message, State),
        loop(State);
    {notice, Message} ->
        unprompted(Message, State),
        loop(State);
    {acquire_minion, Pid} ->
        NewMinion = acquire_minion(Pid),
        loop({Talker, Handle, NewMinion, Channels});
    {'DOWN', MRef, process, MPid, _Reason} ->
        unprompted("Minion disconnected.", State),
        Actions = init_actions(none),
        loop({Talker, Handle, {none, none, Actions}, Channels});
    Message = {'DOWN', _, process, _, _} ->
        NewState = handle_down(State, Message),
        loop(NewState);
    status ->
        note("Status:~n  Talker: ~p~n  Handle: ~p~n  Minion: ~p~n, Channels: ~p",
             [Talker, Handle, Minion, Channels]),
        loop(State);
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
    {Response, NewState} = interpret(Expansion, State),
    Reply = case Response of
        none -> prompt(Handle);
        ok   -> "ok" ++ "\r\n" ++ prompt(Handle);
        Any  -> Any ++ "\r\n" ++ prompt(Handle)
    end,
    Talker ! {send, Reply},
    NewState.

interpret(Expansion, State = {_, _, Minion, Channels}) ->
    {Keyword, Line} = head(Expansion),
    case Keyword of
        "chat"  -> {chat(Channels, Line), State};
        "sys"   -> sys(State, Line);
        "help"  -> {help(Minion), State};
        ""      -> {none, State};
        _       -> {perform(Keyword, Line, Minion), State}
    end.

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
        $#  -> "chat " ++ Line;
        $\\ -> "sys " ++ T;
        $?  -> "help " ++ T;
        _   -> Line
    end.

head(Line) ->
    Stripped = string:strip(Line),
    {Head, Tail} = head([], Stripped),
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
who(State, _) ->
    {"Not yet implemented.", State}.

echo(String) -> String.

bargle() -> "Arglebargle, glop-glyph!?!".

quit(Talker, Handle) ->
    Message = "Goodbye, " ++ Handle ++ "!\r\n",
    Talker ! {send, Message},
    exit(quit).

sys(State = {Talker, Handle, _, Channels}, Line) ->
    {Keyword, String} = head(Line),
    case Keyword of
        "chan"  -> {chan(String, Channels), State};
        "join"  -> join(State, String);
        "leave" -> leave(State, String);
        "who"   -> who(State, String);
        "echo"  -> {echo(Line), State};
        "quit"  -> quit(Talker, Handle);
        _       -> {bargle(), State}
    end.

help({_, _, Actions}) ->
    Sys = sys_help(),
    A = case dict:to_list(Actions) of
        []        -> "none (no minion currently under control)";
        Available -> [String ++ "\r\n" || {String, _} <- Available]
    end,
    Sys ++ A.

%% Chat
chan(Line, Channels) ->
    {Keyword, _String} = head(Line),
    case Keyword of
        ""      -> show(Channels);
        Channel -> exam(Channel)
    end.

show(Channels) ->
    List = chanman:list(),
    Mine = [Name || {Name, _, _} <- Channels],
    NotMine = lists:subtract(List, Mine),
    DisplayList =
        fun(Z) ->
            Count = length(Z),
            if
                Count >  0 -> string:join(Z, "\r\n    ");
                Count =< 0 -> "[None]"
            end
        end,
    Message = "Channels joined:\r\n    " ++
        DisplayList(Mine) ++
        "\r\nAvailable channels:\r\n    " ++
        DisplayList(NotMine),
    Message.

exam(Channel) ->
    case chanman:get_pid(Channel) of
        {ok, ChanPid} ->
            Handles = channel:handles(ChanPid),
            Count = length(Handles),
            Message = 
                Channel ++ ": " ++
                integer_to_list(Count) ++ "\r\n    " ++
                string:join(Handles, "\r\n    "),
            Message;
        {error, _}    ->
            "Channel " ++ Channel ++ " does not exist."
    end.

join(State, []) -> {bargle(), State};
join(State = {Talker, Handle, Minion, Channels}, String) ->
    {Channel, _} = chanhead(String),
    case lists:keymember(Channel, 1, Channels) of
        true ->
            Response = "Already in " ++ Channel,
            {Response, State};
        false ->
            ChanPid = chanman:acquire(Channel),
            ChanMon = monitor(process, ChanPid),
            ChanPid ! {join, {Handle, self()}},
            NewChannels = [{Channel, ChanPid, ChanMon} | Channels],
            NewState = {Talker, Handle, Minion, NewChannels},
            {ok, NewState}
    end.

leave(State, []) -> {bargle(), State};
leave(State = {Talker, Handle, Minion, Channels}, String) ->
    {Channel, _} = chanhead(String),
    case lists:keyfind(Channel, 1, Channels) of
        false ->
            Response = "You're not in " ++ Channel,
            {Response, State};
        Chan = {Channel, ChanPid, ChanMon} ->
            NewChannels = lists:delete(Chan, Channels),
            demonitor(ChanMon),
            ChanPid ! {leave, self()},
            NewState = {Talker, Handle, Minion, NewChannels},
            {ok, NewState}
    end.

chat(Channels, Line) ->
    {Channel, Message} = chanhead(Line),
    case lists:keyfind(Channel, 1, Channels) of
        false ->
            "You aren't in " ++ Channel;
        {_, Pid, _}   ->
            Pid ! {chat, {self(), Message}},
            none
    end.

chanhead(String) ->
    {Word, Line} = head(String),
    Channel = case Word of
        [$#|_] -> Word;
        _      -> [$#|Word]
    end,
    {Channel, Line}.

%% Magic
prompt(Handle) -> Handle ++ " $ ".

unprompted(Data, {Talker, Handle, _, _}) ->
    Message = "\r" ++ Data ++ "\r\n" ++ prompt(Handle),
    Talker ! {send, Message}.

greet() ->
    "\r\nWelcome to ErlMUD\r\n\r\n"
    "By what name do you wish to be known?\r\n"
    "$ ".

sys_help() ->
    "Available commands:\r\n"
    "------------------------------------------------------------------------\r\n"
    "chat Channel Text\r\n"
    "sys Command [Args]\r\n"
    "  where Command is:\r\n"
    "    join Channel\r\n"
    "    leave Channel\r\n"
    "    chan\r\n"
    "    chan Channel\r\n"
    "    echo Text\r\n"
    "    quit\r\n"
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

%% Handler
handle_down(State = {Talker, Handle, Minion, Channels},
            Message = {'DOWN', Ref, process, _, _}) ->
    case lists:keyfind(Ref, 3, Channels) of
        false ->
            note("Received ~p", [Message]),
            State;
        Chan = {Channel, _, _} ->
            Notice = "CHAT: Channel " ++ Channel ++ " closed.",
            unprompted(Notice, State),
            NewChannels = lists:delete(Chan, Channels),
            {Talker, Handle, Minion, NewChannels}
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

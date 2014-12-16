-module(telcon).
-export([start_link/1, code_change/1,
         black/1,  dk_gray/1,
         red/1,    lt_red/1,
         green/1,  lt_green/1,
         brown/1,  yellow/1,
         blue/1,   lt_blue/1,
         purple/1, lt_purple/1,
         cyan/1,   lt_cyan/1,
         gray/1,   white/1]).

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
            note("accman:check/1 failed with ~p~n", [Reason]),
            Talker ! {send, "Something went wrong, let's try that again...\r\n"},
            welcome(Talker)
    end.

register_acc(Talker, Handle) ->
    Talker ! {send, "\r\nLooks like you're new here.\r\nEnter a passphrase: "},
    PassHash = receive {received, P1Bin} -> accman:salthash(topbin(P1Bin)) end,
    Talker ! {send, "Re-enter to confirm: "},
    Check = receive {received, P2Bin} -> accman:checkhash(PassHash, topbin(P2Bin)) end,
    case Check of
        true  ->
            case accman:create(Handle, PassHash) of
                ok ->
                    M = "\r\nWelcome to ErlMUD, " ++ Handle ++ "!\r\n" ++
                        "Enjoy your stay, and don't feed the trolls.\r\n" ++
                        Handle ++ " $ ",
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
    PW = receive {received, Bin} -> topbin(Bin) end,
    case accman:verify(Handle, PW) of
        verified ->
            Salutation = "Welcome back, " ++ Handle ++ "!\r\n" ++ Handle ++ " $ ",
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
    Minion = {none, none, none, init_actions(none)},
    Aliases = [],
    Channels = [],
    State = {Talker, Handle, Aliases, Minion, Channels},
    loop(State).

init_actions(none) -> [];
init_actions(Ilk)  -> Ilk:actions().

%% Service
loop(State = {Talker, Handle, Aliases, Minion = {Ilk, MPid, MRef, Actions}, Channels}) ->
  receive
    {received, Bin} ->
        NewState = evaluate(Bin, State),
        loop(NewState);
    {chat, Message} ->
        handle_chat(Message, State),
        loop(State);
    {observation, Event} ->
        case Ilk of
            none -> note("Received ~tp~n", [Event]);
            Ilk  -> emit(Ilk:observe(Event, Minion), State)
        end,
        loop(State);
    {system, Message} ->
        unprompted(Message, State),
        loop(State);
    {'DOWN', MRef, process, MPid, _Reason} ->
        unprompted("Your minion vanished in a puff of smoke!", State),
        Actions = init_actions(none),
        loop({Talker, Handle, Aliases, {none, none, none, Actions}, Channels});
    Message = {'DOWN', _, process, _, _} ->
        NewState = handle_down(State, Message),
        loop(NewState);
    status ->
        note("Status:~n  Talker: ~p~n  Handle: ~p~n  Minion: ~p~n Channels: ~p",
             [Talker, Handle, Aliases, Minion, Channels]),
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
evaluate(Bin, State = {Talker, _, Aliases, _, _}) ->
    Line = topline(Bin),
    Expansion = rewrite(Line, Aliases),
    {Response, NewState} = interpret(Expansion, State),
    Reply = case Response of
        none -> prompt(NewState);
        ok   -> "ok" ++ "\r\n" ++ prompt(NewState);
        Any  -> Any ++ "\r\n" ++ prompt(NewState)
    end,
    Talker ! {send, Reply},
    NewState.

interpret(Expansion, State = {_, _, _, Minion, Channels}) ->
    {Keyword, Line} = head(Expansion),
    case Keyword of
        "chat"  -> {chat(Channels, Line), State};
        "sys"   -> sys(State, Line);
        "help"  -> {help(Minion), State};
        ""      -> {none, State};
        _       -> {perform(Keyword, Line, Minion), State}
    end.

perform(_, _, {none, _, _, _}) ->
    bargle();
perform(Keyword, String, {_, MPid, _, Actions}) ->
    case lists:keyfind(Keyword, 1, Actions) of
        {_, Command, Nature, _, _} ->
            MPid ! {action, {Nature, {Command, String}}},
            none;
        false ->
            bargle()
    end.

handle_chat(Message, State) ->
    unprompted(cyan(Message), State).

%% Binary & string handling
rewrite([], _) -> [];
rewrite(Line = [H|T], Aliases) ->
    case H of
        $! -> "chat " ++ Line;
        $/ -> "sys " ++ T;
        $? -> "help " ++ T;
        _  -> unalias(Line, Aliases)
    end.

unalias(Line, []) ->
    Line;
unalias(Line, Aliases) ->
    {Key, Rest} = head(Line),
    case proplists:lookup(Key, Aliases) of
        none        -> Line;
        {_, Actual} -> string:join([Actual, Rest], " ")
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

topbin(Bin) ->
    [H|_] = binary:split(Bin, <<"\r\n">>),
    H.

topline(Bin) -> binary_to_list(topbin(Bin)).

%% Controller actions
sys(State = {Talker, Handle, _, _, _}, Line) ->
    {Keyword, String} = head(Line),
    case Keyword of
        "chan"  -> chan(State, String);
        "char"  -> char(State, String);
        "alias" -> alias(State, String);
        "who"   -> who(State, String);
        "echo"  -> {echo(Line), State};
        "quit"  -> quit(Talker, Handle);
        _       -> {bargle(), State}
    end.

who(State, _) -> {"Not yet implemented.", State}.

alias(State, _) -> {"Not yet implemented.", State}.

echo(String) -> String.

bargle() -> "Arglebargle, glop-glyph!?!".

quit(Talker, Handle) ->
    Message = "Goodbye, " ++ Handle ++ "!\r\n",
    Talker ! {send, Message},
    exit(quit).

help({_, _, _, []}) ->
    sys_help() ++ gray("    [None]");
help({_, _, _, Actions}) ->
    Sys = sys_help(),
    Act = string:join([string:left(yellow(Command), 23) ++ gray("- " ++ Desc)
                       || {_, _, _, Command, Desc} <- Actions],
                      "\r\n    "),
    string:join([Sys, Act], "    ").

%% Chat
chan(State = {_, _, _, _, Channels}, Line) ->
    {Keyword, String} = head(Line),
    case Keyword of
        ""      -> {show(Channels), State};
        "list"  -> {show(Channels), State};
        "join"  -> join(State, String);
        "leave" -> leave(State, String);
        Channel -> {exam(Channel), State}
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
    io_lib:format(white("  Channels joined:\r\n") ++
                  cyan("    ~ts\r\n") ++
                  white("  Available channels:\r\n") ++
                  cyan("    ~ts"),
                  [DisplayList(Mine), DisplayList(NotMine)]).

exam(Channel) ->
    case chanman:get_pid(Channel) of
        {ok, ChanPid} ->
            Handles = channel:handles(ChanPid),
            Count = length(Handles),
            io_lib:format(cyan("~ts") ++ gray(": ~p\r\n    ~ts"),
                          [Channel, Count, string:join(Handles, "\r\n    ")]);
        {error, _}    ->
            Message = string:join([gray("Channel"),
                                   cyan("~ts"),
                                   gray("does not exist")],
                                  " "),
            io_lib:format(Message, [Channel])
    end.

join(State, []) ->
    {bargle(), State};
join(State = {Talker, Handle, Aliases, Minion, Channels}, String) ->
    {Channel, _} = chanhead(String),
    case lists:keymember(Channel, 1, Channels) of
        true ->
            Response = io_lib:format("Already in " ++ cyan("~ts"), [Channel]),
            {Response, State};
        false ->
            ChanPid = chanman:acquire(Channel),
            ChanMon = monitor(process, ChanPid),
            ChanPid ! {join, {Handle, self()}},
            NewChannels = [{Channel, ChanPid, ChanMon} | Channels],
            NewState = {Talker, Handle, Aliases, Minion, NewChannels},
            {ok, NewState}
    end.

leave(State, []) ->
    {bargle(), State};
leave(State = {Talker, Handle, Aliases, Minion, Channels}, String) ->
    {Channel, _} = chanhead(String),
    case lists:keyfind(Channel, 1, Channels) of
        Chan = {Channel, ChanPid, ChanMon} ->
            NewChannels = lists:delete(Chan, Channels),
            demonitor(ChanMon),
            ChanPid ! {leave, self()},
            NewState = {Talker, Handle, Aliases, Minion, NewChannels},
            {ok, NewState};
        false ->
            Response = io_lib:format("You're not in ~ts", [Channel]),
            {Response, State}
    end.

chat(Channels, Line) ->
    {Channel, Message} = chanhead(Line),
    case lists:keyfind(Channel, 1, Channels) of
        {_, Pid, _}   ->
            Pid ! {chat, {self(), Message}},
            none;
        false ->
            io_lib:format("You aren't in ~ts", [Channel])
    end.

chanhead(String) ->
    {Word, Line} = head(String),
    Channel = case Word of
        [$!|_] -> Word;
        _      -> [$!|Word]
    end,
    {Channel, Line}.

%% Char system actions
char(State, Line) ->
    {Keyword, String} = head(Line),
    case Keyword of
        ""     -> charlist(State);
        "list" -> charlist(State);
        "load" -> charload(State, String);
        "quit" -> charquit(State);
        "make" -> charmake(State, String);
        "drop" -> chardrop(State, String);
        _      -> {bargle(), State}
    end.

charlist(State = {_, Handle, _, _, _}) ->
    Mobs = case charman:list(Handle) of
        []   -> "[None]";
        List -> string:join(List, "\r\n    ")
    end,
    Response = "  Your chars:\r\n    " ++ Mobs,
    {Response, State}.

charload(State = {Talker, Handle, _, {none, _, _, _}, Channels}, String) ->
    {Name, _} = head(String),
    case charman:load(Handle, Name) of
        {error, owner} ->
            Message = Name ++ " is not one of your characters.",
            {Message, State};
        {ok, CharData = {{Mod, _}, _}} ->
            Ilk = (Mod:read(ilk, CharData)):con_ext(text),
            CharPid = mobman:spawn_mob(CharData),
            CharRef = monitor(process, CharPid),
            Actions = init_actions(Ilk),
            Minion = {Ilk, CharPid, CharRef, Actions},
            NewAliases = Ilk:alias(),
            NewState = {Talker, Handle, NewAliases, Minion, Channels},
            Message = "A new reality grips you...",
            {Message, NewState}
    end;
charload(State, _) ->
    Message = "You already control a character.",
    {Message, State}.

charquit(State = {_, _, _, {none, _, _, _}, _}) ->
    Message = "No characters to unload.",
    {Message, State};
charquit({Talker, Handle, _, {_, CharPid, CharRef, _}, Channels}) ->
    demonitor(CharRef),
    CharPid ! {self(), divorce},
    NewState = {Talker, Handle, [], {none, none, none, []}, Channels},
    Message = "You're such a quitter.",
    {Message, NewState}.

charmake(State, []) ->
    {bargle(), State};
charmake(State, String) ->
    {Name, _} = head(String),
    Message = case charman:check(Name) of
        available -> charcreate(Name, State);
        taken     -> "That name is already taken!"
    end,
    {Message, State}.

charcreate(Name, State = {_, Handle, _, _, _}) ->
    Assemble = fun(Mod, Opts) -> Mod:species() ++ Opts end,
    Available = lists:foldl(Assemble, [], mobman:playable()),
    {RollStats, SpecOpts} = pickone("What species?", Available, State),
    Picker = fun({Label, Opt}, I) -> I ++ pickone(Label, Opt, State) end,
    Influences = lists:foldl(Picker, RollStats, SpecOpts),
    Mob = mob:topoff(mob:roll(Name, Influences)),
    case charman:make(Handle, Mob) of
        ok              -> io_lib:format("~ts created.", [Name]);
        {error, exists} -> "Someone just swiped that name!"
    end.

chardrop(State, []) ->
    {bargle(), State};
chardrop(State = {_, Handle, _, _}, String) ->
    {Name, _} = head(String),
    Message = case charman:drop(Handle, Name) of
        ok             -> Name ++ " dropped.";
        {error, owner} -> Name ++ " is not one of your characters!"
    end,
    {Message, State}.

%% Magic
pickone(Question, Index, State) ->
    pickone(Question, Index, fun unprompted/2, State).

pickone(Question, Index, Speaker, State = {Talker, _, _, _, _}) ->
    {Menu, Opts} = menufy(Index),
    String = white("~ts\r\n") ++ gray("~ts\r\n$ "),
    UserQuery = io_lib:format(String, [Question, Menu]),
    Talker ! {send, UserQuery},
    Choice = receive {received, Bin} -> topline(Bin) end,
    case proplists:lookup(Choice, Opts) of
        none ->
            Speaker("That's not an option. Typo? Try again.", State),
            pickone(Question, Index, State);
        {_, Name} ->
            proplists:get_value(Name, Index)
    end.

menufy(Index) ->
    List = [Name || {Name, _} <- Index],
    Opts = [{integer_to_list(N), O}
            || {N, O} <- lists:zip(lists:seq(1, length(List)), List)],
    Menu = string:join([io_lib:format("  ~s - ~ts", [N, O]) || {N, O} <- Opts], "\r\n"),
    {Menu, Opts}.

emit(silent, _)      -> ok;
emit(Message, State) -> unprompted(Message, State).

prompt({_, Handle, _, {none, _, _, _}, _}) ->
    gray(Handle ++ " $ ");
prompt({_, _, _, {Ilk, MPid, _, _}, _}) ->
    gray(Ilk:prompt(MPid)).

unprompted(Data, State = {Talker, _, _, _, _}) ->
    % TODO: Find a better way to clear the current line...
    Message = "\r" ++ string:chars($\s, 78, "\r") ++
        Data ++ "\r\n" ++ prompt(State),
    Talker ! {send, Message}.

greet() ->
    white("\r\nWelcome to ErlMUD\r\n\r\n") ++
    gray("By what name do you wish to be known?\r\n"
         "$ ").

sys_help() ->
    white("  Available commands:\r\n") ++
    gray("    chat Channel Text      - Send Text to everyone in Channel\r\n"
         "    sys Command [Args]     - Execute system Command\r\n"
         "    help                   - Display this message\r\n"
         "\r\n") ++
    white("  Command is one of:\r\n") ++
    gray("    char [Args]            - Invoke character commands\r\n"
         "    chan [Args]            - Invoke channel commands\r\n"
         "    alias [Args]           - Invoke alias commands\r\n"
         "    echo Text              - Echo Text back to your terminal\r\n"
         "    quit                   - Disconnect abruptly\r\n"
         "\r\n") ++
    white("  Character commands:\r\n") ++
    gray("    list OR (nothing)      - List your characters\r\n"
         "    CharacterName          - Display character status\r\n"
         "    load Name              - Take control of character\r\n"
         "    quit                   - Release control of current chatacter\r\n"
         "    make Name              - Create a new character\r\n"
         "\r\n") ++
    white("  Channel commands:\r\n") ++
    gray("    list OR (nothing)      - List system chat channels\r\n"
         "    ChannelName            - Display user count and handle list\r\n"
         "    join Channel           - Join or create the named Channel\r\n"
         "    leave Channel          - Leave the named Channel\r\n"
         "\r\n") ++
    white("  Alias commands:\r\n") ++
    gray("    list OR (nothing)      - List current command aliases\r\n"
         "    Alias Text             - Set Alias to Text\r\n"
         "    clear                  - Clear all aliases\r\n"
         "    clear Alias            - Clear Alias\r\n"
         "    default                - Reset aliases to default\r\n"
         "\r\n") ++
    white("  Shortcuts:\r\n") ++
    gray("    !Channel Text      OR   chat Channel Text\r\n"
         "    /Command [Args]    OR   sys Command [Args]\r\n"
         "    ?                  OR   help\r\n"
         "\r\n") ++
    white("  Available Actions:\r\n").

%% Handler
handle_down(State = {Talker, Handle, Minion, Channels},
            Message = {'DOWN', Ref, process, _, _}) ->
    case lists:keyfind(Ref, 3, Channels) of
        Chan = {Channel, _, _} ->
            Notice = "CHAT: Channel " ++ Channel ++ " closed.",
            unprompted(Notice, State),
            NewChannels = lists:delete(Chan, Channels),
            {Talker, Handle, Minion, NewChannels};
        false ->
            note("Received ~p", [Message]),
            State
    end.

black(String)     -> [27,91,48,48,59,51,48,109] ++ String.
dk_gray(String)   -> [27,91,48,49,59,51,48,109] ++ String.
red(String)       -> [27,91,48,48,59,51,49,109] ++ String.
lt_red(String)    -> [27,91,48,49,59,51,49,109] ++ String.
green(String)     -> [27,91,48,48,59,51,50,109] ++ String.
lt_green(String)  -> [27,91,48,49,59,51,50,109] ++ String.
brown(String)     -> [27,91,48,48,59,51,51,109] ++ String.
yellow(String)    -> [27,91,48,49,59,51,51,109] ++ String.
blue(String)      -> [27,91,48,48,59,51,52,109] ++ String.
lt_blue(String)   -> [27,91,48,48,59,51,52,109] ++ String.
purple(String)    -> [27,91,48,48,59,51,53,109] ++ String.
lt_purple(String) -> [27,91,48,49,59,51,53,109] ++ String.
cyan(String)      -> [27,91,48,48,59,51,54,109] ++ String.
lt_cyan(String)   -> [27,91,48,49,59,51,54,109] ++ String.
gray(String)      -> [27,91,48,48,59,51,55,109] ++ String.
white(String)     -> [27,91,48,49,59,51,55,109] ++ String.

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

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
    Minion = {none, none, init_actions(none, none)},
    Aliases = default_command_alias(),
    Channels = [],
    State = {Talker, Handle, Aliases, Minion, Channels},
    loop(State).

init_actions(none, none)   -> [];
init_actions(MobPid, Form) -> mob:get_actions(MobPid, Form).

%% Service
loop(State = {Talker, Handle, Aliases, Minion = {MPid, MRef, Actions}, Channels}) ->
  receive
    {received, Bin} ->
        NewState = evaluate(Bin, State),
        loop(NewState);
    {chat, Message} ->
        handle_chat(Message, State),
        loop(State);
    {observation, Event} ->
        observe(Event, State),
        loop(State);
    {system, Message} ->
        unprompted(Message, State),
        loop(State);
    {'DOWN', MRef, process, MPid, _Reason} ->
        unprompted("Your minion vanished in a puff of smoke!", State),
        Actions = init_actions(none, none),
        loop({Talker, Handle, Aliases, {none, none, Actions}, Channels});
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

perform(_, _, {none, _, _}) ->
    bargle();
perform(Keyword, String, {MPid, _, Actions}) ->
    case lists:keyfind(Keyword, 1, Actions) of
        {_, Command, Nature, _, _} ->
            MPid ! {action, {Nature, {Command, String}}},
            none;
        false ->
            bargle()
    end.

handle_chat(Message, State) ->
    unprompted(Message, State).

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

help({_, _, []}) ->
    sys_help() ++ "    [None]";
help({_, _, Actions}) ->
    Sys = sys_help(),
    Act = string:join([string:left(Command, 23) ++ "- " ++ Desc
                       || {_, _, Command, Desc} <- Actions],
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
    Message =
        "  Channels joined:\r\n    " ++
        DisplayList(Mine) ++
        "\r\n  Available channels:\r\n    " ++
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

join(State, []) ->
    {bargle(), State};
join(State = {Talker, Handle, Aliases, Minion, Channels}, String) ->
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
            Response = "You're not in " ++ Channel,
            {Response, State}
    end.

chat(Channels, Line) ->
    {Channel, Message} = chanhead(Line),
    case lists:keyfind(Channel, 1, Channels) of
        {_, Pid, _}   ->
            Pid ! {chat, {self(), Message}},
            none;
        false ->
            "You aren't in " ++ Channel
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

charload(State = {Talker, Handle, Aliases, {none, _, _}, Channels}, String) ->
    {Name, _} = head(String),
    case charman:load(Handle, Name) of
        {error, owner} ->
            Message = Name ++ " is not one of your characters.",
            {Message, State};
        {ok, CharData} ->
            CharPid = mobman:spawn_minion(CharData),
            CharRef = monitor(process, CharPid),
            Actions = init_actions(CharPid, text),
            Minion = {CharPid, CharRef, Actions},
            NewState = {Talker, Handle, Aliases, Minion, Channels},
            Message = io_lib:format("Received chardata ~p.", [CharData]),
            {Message, NewState}
    end;
charload(State, _) ->
    Message = "You already control a character.",
    {Message, State}.

charquit(State = {_, _, _, {none, _, _}, _}) ->
    Message = "No characters to unload.",
    {Message, State};
charquit({Talker, Handle, Aliases, {CharPid, CharRef, _}, Channels}) ->
    demonitor(CharRef),
    CharPid ! {self(), divorce},
    NewState = {Talker, Handle, Aliases, {none, none, []}, Channels},
    Message = "You're such a quitter.",
    {Message, NewState}.

charmake(State, []) ->
    {bargle(), State};
charmake(State = {_, Handle, _, _, _}, String) ->
    {Name, _} = head(String),
    Char = solicit_chardata(State, Name),
    Message = case charman:make(Handle, Char) of
        ok              -> Name ++ " created.";
        {error, exists} -> "That name is already taken!"
    end,
    {Message, State}.

solicit_chardata(_, Name) ->
    {Name,
     {{Name,
       ["human"],                   % Aliases
       humanoid,                    % Ilk
       "Wanderer",                  % Class
       "Some description.",         % Description
       "Male"},                     % Sex
      {{{15, 30}, {30, 120}},       % Condition
       {{0, []}, {0, []}},          % Inventory
       [],                          % Effects
       {{[], []}},                  % Skills
       {1, 10},                     % Score
       {100, 100, 100, 100, 100,    % Stats
        100, 100, 100, 100}},
      {0,0,0}}}.                    % Location

chardrop(State, []) ->
    {bargle(), State};
chardrop(State = {_, Handle, _, _}, String) ->
    {Name, _} = head(String),
    Message = case charman:drop(Handle, Name) of
        ok             -> Name ++ " dropped.";
        {error, owner} -> Name ++ " is not one of your characters!"
    end,
    {Message, State}.

%% Semantic event -> text translation
observe(Event, State) ->
    Message = case Event of
        {look, self, View} ->
            io_lib:format("You see: ~p", [View]);
        {{say, Line}, self, success} ->
            "You say,\"" ++ Line ++ "\"";
        {{say, Line}, Speaker, success} ->
            Speaker ++ " says,\"" ++ Line ++ "\"";
        {status, self, Stat} ->
            io_lib:format("Your stat: ~p", [Stat]);
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
    end,
    emit(Message, State).

emit(silent, State) ->
    State;
emit(Message, State) ->
    unprompted(Message, State),
    State.

prompt({_, Handle, _, {none, _, _}, _}) ->
    Handle ++ " $ ";
prompt({_, _, _, {MPid, _, _}, _}) ->
    {{CurHP, MaxHP}, {CurSP, MaxSP}} = mob:condition(MPid),
    Health = case (CurHP div (MaxHP div 5)) of
        5 -> "Healthy";
        4 -> "Scratched";
        3 -> "Bloodied";
        2 -> "Hurt";
        1 -> "Wounded";
        0 -> "Critical"
    end,
    Stamina = case (CurSP div (MaxSP div 5)) of
        5 -> "Fresh";
        4 -> "Strong";
        3 -> "Tiring";
        2 -> "Winded";
        1 -> "Haggard";
        0 -> "Bonked"
    end,
    "(" ++ Health ++ ", " ++ Stamina ++ ") $ ".

unprompted(Data, State = {Talker, _, _, _, _}) ->
    % TODO: Find a better way to clear the current line...
    Message = "\r" ++ string:chars($\s, 78, "\r") ++
        Data ++ "\r\n" ++ prompt(State),
    Talker ! {send, Message}.

%% Magic
greet() ->
    "\r\nWelcome to ErlMUD\r\n\r\n"
    "By what name do you wish to be known?\r\n"
    "$ ".

sys_help() ->
    "  Available commands:\r\n"
    "    chat Channel Text      - Send Text to everyone in Channel\r\n"
    "    sys Command [Args]     - Execute system Command\r\n"
    "    help                   - Display this message\r\n"
    "  Command is one of:\r\n"
    "    char [Args]            - Invoke character commands\r\n"
    "    chan [Args]            - Invoke channel commands\r\n"
    "    alias [Args]           - Invoke alias commands\r\n"
    "    echo Text              - Echo Text back to your terminal\r\n"
    "    quit                   - Disconnect abruptly\r\n"
    "\r\n"
    "  Character commands:\r\n"
    "    list OR (nothing)      - List your characters\r\n"
    "    CharacterName          - Display character status\r\n"
    "    load Name              - Take control of character\r\n"
    "    quit                   - Release control of current chatacter\r\n"
    "    make Name              - Create a new character\r\n"
    "\r\n"
    "  Channel commands:\r\n"
    "    list OR (nothing)      - List system chat channels\r\n"
    "    ChannelName            - Display user count and handle list of ChannelName\r\n"
    "    join Channel           - Join or create the named Channel\r\n"
    "    leave Channel          - Leave the named Channel (and close if last member)\r\n"
    "\r\n"
    "  Alias commands:\r\n"
    "    list OR (nothing)      - List current command aliases\r\n"
    "    Alias Text             - Set Alias to Text\r\n"
    "    clear                  - Clear all aliases\r\n"
    "    clear Alias            - Clear Alias\r\n"
    "    default                - Reset aliases to default\r\n"
    "\r\n"
    "  Shortcuts:\r\n"
    "    !Channel Text      OR   chat Channel Text\r\n"
    "    /Command [Args]    OR   sys Command [Args]\r\n"
    "    ?                  OR   help\r\n"
    "\r\n"
    "  Available Actions:\r\n".

default_command_alias() ->
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
     {"/", "kill"},
     {"st", "status"},
     {"stat", "status"},
     {"north", "go north"},
     {"south", "go south"},
     {"east", "go east"},
     {"west", "go west"},
     {"down", "go down"},
     {"up", "go up"}].

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

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

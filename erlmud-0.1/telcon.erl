-module(telcon).
-export([start_link/1]).

start_link(Talker) ->
    spawn_link(fun() -> welcome(Talker) end).

%% Authentication & Registration
welcome(Talker) ->
    Greeting = greet(),
    Talker ! {send, Greeting},
    Handle = receive {received, Bin} -> topline(Bin) end,
    case check_registry_for(Handle) of
        unregistered   -> register_acc(Talker, Handle);
        registered     -> authenticate(Talker, Handle);
        {fail, Reason} ->
            io:format("~p telcon: check_registry_for/1 failed with ~p~n",
                      [self(), Reason]),
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
                    io:format("~p telcon: create_acc/2 failed with ~p~n", [self(), Reason]),
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
    case check_password(Handle, PW) of
        verified ->
            Salutation = "Welcome back, " ++ Handle ++ "!\r\n" ++ prompt(Handle),
            Talker ! {send, Salutation},
            init(Talker, Handle);
        badpass ->
            timer:sleep(7000),
            authenticate(Talker, Handle);
        {fail, Reason} ->
            io:format("~p telcon: check_password/2 failed with ~p~n",
                      [self(), Reason]),
            Talker ! {send, "Something went wrong, let's try that again...\r\n"},
            authenticate(Talker, Handle)
    end.

init(Talker, Handle) ->
    Modules = [telcon],
    Commands = load_command(Modules),
    loop(Talker, Handle, Commands, Modules).

%% Main service
loop(Talker, Handle, Commands, Modules) ->
  receive
    {received, Bin} ->
        ok = evaluate(Bin, Talker, Handle, Commands),
        loop(Talker, Handle, Commands, Modules);
    shutdown ->
        io:format("~p telcon: Shutting down.~n", [self()]);
    Any ->
        io:format("~p telcon: Received ~tp~n", [self(), Any]),
        loop(Talker, Handle, Commands, Modules)
  end.

%% Input evaluation
evaluate(Bin, Talker, Handle, Commands) ->
    Line = topline(Bin),
    {Action, Args} = interpret(Line, Commands),
    Result = Action(Handle, Args),
    Reply = Result ++ "\r\n" ++ prompt(Handle),
    Talker ! {send, Reply},
    ok.

interpret(Line, Commands) ->
    {Command, Args} = head(Line),
    case orddict:find(Command, Commands) of
        {ok, Action} -> {Action, Args};
        error        -> {fun notify/2, "Arglebargle, glop-glyf!?!"}
    end.

%% Binary & string handling
head(Line) ->
    {Head, Tail} = head([], Line),
    {lists:reverse(Head), Tail}.

head(Word, []) ->
    {Word, []};
head([], Line = [H|T]) ->
    case H of
        $#  -> {"chat", Line};
        $\\ -> {"sys", Line};
        Z   -> head([Z], T)
    end;
head(Word, [H|T]) ->
    case H of
        $\s -> {Word, T};
        Z   -> head([Z|Word], T)
    end.

stringify(Bin) -> string:tokens(binary_to_list(Bin), "\r\n").

topline(Bin) ->
    case stringify(Bin) of
        [Top | _] -> Top;
        []        -> ""
    end.

%% Controller actions
notify(_, String) -> String.

echo(_, String) -> "echo: " ++ String.

nothing(_, _) -> "".

quit(_, _) -> exit(quit).

%% Magic
prompt(Handle) -> Handle ++ " $ ".

greet() ->
    "\r\nWelcome to ErlMUD\r\n\r\n"
    "By what name do you wish to be known?\r\n"
    "$ ".

% TODO: Query modules for available commands
load_command(_Modules) ->
    Z = [{"", fun nothing/2},
         {"quit", fun quit/2},
         {"echo", fun echo/2}],
    orddict:from_list(Z).

%% Accman interactions
check_registry_for(Handle) ->
    call(accman, lookup, Handle).

check_password(Handle, PW) ->
    call(accman, verify, {Handle, PW}).

create_acc(Handle, PW) ->
    call(accman, create, {Handle, PW}).

%% Synchronous handler
call(Proc, Request, Data) ->
    Ref = monitor(process, Proc),
    Proc ! {self(), Ref, {Request, Data}},
    receive
        {Ref, Res} ->
            demonitor(Ref, [flush]),
            Res;
        {'DOWN', Ref, process, Proc, Reason} ->
            {fail, Reason}
    after 1000 ->
        io:format("~p: ask(~p, ~p, ~p) timed out.~n", [self(), Proc, Request, Data]),
        {fail, timeout}
    end.

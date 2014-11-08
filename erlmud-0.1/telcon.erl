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
                    loop(Talker, Handle);
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
            loop(Talker, Handle);
        badpass ->
            timer:sleep(7000),
            authenticate(Talker, Handle);
        {fail, Reason} ->
            io:format("~p telcon: check_password/2 failed with ~p~n",
                      [self(), Reason]),
            Talker ! {send, "Something went wrong, let's try that again...\r\n"},
            authenticate(Talker, Handle)
    end.

%% Main service
loop(Talker, Handle) ->
  receive
    {received, Bin} ->
        Message = stringify(Bin),
        Reply = evaluate(Message) ++ "\r\n" ++ prompt(Handle),
        Talker ! {send, Reply},
        loop(Talker, Handle);
    shutdown ->
        io:format("~p telcon: Shutting down.~n", [self()]);
    Any ->
        io:format("~p telcon: Received ~tp~n", [self(), Any]),
        loop(Talker, Handle)
  end.

%% Magic
stringify(Bin) -> string:tokens(binary_to_list(Bin), "\r\n").

topline(Bin) ->
    [Top|_] = stringify(Bin),
    Top.

evaluate(Message) -> evaluate([], Message).

evaluate(A, []) -> string:join(lists:reverse(A), "\r\n");
evaluate(A, [TopLine | Rest]) ->
    evaluate([parse(TopLine) | A], Rest).

parse(Line) -> 
    [Command | Tokens] = string:tokens(Line, " "),
    parse(Command, Tokens, Line).

parse("say", _, Line) ->
    Phrase = string:strip(string:sub_string(Line, string:chr(Line, $\s))),
    "You say,\"" ++ Phrase ++ "\"";
    % say/1 should always pass through the location
    % say(Phrase)

parse(Other, _, _) -> "You don't know how to " ++ Other.

prompt(Handle) -> Handle ++ " $ ".

greet() ->
    "\r\nWelcome to ErlMUD\r\n\r\n"
    "By what name do you wish to be known?\r\n"
    "$ ".

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

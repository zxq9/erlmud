-module(telcon).
-export([start_link/1]).

start_link(Talker) ->
    spawn_link(fun() -> welcome(Talker) end).

%% Authentication & Registration
welcome(Talker) ->
    Greeting = greet(),
    Talker ! {send, Greeting},
    Handle = receive {received, Bin} -> stringify(Bin) end,
    case check_registry_for(Handle) of
        unregistered -> register_acc(Talker, Handle);
        registered   -> authenticate(Talker, Handle)
    end.

register_acc(Talker, Handle) ->
    Talker ! {send, "Enter a passphrase: "},
    P1 = receive {received, P1Bin} -> stringify(P1Bin) end,
    Talker ! {send, "Re-enter to confirm: "},
    P2 = receive {received, P2Bin} -> stringify(P2Bin) end,
    case P1 =:= P2 of
        true  ->
            case create_acc(Handle, P1) of
                ok  ->
                    M = "Welcome to ErlMUD, " ++ Handle ++ "!\r\n" ++
                        "Enjoy your stay, and don't feed the trolls.\r\n" ++ prompt(),
                    Talker ! {send, M},
                    loop(Talker, Handle);
                {error, handle_is_in_use} -> 
                    M = "Someone else nabbed your handle!\r\n"
                        "Let's try this again...\r\n",
                    Talker ! {send, M},
                    welcome(Talker)
            end;
        false ->
            Talker ! {send, "OK, for real this time...\r\n"},
            register_acc(Talker, Handle)
     end.

authenticate(Talker, Handle) ->
    Talker ! {send, "Passphrase: "},
    PW = receive {received, Bin} -> stringify(Bin) end,
    case check_password(Handle, PW) of
        verified ->
            Salutation = "Welcome back, " ++ Handle ++ "!\r\n" ++ prompt(),
            Talker ! {send, Salutation},
            loop(Talker, Handle);
        badpass ->
            timer:sleep(7000),
            authenticate(Talker, Handle)
    end.

%% Main service
loop(Talker, Handle) ->
  receive
    {received, Bin} ->
        Message = stringify(Bin),
        Reply = evaluate(Message) ++ "\r\n" ++ prompt(),
        Talker ! {send, Reply},
        loop(Talker, Handle);
    shutdown ->
        io:format("~p telcon: Shutting down.~n", [self()]);
    Any ->
        io:format("~p telcon: Received ~tp~n", [self(), Any]),
        loop(Talker, Handle)
  end.

%% Magic
evaluate(Message) -> "Got: " ++ Message.

stringify(Bin) -> binary_to_list(binary:replace(Bin, <<"\r\n">>, <<>>)).

prompt() -> "(Some Prompt)$ ".

greet() ->
    "\r\nWelcome to ErlMUD\r\n\r\n"
    "By what name do you wish to be known?\r\n"
    "$ ".

%% Accman interactions
check_registry_for(Handle) ->
    Ref = make_ref(),
    accman ! {self(), Ref, {lookup, Handle}},
    receive {Ref, Response} -> Response end.

check_password(Handle, PW) ->
    Ref = make_ref(),
    accman ! {self(), Ref, {verify, {Handle, PW}}},
    receive {Ref, Response} -> Response end.

create_acc(Handle, PW) ->
    Ref = make_ref(),
    accman ! {self(), Ref, {create, {Handle, PW}}},
    receive {Ref, Response} -> Response end.

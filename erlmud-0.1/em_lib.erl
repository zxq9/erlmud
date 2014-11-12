-module(em_lib).
-export([note/3, broadcast/2, call/2, call/3]).

note(Module, String, Args) ->
    S = "~p ~p: " ++ String ++ "~n",
    A = [self(), Module | Args],
    io:format(S, A).

broadcast(Procs, Message) ->
    [Proc ! Message || Proc <- Procs],
    ok.

call(Proc, Verb, Data) ->
    call(Proc, {Verb, Data}).

%% Synchronous handler
call(Proc, Request) ->
    Ref = monitor(process, Proc),
    Proc ! {self(), Ref, Request},
    receive
        {Ref, Res} ->
            demonitor(Ref, [flush]),
            Res;
        {'DOWN', Ref, process, Proc, Reason} ->
            {fail, Reason}
    after 1000 ->
        io:format("~p: call(~p, ~p) timed out.~n", [self(), Proc, Request]),
        {fail, timeout}
    end.

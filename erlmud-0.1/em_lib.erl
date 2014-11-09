-module(em_lib).
-export([note/3, broadcast/2]).

note(Module, String, Args) ->
    S = "~p ~p: " ++ String ++ "~n",
    A = [self(), Module | Args],
    io:format(S, A).

broadcast(Procs, Message) ->
    [Proc ! Message || Proc <- Procs],
    ok.

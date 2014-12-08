-module(em_lib).
-export([note/3, broadcast/2, call/2, call/3, incoming/2,
         calc_weight/1, weight/1, roll/1, roll/3]).

note(Module, String, Args) ->
    S = "~p ~p: " ++ String ++ "~n",
    A = [self(), Module | Args],
    io:format(S, A).

broadcast(Procs, Message) ->
    [Proc ! Message || Proc <- Procs],
    ok.

call(Proc, Verb, Data) ->
    call(Proc, {Verb, Data}).

incoming(Pid, Event) ->
    call(Pid, incoming, Event).

calc_weight(Entities) ->
    SumWeight = fun(Entity, A) -> weight(Entity) + A end,
    lists:foldl(SumWeight, 0, Entities).

weight(Entity = {{Mod, _}, _}) ->
    Mod:total_weight(Entity).

roll({Min, Mean, Max}) ->
    roll(Min, Mean, Max).

roll(Min, Mean, Max) ->
    random:seed(now()),
    Span = Max - Min,
    Peak = Mean - Min,
    Base = random:uniform(Span),
    Pull = random:uniform(),
    Z = if
        Base == Peak -> Base; 
        Base >  Peak -> (((Base - Peak) * Pull) + Base) / 2;
        Base <  Peak -> (((Peak - Base) * Pull) + Base) / 2
    end,
    round(Z + Min).

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

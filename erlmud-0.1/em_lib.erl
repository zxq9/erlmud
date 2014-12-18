-module(em_lib).
-export([note/3, broadcast/2, call/2, call/3, incoming/2,
         roll/1, roll/3, bracket/3,
         entity/1]).

note(Module, String, Args) ->
    S = "~p ~p: " ++ String ++ "~n",
    A = [self(), Module | Args],
    io:format(S, A).

broadcast(Procs, Message) ->
    [Proc ! Message || Proc <- Procs],
    ok.

% Synchronous handler
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

call(Proc, Verb, Data) ->
    call(Proc, {Verb, Data}).

incoming(Pid, Event) ->
    call(Pid, incoming, Event).

roll({Min, Mean, Max}) ->
    roll(Min, Mean, Max).

roll(Min, Mean, Max) ->
    Offset = if
        Min <  0 -> abs(Min);
        Min == 0 -> Min;
        Min >  0 -> -Min
    end,
    Span = Max + Offset,
    Peak = Mean + Offset,
    Base = random:uniform(Span),
    Pull = random:uniform(),
    Z = if
        Base == Peak -> Base; 
        Base >  Peak -> ((((Base - Peak) * Pull) + Peak) + Base) / 2;
        Base <  Peak -> ((((Peak - Base) * Pull) + Peak) + Base) / 2
    end,
    round(Z - Offset).

bracket(_, 0, _) ->
    1;
bracket(Index, Range, Layers) ->
    (Index * Layers) div ((Range * Layers) div Layers).

entity(State = {{Mod, _}, _}) ->
    Name = Mod:read(name, State),
    Aliases = [Name | Mod:read(aliases, State)],
    Weight = Mod:read(total_weight, State),
    Vis = Mod:read(vis, State),
    {self(), Aliases, {Name, Mod, Weight, Vis}}.

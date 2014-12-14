-module(em_lib).
-export([note/3, broadcast/2, call/2, call/3, incoming/2,
         calc_weight/1, weight/1, roll/1, roll/3, bracket/3,
         entity/1, hand/5]).

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

calc_weight(Entities) ->
    SumWeight = fun(Entity, A) -> weight(Entity) + A end,
    lists:foldl(SumWeight, 0, Entities).

weight(Entity = {{Mod, _}, _}) ->
    Mod:total_weight(Entity).

roll({Min, Mean, Max}) ->
    roll(Min, Mean, Max).

roll(Min, Mean, Max) ->
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

bracket(Index, Range, Layers) ->
    (Index * Layers) div ((Range * Layers) div Layers).

entity(State = {{Mod, _}, _}) ->
    Name = Mod:read(name, State),
    Aliases = [Name | Mod:read(aliases, State)],
    Weight = Mod:read(total_weight, State),
    Vis= Mod:read(vis, State),
    {self(), Aliases, {Name, Mod, Weight, Vis}}.

hand(Requestor, TRef, Target, HolderPid, RecipientPid) ->
    Result = case call(HolderPid, transfer, {Target, TRef}) of
        M = {ok, TPid} ->
            link(TPid),
            TEntity = call(TPid, {move, RecipientPid}),
            ok = call(RecipientPid, load, {TPid, TEntity}),
            unlink(TPid),
            HolderPid ! {ok, TRef},
            M;
        M = {error, _} ->
            M
    end,
    Requestor ! {TRef, Result}.

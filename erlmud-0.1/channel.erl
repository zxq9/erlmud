-module(channel).
-export([start/2, start_link/2, code_change/1,
         handles/1]).

%% Interface
handles(ChanPid) ->
    em_lib:call(ChanPid, handles).

%% Startup
start(Parent, Conf)         -> starter(fun spawn/1, Parent, Conf).
start_link(Parent, Conf) -> starter(fun spawn_link/1, Parent, Conf).

starter(Spawn, Parent, Conf) -> Spawn(fun() -> init(Parent, Conf) end).

init(Parent, {Channel, Roster, Banned}) ->
    loop({Parent, Channel, Roster, Banned}).

%% Service
loop(State = {Parent, Channel, Roster, Banned}) ->
  receive
    {chat, Message} ->
        chat(Channel, Message, Roster),
        loop(State);
    {join, User} ->
        NewRoster = add(User, Roster),
        note("Added ~p to ~p", [User, Roster]),
        loop({Parent, Channel, NewRoster, Banned});
    {leave, Pid} ->
        NewRoster = remove(Pid, Roster),
        note("Removed ~p from ~p", [Pid, Roster]),
        loop({Parent, Channel, NewRoster, Banned});
    {From, Ref, handles} ->
        Handles = get_handles(Roster),
        From ! {Ref, Handles},
        loop(State);
    Message = {'DOWN', _, process, _, _} ->
        NewState = handle_down(State, Message),
        loop(NewState);
    status ->
        note("Roster ~tp", [Roster]),
        loop(State);
    code_change ->
        ?MODULE:code_change(State);
    shutdown ->
        note("Shutting down."),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(State)
  end.

%% Magic
chat(Channel, {Pid, Line}, Roster) ->
    case lists:keyfind(Pid, 1, Roster) of
        {_, Handle, _, _}  ->
            Message = Channel ++ " <" ++ Handle ++ "> " ++ Line,
            em_lib:broadcast(con_pids(Roster), {chat, Message}),
            ok;
        false ->
            ok
    end.

add(User = {_, ConPid}, Roster) ->
    Ref = monitor(process, ConPid),
    add(User, Ref, Roster).

add({Handle, ConPid}, Ref, [])     -> [{ConPid, Handle, Ref, owner}];
add({Handle, ConPid}, Ref, Roster) -> [{ConPid, Handle, Ref, lurker} | Roster].

remove(Pid, Roster) ->
    NewRoster = case lists:keyfind(Pid, 1, Roster) of
        User = {_, _, Ref, _} ->
            demonitor(Ref),
            lists:delete(User, Roster);
        false ->
            Roster
    end,
    case NewRoster of
        []  -> exit(closed);
        _   -> NewRoster
    end.

con_pids(Roster) -> [Pid || {Pid, _, _, _} <- Roster].

handle_down(State = {Parent, Channel, Roster, Banned},
            Message = {_, Ref, _, _, _}) ->
    case lists:keyfind(Ref, 3, Roster) of
        false ->
            note("Received ~p", [Message]),
            State;
        User = {Pid, _, _, _} ->
            note("~p ~p sent 'DOWN'", [Channel, User]),
            NewRoster = remove(Pid, Roster),
            {Parent, Channel, NewRoster, Banned}
    end.

get_handles(Roster) ->
    [Handle || {_, Handle, _, _} <- Roster].

%% Code changer
code_change(State) ->
    note("Changing code."),
    loop(State).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

-module(channel).
-export([start/1, start_link/1, code_change/4]).

%% Startup
start(Parent)            -> start(Parent, none).
start(Parent, Conf)      -> starter(fun spawn/1, Parent, Conf).
start_link(Parent)       -> start_link(Parent, none).
start_link(Parent, Conf) -> starter(fun spawn_link/1, Parent, Conf).

starter(Spawn, Parent, Conf) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = Spawn(fun() -> init(Parent, Conf) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init(Parent, {Channel, Roster, Banned}) ->
    loop(Parent, Channel, Roster, Banned).

%% Service
loop(Parent, Channel, Roster, Banned) ->
  receive
    {chat, {Sender, Message}} ->
        chat(Channel, Sender, Message, Roster),
        loop(Parent, Channel, Roster, Banned);
    {join, User} ->
        NewRoster = add(User, Roster),
        loop(Parent, Channel, NewRoster, Banned);
    {leave, Handle} ->
        NewRoster = remove(Handle, Roster),
        loop(Parent, Channel, NewRoster, Banned);
    status ->
        note("Roster ~tp", [Roster]),
        loop(Parent, Channel, Roster, Banned);
    code_change ->
        ?MODULE:code_change(Parent, Channel, Roster, Banned);
    shutdown ->
        note("Shutting down."),
        exit(shutdown);
    Any ->
        note("Received ~tp", [Any]),
        loop(Parent, Channel, Roster, Banned)
  end.

%% Magic
chat(Channel, Sender, Message, Roster) ->
    case dict:is_key(Sender, Roster) of
        true  ->
            Line = Channel ++ "> " ++ Sender ++ ": " ++ Message,
            UserPids = con_pids(Roster),
            em_lib:broadcast(UserPids, {notice, Line}),
            ok;
        false ->
            case get_conpid(Sender) of
                {ok, SenderPid} ->
                    Message = "@system: You are not in " ++ Channel,
                    SenderPid ! {notice, Message};
                {error, _} ->
                    note("Received message \"~tp\"~n"
                         "  for channel ~tp~n"
                         "  from unknown sender ~tp.",
                         [Message, Channel, Sender])
            end
    end.

add({Handle, ConPid}, Roster) ->
    dict:store(Handle, {ConPid, lurker}, Roster).

remove(Handle, Roster) ->
    dict:erase(Handle, Roster).

con_pids(Roster) ->
    [Pid || {_, {Pid, _}} <- Roster].

%% External calls
get_conpid(Handle) ->
    case em_lib:call(conman, lookup, Handle) of
        {fail, Reason} -> {error, Reason};
        ConPid         -> ConPid
    end.

%% Code changer
code_change(Parent, Channel, Roster, Banned) ->
    note("Changing code."),
    loop(Parent, Channel, Roster, Banned).

%% System
note(String) ->
    note(String, []).

note(String, Args) ->
    em_lib:note(?MODULE, String, Args).

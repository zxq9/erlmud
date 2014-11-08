-module(chanman).
-export([start/1, start/2, start_link/1, start_link/2]).

start(Parent) -> start(Parent, []).

start(Parent, Conf) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = spawn(fun() -> init(Parent, Conf) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid -> 
            {ok, Pid}
    end.

start_link(Parent) -> start_link(Parent, []).

start_link(Parent, Conf) ->
    Name = ?MODULE,
    case whereis(Name) of
        undefined ->
            Pid = spawn_link(fun() -> init(Parent, Conf) end),
            true = register(Name, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init(Parent, Conf) ->
    io:format("~p chanman: Notional initialization.~n", [self()]),
    loop(Parent, Conf).

loop(Parent, Conf) ->
  receive
    shutdown ->
        io:format("~p chanman: Shutting down.~n", [self()]),
        exit(shutdown);
    Any ->
        io:format("~p chanman: Received ~tp~n", [self(), Any]),
        loop(Parent, Conf)
  end.

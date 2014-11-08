-module(erlmud).
-export([start/0]).

start() ->
    true = register(erlmud, spawn(fun() -> init() end)).

init() ->
    io:format("~p erlmud: Starting up.~n", [self()]),
    ok = chanman:start(),
    ok = accman:start(),
    ok = wayman:start(),
    ok = locman:start(),
    ok = objman:start(),
    ok = mobman:start(),
    ok = netman:start([{telnet, start_link, [2222]}]),
    loop().

loop() ->
  receive
    shutdown ->
        io:format("~p erlmud: Shutting down.~n", [self()]),
        netman ! shutdown,
        mobman ! shutdown,
        objman ! shutdown,
        locman ! shutdown,
        wayman ! shutdown,
        accman ! shutdown,
        chanman ! shutdown,
        exit(shutdown);
    Any ->
        io:format("~p erlmud: Received ~tp~n", [self(), Any]),
        loop()
  end.

-module(my_port).
-include("cargo.hrl").

-export([start/0, stop/0, foo/1, bar/1]).

start() ->
    PortPath = filename:join([
        code:priv_dir(?CARGO_LOAD_APP), 
        "crates", 
        "my_port", 
        "my_port"
    ]),
    spawn(fun() -> init(PortPath) end).

stop() ->
    my_port ! stop.

foo(X) ->
    call_port([1, X]).

bar(X) ->
    call_port([2, X]).

call_port(Data) ->
    my_port ! {call, self(), Data},
    receive
        {my_port, Result} -> Result
    end.

init(PortPath) ->
    register(my_port, self()),
    Port = open_port({spawn, PortPath}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
        {call, Caller, Data} ->
            Port ! {self(), {command, Data}},
            receive
                {Port, {data, Result}} ->
                    Caller ! {my_port, Result}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} -> ok
            end
    end.
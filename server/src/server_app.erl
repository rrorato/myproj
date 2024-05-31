-module(server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ServPid = start_server(),
    {ok, ServPid}.

stop(_State) ->
    ok.

start_server() ->
    Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(8080, [binary, {active, false}]),
        spawn(fun() -> acceptor(Listen) end),
        timer:sleep(infinity)
    end),
    Pid.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    handle(Socket).

handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            io:format("Quitted: ~n", []),
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            io:format("Recieved: ~p~n", [Msg]),
            gen_tcp:send(Socket, Msg),
            handle(Socket)
    end.


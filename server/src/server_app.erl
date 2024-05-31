-module(server_app).
-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    ServPid = start_server(),
    {ok, ServPid}.

stop(_State) ->
    ok.

start_server() ->
    ets:new(allowedUsers, [named_table, protected, set, {keypos, 1}]),
    ets:insert(allowedUsers, {<<"riccardo">>, allowed}),
    ets:insert(allowedUsers, {<<"alessandro">>, allowed}),
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
            %io:format("Recieved: ~p~n", [Msg]),
            Res = parse_input(Msg),
            %io:format("Sending: ~p~n", [Res]),
            gen_tcp:send(Socket, Res),
            handle(Socket)
    end.

parse_input(ActionInfo) ->
    ActionInfoParsed = jsx:decode(ActionInfo),
    case is_valid_action_info(ActionInfoParsed) of
        false ->
            {error, formatError};
        true ->
            Name = maps:get(<<"name">>, ActionInfoParsed),
            case ets:lookup(allowedUsers, Name) of
                [] ->
                    {error, userNotAllowedError};
                [_] ->
                    Action = maps:get(<<"action">>, ActionInfoParsed),
                    ActionSpecs = maps:get(<<"actionSpecs">>, ActionInfoParsed),
                    action(Name, Action, ActionSpecs)
            end
    end.

is_valid_action_info(ActionInfo) when is_map(ActionInfo) ->
    maps:is_key(<<"name">>, ActionInfo) andalso
    maps:is_key(<<"action">>, ActionInfo) andalso
    maps:is_key(<<"actionSpecs">>, ActionInfo);
is_valid_action_info(_) ->
    false.

action(Name, Action, ActionSpecs) ->
    io:format("TAKING ACTION: ~n", []),
    "actionDone".
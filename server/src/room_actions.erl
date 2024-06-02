-module(room_actions).

-export([init/0, handle_action/3, register_user/2]).

init() ->
    ets:new(rooms, [named_table, public, set, {keypos, 1}]),
    ets:new(user_rooms, [named_table, public, set]),
    ets:new(userPIDs, [named_table, public, set, {keypos, 1}]).

% This function registers a user with their PID
register_user(User, Pid) ->
    ets:insert(userPIDs, {User, Pid}).

handle_action(Name, <<"createRoom">>, #{<<"room">> := Room}) ->
    io:format("~p created room ~p with options~n", [Name, Room]),
    ets:insert_new(rooms, {Room, Name, []}),
    #{<<"state">> => <<"ok">>, <<"info">> => <<"roomCreated">>};

handle_action(Name, <<"destroyRoom">>, #{<<"room">> := Room}) ->
    case ets:lookup(rooms, Room) of
        [{Room, Name, _}] ->
            ets:delete(rooms, Room),
            ets:delete(user_rooms, {Room, Name}),
            #{<<"state">> => <<"ok">>, <<"info">> => <<"roomDestroyed">>};
        _ ->
            #{<<"state">> => <<"error">>, <<"why">> => <<"notAllowed">>}
    end;

handle_action(_Name, <<"listRooms">>, _) ->
    Rooms = ets:tab2list(rooms),
    RoomNames = [Room || {Room, _, _} <- Rooms],
    RoomNamesString = lists:flatten(io_lib:format("~p", [RoomNames])),
    RoomNamesBinary = iolist_to_binary(RoomNamesString),
    #{<<"state">> => <<"ok">>, <<"roomNames">> => RoomNamesBinary};

handle_action(Name, <<"joinRoom">>, #{<<"room">> := Room}) ->
    case ets:lookup(rooms, Room) of
        [{Room, _, _}] ->
            ets:insert(user_rooms, {Room, Name}),
            #{<<"state">> => <<"ok">>, <<"info">> => <<"joinedRoom">>};
        [] ->
            #{<<"state">> => <<"error">>, <<"info">> => <<"roomNotFound">>}
    end;

handle_action(Name, <<"leaveRoom">>, #{<<"room">> := Room}) ->
    ets:delete_object(user_rooms, {Room, Name}),
    #{<<"state">> => <<"ok">>, <<"info">> => <<"leftRoom">>};

handle_action(_, <<"sendMessage">>, #{<<"room">> := Room, <<"message">> := Message}) ->
    case ets:lookup(rooms, Room) of
        [{Room, _, _}] ->
            Members = ets:lookup(user_rooms, Room),
            lists:foreach(fun({RoomActual, Member}) ->
                case ets:lookup(userPIDs, Member) of
                    [{Member, Pid}] ->
                        io:format("Sending message ~p to ~p~n", [Message,Pid]),
                        Pid ! {sendMessage, RoomActual, Message};
                    [] ->
                        #{<<"state">> => <<"error">>, <<"info">> => <<"noUserFoundInRoom">>}
                end
            end, Members),
            #{<<"state">> => <<"ok">>, <<"info">> => <<"messageSent">>};
        [] ->
            #{<<"state">> => <<"error">>, <<"why">> => <<"roomNotFound">>}
    end;

handle_action(_Name, _Action, _ActionSpecs) ->
    io:format("~p taking unknown action ~p with options ~p ~n", [_Name, _Action, _ActionSpecs]),
    #{<<"state">> => <<"error">>, <<"why">> => <<"unknownAction">>}.


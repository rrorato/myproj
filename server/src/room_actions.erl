-module(room_actions).
-export([init/0, handle_action/3]).

init() ->
    ets:new(rooms, [named_table, public, set, {keypos, 1}]),
    ets:new(user_rooms, [named_table, public, set]),
    ets:new(messages, [named_table, public, duplicate_bag]).

handle_action(Name, <<"createRoom">>, #{<<"room">> := Room}) ->
    io:format("~p created room ~p with options~n", [Name, Room]),
    ets:insert_new(rooms, {Room, Name, []}),
    utils:build_dict("ok", "roomCreated");

handle_action(Name, <<"destroyRoom">>, #{<<"room">> := Room}) ->
    case ets:lookup(rooms, Room) of
        [{Room, Name, _}] ->
            ets:delete(rooms, Room),
            ets:delete(user_rooms, {Room, Name}),
            utils:build_dict("ok", "roomDestroyed");
        _ ->
            utils:build_dict("error", "notAllowed")
    end;

handle_action(_Name, <<"listRooms">>, _) ->
    RoomNamesBinary = utils:tab2binary(rooms),
    #{<<"state">> => <<"ok">>, <<"rooms">> => RoomNamesBinary};

handle_action(Name, <<"joinRoom">>, #{<<"room">> := Room}) ->
    case ets:lookup(rooms, Room) of
        [{Room, _, _}] ->
            ets:insert(user_rooms, {Room, Name}),
            utils:build_dict("ok", "joinedRoom");
        [] ->
            utils:build_dict("error", "roomNotFound")
    end;

handle_action(Name, <<"leaveRoom">>, #{<<"room">> := Room}) ->
    ets:delete_object(user_rooms, {Room, Name}),
    utils:build_dict("ok", "leftRoom");

handle_action(_, <<"sendMessage">>, #{<<"room">> := Room, <<"message">> := Message}) ->
    case ets:lookup(rooms, Room) of
        [{Room, _, _}] ->
            ets:insert(messages, {Room, Message}),
            utils:build_dict("ok", "messageSent");
        [] ->
            utils:build_dict("error", "roomNotFound")
    end;

handle_action(Name, <<"receiveMessage">>, #{<<"room">> := Room}) ->
    case ets:lookup(rooms, Room) of
        [{Room, _, _}] ->
            case ets:match_object(user_rooms, {Room, Name}) of
                [{Room, Name}] ->
                    Messages = ets:match_object(messages, {Room, '_'}),
                    #{<<"state">> => <<"ok">>, <<"messages">> => lists:map(fun({_, Message}) -> Message end, Messages)};
                [] ->
                    utils:build_dict("error", "notInRoom")
            end;
        [] ->
            utils:build_dict("error", "roomNotFound")
    end;
    
handle_action(_Name, _Action, _ActionSpecs) ->
    io:format("~p taking unknown action ~p with options ~p ~n", [_Name, _Action, _ActionSpecs]),
    utils:build_dict("error", "unknownAction").
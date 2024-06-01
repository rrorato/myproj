-module(room_actions).

% -export([init/0, handle_action/3]).

% init() ->
%     ets:new(rooms, [named_table, protected, set, {keypos, 1}]),
%     ets:new(user_rooms, [bag, protected]).

% handle_action(Name, <<"createRoom">>, #{<<"room">> := Room}) ->
%     ets:insert_new(rooms, {Room, Name, []}),
%     {ok, roomCreated};

% handle_action(Name, <<"destroyRoom">>, #{<<"room">> := Room}) ->
%     case ets:lookup(rooms, Room) of
%         [{Room, Name, _}] ->
%             ets:delete(rooms, Room),
%             ets:delete(user_rooms, {Room, '_'}),
%             {ok, roomDestroyed};
%         _ ->
%             {error, notAllowed}
%     end;

% handle_action(_Name, <<"listRooms">>, _) ->
%     Rooms = ets:tab2list(rooms),
%     RoomNames = [Room || {Room, _, _} <- Rooms],
%     {ok, RoomNames};

% handle_action(Name, <<"joinRoom">>, #{<<"room">> := Room}) ->
%     case ets:lookup(rooms, Room) of
%         [{Room, _, _}] ->
%             ets:insert(user_rooms, {Room, Name}),
%             {ok, joinedRoom};
%         [] ->
%             {error, roomNotFound}
%     end;

% handle_action(Name, <<"leaveRoom">>, #{<<"room">> := Room}) ->
%     ets:delete_object(user_rooms, {Room, Name}),
%     {ok, leftRoom};

% handle_action(Name, <<"sendMessage">>, #{<<"room">> := Room, <<"message">> := Message}) ->
%     case ets:lookup(rooms, Room) of
%         [{Room, _, _}] ->
%             Members = ets:lookup(user_rooms, Room),
%             lists:foreach(fun({_Room, Member}) ->
%                 % Here you would send the message to each member
%                 io:format("Sending message to ~p: ~p~n", [Member, Message])
%             end, Members),
%             {ok, messageSent};
%         [] ->
%             {error, roomNotFound}
%     end;

handle_action(_Name, _Action, _ActionSpecs) ->
    io:format("~p taking unknown action ~p with options ~p ~n", [_Name, _Action, _ActionSpecs]),
    {error, unknownAction}.

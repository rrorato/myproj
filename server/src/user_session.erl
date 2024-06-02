-module(user_session).

-export([start/1]).

start(User) ->
    room_actions:register_user(User, self()),
    loop(User).

loop(User) ->
    receive
        {sendMessage, Room, Message} ->
            io:format("User ~p received message in room ~p: ~p~n", [User, Room, Message]),
            loop(User);
        Other ->
            io:format("User ~p received unknown message: ~p~n", [User, Other]),
            loop(User)
    end.
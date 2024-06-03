-module(utils).
-export([tab2binary/1, build_dict/2]).

tab2binary(Tab) ->
    TabList = ets:tab2list(Tab),
    TabConcat = [Elem || {Elem, _, _} <- TabList],
    TabString = lists:flatten(io_lib:format("~p", [TabConcat])),
    TabBinary = iolist_to_binary(TabString),
    TabBinary.

build_dict(State, Info) ->
    #{<<"state">> => list_to_binary(State), <<"info">> => list_to_binary(Info)}.
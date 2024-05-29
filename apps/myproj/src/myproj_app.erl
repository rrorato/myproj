%%%-------------------------------------------------------------------
%% @doc myproj public API
%% @end
%%%-------------------------------------------------------------------

-module(myproj_app).

-behaviour(application).

-export([start/2, stop/1, hello/0]).

hello() -> "Hello World".

start(_StartType, _StartArgs) ->
    myproj_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

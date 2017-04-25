-module(erlide_server_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    io:format("Start server app ~p~n", [application:get_all_env()]),
    case erlide_server_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    io:format("Stop server app ~n"),
    init:stop(),
    ok.


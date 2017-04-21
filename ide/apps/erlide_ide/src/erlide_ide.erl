-module(erlide_ide).

-export([main/1]).

-behaviour(application).
-export([start/2, stop/1]).

main(Args) ->
    io:format("Start erlide server. ~p~n", [Args]),
    application:set_env(erlide_ide, main_args, Args, [{persistent, true}]),
    _R = application:ensure_all_started(erlide_ide),
    receive stop -> ok end,
    ok.

start(_Type, _StartArgs) ->
    io:format("Start app ~p~n", [application:get_all_env()]),

    erlide_ide_sup:start_link().

stop(_State) ->
    io:format("Stop app ~n"),
    init:stop(),
    ok.


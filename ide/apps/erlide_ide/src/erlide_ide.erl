-module(erlide_ide).

-export([main/1]).

-behaviour(application).
-export([start/2, stop/1]).

main(_) ->
    io:format("Start erlide server. ...Not implemented yet... ~n"),
    _R = application:ensure_all_started(erlide_ide),
    receive stop -> ok end,
    ok.

start(_Type, _StartArgs) ->
    io:format("Start app ~n"),
    erlide_ide_sup:start_link().

stop(_State) ->
    io:format("Stop app ~n"),
    init:stop(),
    ok.


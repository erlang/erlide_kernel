-module(erlide_server_app).

-export([
         init/0
        ]).

init() ->
    io:format("Start server app~n"),
    ok.


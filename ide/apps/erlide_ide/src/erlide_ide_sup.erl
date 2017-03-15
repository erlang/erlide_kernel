-module(erlide_ide_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    io:format("Start supervisor ~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("Init supervisor ~n"),
    Children = children(),
    %% no supervisor restarts
    {ok, {{one_for_all, 0, 1}, Children}}.

children() ->
    IdeServer = {erlide_ide_server, {erlide_ide_server, start_link, [get_app_args()]},
        permanent, 60000, worker, [erlide_ide_server]},
    [
        IdeServer
    ].

get_app_args() ->
    init:get_arguments().
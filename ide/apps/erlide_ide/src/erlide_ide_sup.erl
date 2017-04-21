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
    IdeServer = {erlide_lsp_server, {erlide_lsp_server, start_link, []},
        permanent, 60000, worker, [erlide_lsp_server]},
    [
        IdeServer
    ].


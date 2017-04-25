-module(erlide_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = children(),
    %% no supervisor restarts
    {ok, {{one_for_all, 0, 1}, Children}}.

children() ->
    {ok, Port} = application:get_env(erlide_server, port),
    IdeServer = {erlide_lsp_server, {erlide_lsp_server, start_link, [Port]},
        permanent, 60000, worker, [erlide_lsp_server]},
    [
        IdeServer
    ].


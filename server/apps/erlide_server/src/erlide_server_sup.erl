-module(erlide_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    R.

init([]) ->
    Children = children(),
    %% no supervisor restarts
    {ok, {{one_for_one, 0, 1}, Children}}.

children() ->
    {ok, Port} = application:get_env(erlide_server, port),
    IdeConnection = {erlide_lsp_connection, {erlide_lsp_connection, start_link, [Port]},
        permanent, 60000, worker, [erlide_lsp_connection]},
    IdeServer = {erlide_lsp_server, {erlide_lsp_server, start_link, []},
        permanent, 60000, worker, [erlide_lsp_server]},
    IdeClient = {erlide_lsp_client, {erlide_lsp_client, start_link, []},
        permanent, 60000, worker, [erlide_lsp_client]},
    [
        IdeConnection, IdeServer, IdeClient
    ].


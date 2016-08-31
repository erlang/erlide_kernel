%% @author vlad
%% @doc @todo Add description to erlide_kernel_app.

-module(erlide_kernel_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         ]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
          {ok, Pid :: pid()}
              | {ok, Pid :: pid(), State :: term()}
              | {error, Reason :: term()}.
%% ====================================================================
start(_Type, StartArgs) ->
    Endpoints = get_endpoints(),
    lists:foreach(fun({N, M}) ->
                          register_endpoint(N, spawn(M, start, []))
                  end,
                  Endpoints),
    case 'TopSupervisor':start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
    Endpoints = get_endpoints(),
    lists:foreach(fun({N, _}) ->
                          unregister_endpoint(N)
                  end,
                  Endpoints),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_endpoints() ->
    [
     {model, erlide_kernel_model},
     {completion, erlide_kernel_completion}
    ].

register_endpoint(Name, Endpoint) ->
    gproc:reg_other(Name, Endpoint).

unregister_endpoint(Name) ->
    gproc:unreg(Name).



%% init1(L) when is_list(L) ->
%%     [init1(X) || X<-L];
%% init1(execute) ->
%%     ok;
%% init1(ide) ->
%%     ok;
%% init1(build) ->
%%     erlide_xref:start(),
%%     ok;
%% init1(monitor) ->
%%     %watch_eclipse(node(JRex)),
%%     ok.



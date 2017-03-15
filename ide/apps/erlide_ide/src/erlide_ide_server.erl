-module(erlide_ide_server).

-behaviour(gen_server).
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Args) ->
    io:format("Start server ->~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Env) ->
    io:format("-- init ~p~n", [Env]),
    erlang:send_after(1000, self(), hello),
    {ok, []}.

handle_call(_Request, _From, State) ->
    io:format(" >> ~p~n", [_Request]),
    {reply, ok, State}.

handle_cast(_Request, State) ->
    io:format(" >> ~p~n", [_Request]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format(" >> ~p~n", [_Info]),
    {stop, normal, State}.

terminate(_A, _B) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
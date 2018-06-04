-module(erlide_eunit_listener).

-behaviour(eunit_listener).

-export([start/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
         terminate/2]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    erlide_jrpc:event(eunit, {init, Options}),
    ok.

terminate({ok, Data}, _St) ->
    erlide_jrpc:event(eunit, {terminated, Data}),
    ok;
terminate({error, Reason}, _St) ->
    erlide_jrpc:event(eunit, {crashed, Reason}),
    sync_end(error).

sync_end(Result) ->
    receive
        {stop, Reference, ReplyTo} ->
            ReplyTo ! {result, Reference, Result},
            ok
    end.

handle_begin(group, Data, _St) ->
    erlide_jrpc:event(eunit, {begin_group, Data}),
    ok;
handle_begin(test, Data, _St) ->
    erlide_jrpc:event(eunit, {begin_test, Data}),
    ok.

handle_end(group, Data, _St) ->
    erlide_jrpc:event(eunit, {end_group, Data}),
    ok;
handle_end(test, Data, _St) ->
    erlide_jrpc:event(eunit, {end_test, Data}),
    ok.

handle_cancel(group, Data, _St) ->
    erlide_jrpc:event(eunit, {cancel_group, Data}),
    ok;
handle_cancel(test, Data, _St) ->
    erlide_jrpc:event(eunit, {cancel_test, Data}),
    ok.

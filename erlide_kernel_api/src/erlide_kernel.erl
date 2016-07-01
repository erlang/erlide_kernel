%% @author vlad
%% @doc @todo Add description to erlide_kernel.

-module(erlide_kernel).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

-include("erlide_kernel.hrl").

-type endpoint() :: pid().

%%% Meta API

%% Returns the list of supported endpoint names.
-spec get_endpoint_names() -> [atom()].
get_endpoint_names() ->
    [
     model,
     completion
    ].

-spec get_endpoint(atom()) -> endpoint().
get_endpoint(Name) ->
    try
        Pid = gproc:lookup_local_name(Name),
        {ok, Pid}
    catch
        _:_ -> {error, not_found}
    end.

%%% Basic API

%% Projects

get_projects() ->
    Db = whereis(?MODULE),
    call(Db, get_projects, [], []).

%% Apps

get_apps() ->
    Db = whereis(?MODULE),
    call(Db, get_apps, [], []).

get_app() ->
    Db = whereis(?MODULE),
    call(Db, get_app, [], []).

create_app() ->
    Db = whereis(?MODULE),
    call(Db, create_app, [], []).

delete_app() ->
    Db = whereis(?MODULE),
    call(Db, delete_app, [], []).

update_app() ->
    Db = whereis(?MODULE),
    call(Db, update_app, [], []).

get_app_info() ->
    Db = whereis(?MODULE),
    call(Db, get_app_info, [], []).

update_app_info() ->
    Db = whereis(?MODULE),
    call(Db, update_app_info, [], []).

%% Units (per app)

get_units() ->
    {ok, []}.

get_unit() ->
    ok.

create_unit() ->
    ok.

delete_unit() ->
    ok.

update_unit() ->
    ok.

get_unit_info() ->
    ok.

update_unit_info() ->
    ok.

%% Forms (per unit)

get_forms() ->
    ok.

get_form() ->
    ok.

create_form() ->
    ok.

delete_form() ->
    ok.

update_form() ->
    ok.

get_form_info() ->
    ok.

update_form_info() ->
    ok.

%%% Advanced API

start_search_definitions() ->
    ok.

cancel_search_definitions() ->
    ok.

get_definitions() ->
    ok.

search_definition() ->
    ok.

start_search_references() ->
    ok.

cancel_search_references() ->
    ok.

get_references() ->
    ok.

get_call_stack() ->
    ok.

complete_code() ->
    ok.

rename() ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type db():: endpoint().

-spec call(db(), atom(), [term()], [term()]) ->
          {ok, any()} | {more, any()} | {error, {atom(), any()}}.
call(_Db, _Op, _Args, _Options) ->
    ok.


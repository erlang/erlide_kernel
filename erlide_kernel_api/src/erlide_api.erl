%% @author vlad
%% @doc @todo Add description to erlide_api.

-module(erlide_api).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

%%% Basic API

%% Apps

get_apps() ->
    {ok, []}.

get_app() ->
    ok.

create_app() ->
    ok.

delete_app() ->
    ok.

update_app() ->
    ok.

get_app_info() ->
    {ok, []}.

update_app_info() ->
    ok.

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

search_definitions() ->
    ok.

search_references() ->
    ok.

get_call_stack() ->
    ok.


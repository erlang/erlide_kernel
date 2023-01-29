%% Author: jakob
%% Created: 17 feb 2010
-module(erlide_dialyze).

%%
%% Include files
%%

%% -define(DEBUG, 1).

-include("erlide_dbglog.hrl").
-include_lib("kernel/include/file.hrl").

%%
%% Exported Functions
%%
-export([dialyze/5,
         format_warnings/1,
         check_plt/1]).

%%
%% API Functions
%%

dialyze(Files, PltFiles, Includes, FromSource, NoCheckPLT) ->
    From = case FromSource of
               true -> src_code;
               false -> byte_code
           end,
    R = (catch dialyzer:run([{files, Files},
                             {plts, PltFiles},
                             {from, From},
                             {include_dirs, Includes},
                             {analysis_type,  'succ_typings'},
                             {check_plt, not NoCheckPLT}])),
    case R of
        {ErrorOrExit, E} when ErrorOrExit =:= 'EXIT'; ErrorOrExit =:= dialyzer_error ->
            {dialyzer_error, flat(E)};
        Result ->
            {warnings, Result}
    end.

format_warnings(Warnings) ->
    [ dialyzer:format_warning(W) || W <- Warnings].

check_plt(Plt) ->
    dialyzer:run([{analysis_type, plt_check},
                  {init_plt, Plt}]).

%%
%% Local Functions
%%

flat({{dialyzer_error, E}, _}) ->
    flat(E);
flat({dialyzer_error, E}) ->
    flat(E);
flat(L) ->
    lists:flatten(L).

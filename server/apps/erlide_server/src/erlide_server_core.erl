-module(erlide_server_core).

-export([
        init/0,
        initialize/2,
        updated_configuration/2,
        updated_watched_files/2,
        changed_file/3,
        opened_file/2,
        closed_file/2,
        saved_file/2,

        workspace_symbol/3,
        completion/3,
        completion_resolve/3,
        hover/3,
        references/3,
        document_highlight/3,
        document_symbol/3,
        formatting/3,
        range_formatting/3,
        on_type_formatting/3,
        definition/3,
        signature_help/3,
        code_action/3,
        code_lens/3,
        code_lens_resolve/3,
        rename/3,

        default_answer/1
]).

-record(state, {
        client_capabilities = #{},
        server_capabilities = #{},
        configuration = #{},
        watched_files = [],
        open_files = []
			   }).

init() ->
	#state{}.

initialize(State, ClientCapabilities) ->
	Capabilities = #{
        	 textDocumentSync => 1, %% 0=none, 1=full, 2=incremental
        	 hoverProvider => true,
        	 completionProvider => #{
        			 resolveProvider => true,
											 triggerCharacters => []
											},
					 signatureHelpProvider => #{
												triggerCharacters => []
											   },
					 definitionProvider => true,
					 referencesProvider => true,
					 documentHighlightProvider => true,
					 documentSymbolProvider => true,
					 workspaceSymbolProvider => true,
					 codeActionProvider => true,
%% 					 codeLensProvider => #{
%% 										   resolveProvider => true
%% 										  },
					 documentFormattingProvider => true,
					 documentRangeFormattingProvider => true,
%% 					 documentOnTypeFormattingProvider => #{
%% 														   firstTriggerCharacter => <<"">>,
%% 														   moreTriggerCharacters => []
%% 														  },
					 renameProvider => true
					},
	Server = #{capabilities => Capabilities},
	{Server, State#state{client_capabilities=ClientCapabilities, server_capabilities=Server}}.

updated_configuration(State, Settings) ->
	%% TODO start parsing & processing
	%% TODO start compile
	State#state{configuration=Settings}.

updated_watched_files(State, _Changes) ->
	Watched = State#state.watched_files,
	NewWatched = lists:foldl(fun process_watched/2, [], Watched),
	%% TODO start compile
	State#state{watched_files=NewWatched}.

opened_file(State, #{uri:=URI, languageId:=_Language, version:=_Version, text:=_Text}=Item) ->
	Open = State#state.open_files,
	NewOpen = [{URI, Item}|Open],
	State#state{open_files=NewOpen}.

changed_file(State, #{uri:=_URI, version:=_Version}, _Changes) ->
	%% TODO start parsing & processing
	%% TODO start compile
	State.

saved_file(State, #{uri:=_URI}) ->
	State.

closed_file(State, #{uri:=URI}) ->
	Open = State#state.open_files,
	NewOpen = lists:keydelete(URI, 1, Open),
	State#state{open_files=NewOpen}.

workspace_symbol(_State, _Query, Reporter) ->
	%% symbol = #{name, kind, location, containerName?}}
	Res = [],
	Reporter(final, Res).

%% completion_item() :: label, kind?, detail?, documentation?, sortText?, filterText?,
%% insertText?, textEdit? additionalTextEdits?, command? data?

completion(_State, {_DocumentId, _Position}, Reporter) ->
	Res = #{
	  isIncomplete => false,
	  items => []
	 },
	Reporter(final, Res).

completion_resolve(_State, Item, Reporter) ->
	Res = Item,
	Reporter(final, Res).

hover(_State, {_DocumentId, _Position}, Reporter) ->
	%% [markedstring()]:: String (=markdown), #{language, value}
	Res = #{
	  contents => []
	 %%, range => erlide_lsp_utils:range(_Position, _Position)
	 },
	Reporter(final, Res).

references(_State, _Args, Reporter) ->
	Res = [],
	Reporter(final, Res).

document_highlight(_State, _Args, Reporter) ->
	Res = [],
	Reporter(final, Res).

document_symbol(_State, _Args, Reporter) ->
	Res = [],
	Reporter(final, Res).

formatting(_State, _Args, Reporter) ->
	Res = [],
	Reporter(final, Res).

range_formatting(_State, _Args, Reporter) ->
	Res = [],
	Reporter(final, Res).

on_type_formatting(_State, _Args, Reporter) ->
	Res = [],
	Reporter(final, Res).

definition(_State, _Args, Reporter) ->
	Res = [],
	Reporter(final, Res).

signature_help(_State, _Args, Reporter) ->
	Res = #{
	  signatures => [],
	  activeSignature => null,
	  activeParameter => null
	  },
	Reporter(final, Res).

code_action(_State, {_URI, _Range, _Context}, Reporter) ->
	Res = [],
	Reporter(final, Res).

code_lens(_State, _Args, Reporter) ->
	Res = [],
	Reporter(final, Res).

code_lens_resolve(_State, Item, Reporter) ->
		Res = Item,
	Reporter(final, Res).

rename(_State, _Args, Reporter) ->
	%% #{URI: [edits]}
	Res = #{changes => []},
	Reporter(final, Res).


%%%%%%%%%%%%%%%%%

process_watched(#{uri:=URI, type:=1}, List) ->
	%% TODO start parsing & processing
	[URI|List];
process_watched(#{uri:=_URI, type:=2}, List) ->
	%% TODO start parsing & processing
	List;
process_watched(#{uri:=URI, type:=3}, List) ->
	lists:delete(URI, List).

completion_item_kind(text) -> 1;
completion_item_kind(method) -> 2;
completion_item_kind(function) -> 3;
completion_item_kind(constructor) -> 4;
completion_item_kind(field) -> 5;
completion_item_kind(variable) -> 6;
completion_item_kind(class) -> 7;
completion_item_kind(interface) -> 8;
completion_item_kind(module) -> 9;
completion_item_kind(property) -> 10;
completion_item_kind(unit) -> 11;
completion_item_kind(value) -> 12;
completion_item_kind(enum) -> 13;
completion_item_kind(keyword) -> 14;
completion_item_kind(snippet) -> 15;
completion_item_kind(color) -> 16;
completion_item_kind(file) -> 17;
completion_item_kind(reference) -> 18;
%
completion_item_kind(type) -> 7;
completion_item_kind(macro) -> 2;
%
completion_item_kind(_) -> 0.

symbol_kind(file) -> 1;
symbol_kind(module) -> 2;
symbol_kind(namespace) -> 3;
symbol_kind(package) -> 4;
symbol_kind(class) -> 5;
symbol_kind(method) -> 6;
symbol_kind(property) -> 7;
symbol_kind(field) -> 8;
symbol_kind(constructor) -> 9;
symbol_kind(enum) -> 10;
symbol_kind(interface) -> 11;
symbol_kind(function) -> 12;
symbol_kind(variable) -> 13;
symbol_kind(constant) -> 14;
symbol_kind(string) -> 15;
symbol_kind(number) -> 16;
symbol_kind(boolean) -> 17;
symbol_kind(array) -> 18;
%
symbol_kind(type) -> 5;
symbol_kind(macro) -> 6;
%
symbol_kind(_) -> 0.

default_answer(completion) ->
	null;
default_answer(completion_resolve) ->
	null;
default_answer(hover) ->
	null;
default_answer(signature_help) ->
	null;
default_answer(_) ->
	[].

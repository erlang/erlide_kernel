%% @author vlad
%% @doc @todo Add description to erlide_lsp_server.

-module(erlide_lsp_server).

-behaviour(gen_server).

-export([
		 start_link/0,

		 show_message/2,
		 show_message_request/3,
		 log_message/2,
		 telemetry_event/1,
		 publish_diagnostics/2
		]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
				stopped = false,
				crt_id = 0,
				pending_in_requests = [],
				pending_out_requests = [],
				internal_state
			   }).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% client API

show_message(Type, Msg) ->
	gen_server:cast(?SERVER, {show_message, Type, Msg}).

show_message_request(Type, Msg, Actions) ->
	gen_server:call(?SERVER, {show_message_request, Type, Msg, Actions, self()}).

log_message(Type, Msg) ->
	gen_server:cast(?SERVER, {log_message, Type, Msg}).

telemetry_event(Msg) ->
	gen_server:cast(?SERVER, {telemetry_event, Msg}).

publish_diagnostics(URI, Diagnostics) ->
	gen_server:cast(?SERVER, {publish_diagnostics, URI, Diagnostics}).

%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	process_flag(trap_exit, true),
	State = #state{
	  		internal_state = erlide_sense:init()
		},
	{ok, State}.

handle_call({'initialize', Id, ClientCapabilities}, 
			_From, State) ->
	{ServerCapabilities, NewState} = erlide_sense:initialize(State#state.internal_state, ClientCapabilities),
	reply(Id, ServerCapabilities),
	{reply, ok, State#state{internal_state=NewState}};
handle_call({'workspace/symbol', Id, #{query:=Query}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, workspace_symbol, Query, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/completion', Id, #{textDocument:=#{uri:=URI}, position:=Position}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, completion, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'completionItem/resolve', Id, CompletionItem},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, completion_resolve, CompletionItem, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/hover', Id, #{textDocument:=#{uri:=URI}, position:=Position}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, hover, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/references', Id, #{textDocument:=#{uri:=URI}, position:=Position, context:=Context}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, references, {URI, Position, Context}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/documentHighlight', Id, #{textDocument:=#{uri:=URI}, position:=Position}},
			_From, State = #state{pending_in_requests=Reqs})->
	Pid = start_worker(Id, document_highlight, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/documentSymbol', Id, #{textDocument:=#{uri:=URI}}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, document_symbol, URI, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/formatting', Id,  #{textDocument:=#{uri:=URI}, options:=Options}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, formatting, {URI, Options}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/rangeFormatting', Id, #{textDocument:=#{uri:=URI}, range:=Range, options:=Options}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, range_formatting, {URI, Range, Options}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/onTypeFormatting', Id, #{textDocument:=#{uri:=URI}, position:=Position, ch:=Ch, options:=Options}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, on_type_formatting, {URI, Position, Ch, Options}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/definition', Id, #{textDocument:=#{uri:=URI}, position:=Position}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, definition, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/signatureHelp', Id, #{textDocument:=#{uri:=URI}, position:=Position}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, signature_help, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/codeAction', Id, #{textDocument:=#{uri:=URI}, range:=Range, context:=Context}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, code_action, {URI, Range, Context}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/codeLens', Id, #{textDocument:=#{uri:=URI}}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, code_lens, URI, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'codeLens/resolve', Id, Item},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, code_lens_resolve, Item, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call({'textDocument/rename', Id, #{textDocument:=#{uri:=URI}, position:=Position, newName:=NewName}},
			_From, State = #state{pending_in_requests=Reqs}) ->
	Pid = start_worker(Id, rename, {URI, Position, NewName}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_in_requests=NewReqs}};
handle_call(Request,
			_From, State) ->
	io:format("HUH???... ~p~n", [Request]),
	Reply = {error, {unknown, Request}},
	{reply, Reply, State}.

handle_cast({'shutdown', _Id, _}, State) ->
	{noreply, State#state{stopped = true}};
handle_cast({'exit', _}, State) ->
	% TODO erlang:halt(),
	{stop, State};
handle_cast({'$/cancelRequest', #{id := Id}}, State) ->
	NewState = cancel_request(Id, State),
	{noreply, NewState};
handle_cast({'workspace/didChangeConfiguration', #{settings := Settings}} , State) ->
	NewState = erlide_sense:updated_configuration(State#state.internal_state, Settings),
	{noreply, State#state{internal_state=NewState}};
handle_cast({'workspace/didChangeWatchedFiles', #{changes := Changes}}, State) ->
	NewState = erlide_sense:updated_watched_files(State#state.internal_state, encode_file_changes(Changes)),
	{noreply, State#state{internal_state=NewState}};
handle_cast({'textDocument/didOpen', #{textDocument := Document}}, State) ->
	NewState = erlide_sense:opened_file(State#state.internal_state, Document),
	{noreply, State#state{internal_state=NewState}};
handle_cast({'textDocument/didChange', #{textDocument := VersionedDocument, contentChanges := Changes}}, State) ->
	NewState = erlide_sense:changed_file(State#state.internal_state, VersionedDocument, Changes),
	{noreply, State#state{internal_state=NewState}};
handle_cast({'textDocument/didSave', #{textDocument := DocumentId}}, State) ->
	NewState = erlide_sense:saved_file(State#state.internal_state, DocumentId),
	{noreply, State#state{internal_state=NewState}};
handle_cast({'textDocument/didClose', #{textDocument := DocumentId}}, State) ->
	NewState = erlide_sense:closed_file(State#state.internal_state, DocumentId),
	{noreply, State#state{internal_state=NewState}};
handle_cast({show_message, Type, Msg}, State) ->
	erlide_lsp_connection ! {notify, 'window/showMessage',
			 #{type => Type,
			   message => unicode:characters_to_binary(Msg)}},
	{noreply, State};
handle_cast({show_message_request, Type, Msg, Actions, Pid}, 
			State = #state{pending_out_requests=Reqs}) ->
	Id = State#state.crt_id,
	NewState = State#state{
						    pending_out_requests = [{Id, Pid} | Reqs],
						    crt_id = Id + 1
						},
	erlide_lsp_connection ! {request, Id, 'window/showMessageRequest',
			 #{type => Type,
			   message => unicode:characters_to_binary(Msg),
			   actions => Actions}
			},
	{noreply, NewState};
handle_cast({log_message, Type, Msg}, State) ->
	erlide_lsp_connection ! {notify, 'window/logMessage',
			 #{type => Type,
			   message => unicode:characters_to_binary(Msg)}},
	{noreply, State};
handle_cast({telemetry_event, Msg}, State) ->
	erlide_lsp_connection ! {notify, 'telemetry/event', Msg},
	{noreply, State};
handle_cast({publish_diagnostics, URI, Diagnostics}, State) ->
	erlide_lsp_connection ! {notify, 'textDocument/publishDiagnostics',
			 #{uri => URI,
			   diagnostics => Diagnostics}},
	{noreply, State};
handle_cast({'$reply', Id, Msg}, State) ->
	case lists:keytake(Id, 1, State#state.pending_out_requests) of
		false ->
			{noreply, State};
		{value, {Id, Pid}, Rest} ->
			Pid ! Msg,
			{noreply, State#state{pending_out_requests=Rest}}
	end;
handle_cast({_F, _A}=Other, State) ->
	%% unknown notification, ignore
	io:format("Unrecognized notification: ~p~n", [Other]),
	{noreply, State};
handle_cast({F, Id, A}, State) ->
	FN = atom_to_binary(F, latin1),
	AN = unicode:characters_to_binary(lists:flatten(io_lib:format("~p~n", [A]))),
	io:format("Unrecognized operation: ~p~n", [{FN, AN}]),
	reply(Id, #{error => #{code => method_not_found,
								    message => <<"Unrecognized method ", FN/binary,
											" called with ", AN/binary>>}}),
	{noreply, State};
handle_cast(Other, State) ->
	io:format("Unrecognized message: ~p~n", [Other]),
	{noreply, State}.

handle_info(_Info, State) ->
	io:format("Unrecognized message: ~p~n", [_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

reply(Id, Answer) ->
	erlide_lsp_connection ! {reply, Id, Answer}.

cancel_request(Id, #state{pending_in_requests=Reqs}=State) ->
	case lists:keytake(Id, 1, Reqs) of
		{value, {Id, Pid}, NewReqs} ->
			cancellable_worker:cancel(Pid),
			State#state{pending_in_requests=NewReqs};
		false ->
			State
	end.

start_worker(Id, Method, Params, State) ->
	Internal = State#state.internal_state,
	Work = fun(Reporter) ->
			erlide_sense:Method(Internal, Params, Reporter)
		end,
	Replier = fun({_, nothing}) ->
				reply(Id, erlide_sense:default_answer(Method));
			({_, Answer}) ->
				reply(Id, Answer)
		end,
	{ok, Pid} = cancellable_worker:start(Id, Work, Replier),
	Pid.

encode_file_changes(Changes) ->
	[encode_file_change(X) || X<-Changes].

encode_file_change(#{type:=1}=Change) ->
	Change#{type=>created};
encode_file_change(#{type:=2}=Change) ->
	Change#{type=>changed};
encode_file_change(#{type:=3}=Change) ->
	Change#{type=>deleted}.

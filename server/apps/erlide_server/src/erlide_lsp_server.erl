%% @author vlad
%% @doc @todo Add description to erlide_lsp_server.

-module(erlide_lsp_server).

-behaviour(gen_server).

-export([
		 start_link/1,
		 
		 show_message/2,
		 show_message_request/3,
		 log_message/2,
		 telemetry_event/1,
		 publish_diagnostics/2
		]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
				proxy,
				stopped = false,
				crt_id = 0,
				pending_reads = [],
				pending_writes = [],
				pending_requests = [],
				internal_state
			   }).

start_link(Port) ->
	{ok, Proxy} = erlide_lsp_proxy:start_link([?SERVER, Port]),
	gen_server:start_link({local, ?SERVER}, ?MODULE, Proxy, []).

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

init(Proxy) ->
	process_flag(trap_exit, true),
	State = #state{
			proxy = Proxy,
	  		internal_state = erlide_server_core:init()
		},
	{ok, State}.

handle_call(Request, _From, State) ->
	io:format("HUH???... ~p~n", [Request]),
	Reply = {error, {unknown, Request}},
	{reply, Reply, State}.

handle_cast({'initialize', Id, ClientCapabilities}, State) ->
	{ServerCapabilities, NewState} = erlide_server_core:initialize(State#state.internal_state, ClientCapabilities),
	reply(State#state.proxy, Id, ServerCapabilities),
	{noreply, State#state{internal_state=NewState}};
handle_cast({'shutdown', _Id, _}, State) ->
	{noreply, State#state{stopped = true}};
handle_cast({'exit', _}, State) ->
	% TODO erlang:halt(),
	{stop, State};
handle_cast({'$/cancelRequest', #{id := Id}}, State) ->
	NewState = cancel_read(Id, State),
	{noreply, NewState};
handle_cast({'workspace/didChangeConfiguration', #{settings := Settings}} , State) ->
	TmpState = cancel_all_pending_reads(State),
	NewState = erlide_server_core:updated_configuration(TmpState#state.internal_state, Settings),
	{noreply, TmpState#state{internal_state=NewState}};
handle_cast({'workspace/didChangeWatchedFiles', #{changes := Changes}}, State) ->
	TmpState = cancel_all_pending_reads(State),
	NewState = erlide_server_core:updated_watched_files(TmpState#state.internal_state, Changes),
	{noreply, TmpState#state{internal_state=NewState}};
handle_cast({'textDocument/didOpen', #{textDocument := Document}}, State) ->
	TmpState = cancel_all_pending_reads(State),
	NewState = erlide_server_core:opened_file(TmpState#state.internal_state, Document),	
	{noreply, TmpState#state{internal_state=NewState}};
handle_cast({'textDocument/didChange', #{textDocument := VersionedDocument, contentChanges := Changes}}, State) ->
	TmpState = cancel_all_pending_reads(State),
	NewState = erlide_server_core:changed_file(TmpState#state.internal_state, VersionedDocument, Changes),
	{noreply, TmpState#state{internal_state=NewState}};
handle_cast({'textDocument/didSave', #{textDocument := DocumentId}}, State) ->
	TmpState = cancel_all_pending_reads(State),
	NewState = erlide_server_core:saved_file(TmpState#state.internal_state, DocumentId),
	{noreply, TmpState#state{internal_state=NewState}};
handle_cast({'textDocument/didClose', #{textDocument := DocumentId}}, State) ->
	TmpState = cancel_all_pending_reads(State),
	NewState = erlide_server_core:closed_file(TmpState#state.internal_state, DocumentId),
	{noreply, TmpState#state{internal_state=NewState}};
handle_cast({'workspace/symbol', Id, #{query:=Query}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, workspace_symbol, Query, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/completion', Id, #{textDocument:=#{uri:=URI}, position:=Position}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, completion, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'completionItem/resolve', Id, CompletionItem}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, completion_resolve, CompletionItem, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/hover', Id, #{textDocument:=#{uri:=URI}, position:=Position}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, hover, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/references', Id, #{textDocument:=#{uri:=URI}, position:=Position, context:=Context}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, references, {URI, Position, Context}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/documentHighlight', Id, #{textDocument:=#{uri:=URI}, position:=Position}}, State = #state{pending_reads=Reqs})->
	Pid = start_worker(Id, document_highlight, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/documentSymbol', Id, #{textDocument:=#{uri:=URI}}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, document_symbol, URI, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/formatting', Id,  #{textDocument:=#{uri:=URI}, options:=Options}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, formatting, {URI, Options}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/rangeFormatting', Id, #{textDocument:=#{uri:=URI}, range:=Range, options:=Options}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, range_formatting, {URI, Range, Options}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/onTypeFormatting', Id, #{textDocument:=#{uri:=URI}, position:=Position, ch:=Ch, options:=Options}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, on_type_formatting, {URI, Position, Ch, Options}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/definition', Id, #{textDocument:=#{uri:=URI}, position:=Position}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, definition, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/signatureHelp', Id, #{textDocument:=#{uri:=URI}, position:=Position}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, signature_help, {URI, Position}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/codeAction', Id, #{textDocument:=#{uri:=URI}, range:=Range, context:=Context}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, code_action, {URI, Range, Context}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/codeLens', Id, #{textDocument:=#{uri:=URI}}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, code_lens, URI, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'codeLens/resolve', Id, Item}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, code_lens_resolve, Item, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({'textDocument/rename', Id, #{textDocument:=#{uri:=URI}, position:=Position, newName:=NewName}}, State = #state{pending_reads=Reqs}) ->
	Pid = start_worker(Id, rename, {URI, Position, NewName}, State),
	NewReqs = [{Id, Pid}|Reqs],
	{noreply, State#state{pending_reads=NewReqs}};
handle_cast({show_message, Type, Msg}, State = #state{proxy = Proxy}) ->
	Proxy ! {notify, 'window/showMessage',
			 #{type => Type,
			   message => unicode:characters_to_binary(Msg)}},
	{noreply, State};
handle_cast({show_message_request, Type, Msg, Actions, Pid}, State = #state{proxy = Proxy}) ->
	Id = State#state.crt_id,
	NewState = State#state{
						   pending_requests = [{Id, Pid} | State#state.pending_requests],
						   crt_id = Id + 1
						  },
	Proxy ! {request, Id, 'window/showMessageRequest',
			 #{type => Type,
			   message => unicode:characters_to_binary(Msg),
			   actions => Actions}
			},
	{noreply, NewState};
handle_cast({log_message, Type, Msg}, State = #state{proxy = Proxy}) ->
	Proxy ! {notify, 'window/logMessage',
			 #{type => Type,
			   message => unicode:characters_to_binary(Msg)}},
	{noreply, State};
handle_cast({telemetry_event, Msg}, State = #state{proxy = Proxy}) ->
	Proxy ! {notify, 'telemetry/event', Msg},
	{noreply, State};
handle_cast({publish_diagnostics, URI, Diagnostics}, State = #state{proxy = Proxy}) ->
	Proxy ! {notify, 'textDocument/publishDiagnostics',
			 #{uri => URI,
			   diagnostics => Diagnostics}},
	{noreply, State};
handle_cast({'$reply', Id, Msg}, State) ->
	case lists:keytake(Id, 1, State#state.pending_requests) of
		false ->
			{noreply, State};
		{value, {Id, Pid}, Rest} ->
			Pid ! Msg,
			{noreply, State#state{pending_requests=Rest}}
	end;
handle_cast({_F, _A}=Other, State) ->
	%% unknown notification, ignore
	io:format("Unrecognized notification  ~p~n", [Other]),
	{noreply, State};
handle_cast({F, Id, A}, State) ->
	FN = atom_to_binary(F, latin1),
	AN = unicode:characters_to_binary(lists:flatten(io_lib:format("~p~n", [A]))),
	io:format("Unrecognized operation ~p~n", [{FN, AN}]),
	reply(State, Id, #{error => #{code => method_not_found,
								  message => <<"Unrecognized method ", FN/binary,
											   " called with ", AN/binary>>}}),
	{noreply, State};
handle_cast(Other, State) ->
	io:format("Unrecognized message  ~p~n", [Other]),
	{noreply, State}.

handle_info(_Info, State) ->
	io:format("@@@ ~p~n", [_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

reply(Proxy, Id, Answer) ->
	Proxy ! {reply, Id, Answer}.

reply(Proxy, Id, undefined, DefaultAnswer) ->
	reply(Proxy, Id, DefaultAnswer);
reply(Proxy, Id, Answer, _DefaultAnswer) ->
	Proxy ! {reply, Id, Answer}.

cancel_read(Id, #state{pending_reads=Reqs}=State) ->
	case lists:keytake(Id, 1, Reqs) of
		{value, {Id, Pid}, NewReqs} ->
			Pid ! cancel,
			State#state{pending_reads=NewReqs};
		false ->
			State
	end.

cancel_all_pending_reads(#state{pending_reads=Reqs}=State) ->
	[cancel_read(X, State) || {X, _} <- Reqs],
	State#state{pending_reads=[]}.

start_worker(Id, Method, Params, State) ->
	spawn(fun() ->
				  Fun = fun(Reporter) ->
								Internal = State#state.internal_state,
								erlide_server_core:Method(Internal, Params, Reporter)
						end,
				  {ok, MonPid} = cancellable_worker:start(Fun),
				  DfltAnswer = erlide_server_core:default_answer(Method),
				  my_worker_loop(Id, State, MonPid, DfltAnswer)
		  end).

my_worker_loop(Id, State, MonPid, DfltAnswer) ->
	receive
		cancel ->
			{_, Result} = cancellable_worker:cancel(MonPid),
			reply(State#state.proxy, Id, Result, DfltAnswer)
	after 10 ->
		case cancellable_worker:check(MonPid) of
			{partial, _} ->
				my_worker_loop(Id, State, MonPid, DfltAnswer);
			{final, Result} ->
				reply(State#state.proxy, Id, Result)
		end
	end.


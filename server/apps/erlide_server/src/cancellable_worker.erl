-module(cancellable_worker).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([
		 start/1,
		 start/3,
		 cancel/1,
		 cancel/2,
		 yield/1,
		 yield/2,
		 check/1
		]).

%% Implement a worker process that can be cancelled and then may return a
%% partial answer.
%% The function doing the actual work takes as argument a Reporter function to
%% use to report results:
%% - Reporter(partial, Value) for a partial result
%% - Reporter(final, Value) for the whole result (if partial results are not
%%     possible); do not report this after any partial values
%% If partial results are sent, they are aggregated in a list, which is returned
start(WorkerFun) ->
	gen_server:start(?MODULE, WorkerFun, []).

start(Module, Function, Args) ->
	start(fun() -> apply(Module, Function, Args) end).

%% Check/1 checks if there are any answers from the worker. It can return
%% - {partial, Values} : the list of all currently reported values
%% - {final, Value} : the final result
%% - {error, {Value1, Value2}} : unexpected 'final' Value2 reported (either
%%     after another 'final' or after 'partial's Value1)
check(MonPid) when is_pid(MonPid) ->
	case is_process_alive(MonPid) of
		true ->
			gen_server:call(MonPid, check);
		false ->
			{error, noproc}
	end.

%% Cancels the worker and returns the current results.
cancel(MonPid) when is_pid(MonPid) ->
	case is_process_alive(MonPid) of
		true ->
			%% io:format("*** cancel worker ~p~n", [{MonPid}]),
			catch gen_server:call(MonPid, cancel);
		false ->
			{error, noproc}
	end.
cancel(MonPid, Timeout) when is_pid(MonPid) ->
	gen_server:call(MonPid, cancel, Timeout).

%% Wait until the the worker has finished and return the final result.
%% TODO don't return partial/final
yield(MonPid) when is_pid(MonPid) ->
	gen_server:call(MonPid, yield).
yield(MonPid, Timeout) when is_pid(MonPid) ->
	gen_server:call(MonPid, yield, Timeout).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {
				worker_pid,
				results = {partial, undefined},
				yielding = false
			   }).

init(WorkerFun) ->
	process_flag(trap_exit, true),
	Monitor = self(),
	Report = fun(partial, V) -> gen_server:cast(Monitor, {partial, V});
				(final, V) -> gen_server:cast(Monitor, {final, V})
			 end,
	{WrkPid, _Ref} = spawn_monitor(fun() ->
										   WorkerFun(Report)
								   end),
	{ok, #state{worker_pid=WrkPid}}.


handle_call(check, _From, State=#state{results=Results, worker_pid=Pid}) when is_pid(Pid) ->
	Reply = adjust(Results),
	{reply, Reply, State};
handle_call(check, _From, State=#state{results=Results}) ->
	{_, Reply} = adjust(Results),
	{reply, {final, Reply}, State};
handle_call(cancel, _From, State=#state{results=Results, worker_pid=Pid}) when is_pid(Pid) ->
	exit(Pid, kill),
	{_, Reply} = adjust(Results),
	{reply, {ok, Reply}, State};
handle_call(cancel, _From, State=#state{results=Results}) ->
	{_, Reply} = adjust(Results),
	{reply, {ok, Reply}, State};
handle_call(yield, _From, State=#state{worker_pid=false, results=Results}) ->
	{_, Reply} = adjust(Results),
	{stop, normal, {ok, Reply}, State};
handle_call(yield, From, State) ->
	{noreply, State#state{yielding=From}};
handle_call(Request, _From, State) ->
	io:format("HUH???... ~p~n", [Request]),
	Reply = {error, {unknown, Request}},
	{reply, Reply, State}.

handle_cast(V, State=#state{results=Results}) ->
	NewResults = merge_result(V, Results),
	{noreply, State#state{results=NewResults}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', _, process, Pid, _Reason},
			State=#state{worker_pid=Pid,
						 yielding=From,
						 results=Results}) when From /= false ->

	{_, Reply} = adjust(Results),
	gen_server:reply(From, {ok, Reply}),
	{noreply, State#state{worker_pid=false}};
handle_info({'DOWN', _, process, Pid, _Reason}, State=#state{worker_pid=Pid}) ->
	{noreply, State#state{worker_pid=false}};
handle_info(_Info, State) ->
	io:format("@@@ ~p~n", [_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

adjust({K, Results}=Arg) ->
	if is_list(Results) ->
		   {K, lists:reverse(Results)};
	   true ->
		   Arg
	end.

merge_result({final, V}, {partial, undefined}) ->
	{final, V};
merge_result({partial, V}, {partial, undefined}) ->
	{partial, [V]};
merge_result({final, V}, {partial, R}) ->
	{final, [V|R]};
merge_result({partial, V}, {partial, R}) ->
	{partial, [V|R]};
merge_result(_V, R) ->
	R.

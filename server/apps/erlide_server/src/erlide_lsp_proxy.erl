-module(erlide_lsp_proxy).

-export([
		 start_link/1
		]).

start_link(Args) ->
	Pid = spawn_link(fun() ->
				start(Args)
			end),
	{ok, Pid}.

start([Server, Port]) ->
	io:format("Start proxy on port ~w~n", [Port]),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, true}]),
	case gen_tcp:accept(LSock) of
		{ok, Socket} ->
			io:format("Listening on: ~p~n", [Socket]),
			loop(Socket, Server, <<"">>, 0);
		Err ->
			io:format("Connection error: ~p~n", [Err]),
		ok
	end.

loop(Socket, Server, Buf, N) ->
	receive
		{notify, Method, Params} ->
			notify(Socket, Method, Params),
			loop(Socket, Server, Buf, N);
		{request, Id, Method, Params} ->
			request(Socket, Id, Method, Params),
			loop(Socket, Server, Buf, N);
		{reply, Id, Answer} ->
			reply(Socket, Id, Answer),
			loop(Socket, Server, Buf, N);
		{tcp, Socket, Data} ->
			Buf2 = <<Buf/binary, Data/binary>>,
			case try_decode(Buf2, 0) of
				{ok, Msg, Buf3} ->
					process_message(Server, Msg),
					loop(Socket, Server, Buf3, N);
				more ->
					loop(Socket, Server, Buf2, N)
			end;
		{tcp_error, Socket, _Error} ->
			ok;
		{tcp_closed, Socket} ->
			ok;
		_Other ->
			io:format("LSP: unknown message: ~p~n", [_Other]),
			loop(Socket, Server, Buf, N)
	end.

try_decode(Buf, N) ->
	case erlang:decode_packet(httph, Buf, []) of
		{ok, {http_header, _, 'Content-Length', undefined, Len}, R} ->
			try_decode(R, list_to_integer(Len));
		{ok, {http_header, _, _, _, _}, R} ->
			try_decode(R, N);
		{ok, http_eoh, R} ->
			if size(R) < N ->
				   more;
			   true ->
				   <<D:N/binary, Rest/binary>> = R,
				   {ok, D, Rest}
			end;
		{more, _} ->
			more
	end.

process_message(Server, D) ->
	Req = parse(D),
	dispatch(Server, Req).

parse(Data) ->
	Req = decode(jsx:decode(Data, [return_maps])),
	%% 	io:format(">>> ~tp~n", [Req]),
	Req.

dispatch(Server, #{jsonrpc := <<"2.0">>,
				   id := Id,
				   result := Result
				  }) ->
	io:format(">>> RESULT ~p: ~tp~n", [Id, Result]),
	gen_server:cast(Server, {'$reply', Id, Result}),
	ok;
dispatch(Server, #{jsonrpc := <<"2.0">>,
				   id := Id,
				   error := Error
				  }) ->
	io:format(">>> RESULT ~p: ~tp~n", [Id, Error]),
	gen_server:cast(Server, {'$reply', Id, Error}),
	ok;
dispatch(Server, #{jsonrpc := <<"2.0">>,
				   id := Id,
				   method := Method0,
				   params := Params
				  }) ->
	Method = binary_to_atom(Method0, unicode),
	io:format("<<< REQUEST ~p: ~tp ~tp~n", [Id, Method, Params]),
	gen_server:cast(Server, {Method, Id, Params});
dispatch(Server, #{jsonrpc := <<"2.0">>,
				   id := Id,
				   method := Method0
				  }) ->
	Method = binary_to_atom(Method0, unicode),
	io:format("<<< REQUEST ~p: ~tp~n", [Id, Method]),
	gen_server:cast(Server, {Method, Id, none});
dispatch(Server, #{jsonrpc := <<"2.0">>,
				   method := Method0,
				   params := Params
				  }) ->
	Method = binary_to_atom(Method0, unicode),
	io:format("<<< NOTIFICATION ~tp ~tp~n", [Method, Params]),
	gen_server:cast(Server, {Method, Params});
dispatch(Server, #{jsonrpc := <<"2.0">>,
				   method := Method0
				  }) ->
	Method = binary_to_atom(Method0, unicode),
	io:format("<<< NOTIFICATION ~tp~n", [Method]),
	gen_server:cast(Server, {Method, none}).

send(Socket, Json0) ->
	Json = encode(Json0),
	Hdr = io_lib:format("Content-Length: ~w\r\n\r\n", [size(Json)]),
	%% 	io:format("<<<< ~ts~ts~n", [Hdr, Json]),
	gen_tcp:send(Socket, Hdr),
	gen_tcp:send(Socket, Json).

notify(Socket, Method, Params) ->
	io:format(">>> NOTIFY ~tp ~tp~n", [Method, Params]),
	Ans = #{jsonrpc => <<"2.0">>,
			method => Method,
			params => Params
		   },
	Json = jsx:encode(Ans),
	send(Socket, Json).

request(Socket, Id, Method, Params) ->
	io:format(">>> REQUEST ~p: ~tp ~tp~n", [Id, Method, Params]),
	Ans = #{jsonrpc => <<"2.0">>,
			id => Id,
			method => Method,
			params => Params
		   },
	Json = jsx:encode(Ans),
	send(Socket, Json).

reply(Socket, Id, Msg) when is_map(Msg); is_list(Msg); Msg==null ->
	io:format(">>> REPLY ~p: ~tp~n", [Id, Msg]),
	Ans = #{jsonrpc => <<"2.0">>,
			id => Id,
			result => Msg
		   },
	Json = jsx:encode(Ans),
	send(Socket, Json);
reply(Socket, Id, {error, Code0, Msg})  ->
	Code = error_code(Code0),
	io:format(">>> ERROR ~p: ~tp~n", [Id, Msg]),
	Ans = #{jsonrpc => <<"2.0">>,
			id => Id,
			error => #{
					   code => Code,
					   message => Msg
					  }
		   },
	Json = jsx:encode(Ans),
	send(Socket, Json);
reply(Socket, Id, Msg) ->
	io:format("Erroneous message ~tp~n",[{Socket, Id, Msg, erlang:get_stacktrace()}]),
	ok.

decode(Json) when is_map(Json) ->
	L1 = maps:to_list(Json),
	L2 = [{erlang:binary_to_atom(K, unicode), decode(V)} || {K, V} <- L1],
	maps:from_list(L2);
decode(Json) when is_list(Json) ->
	[decode(V) || V <- Json];
decode(V) ->
	V.

encode(Json) when is_map(Json) ->
	L1 = maps:to_list(Json),
	L2 = [{erlang:atom_to_binary(K, unicode), encode(V)} || {K, V} <- L1],
	maps:from_list(L2);
encode(Json) when is_list(Json) ->
	[encode(V) || V <- Json];
encode(V) ->
	V.

error_code(parse_error) -> -32700;
error_code(invalid_request) -> -32600;
error_code(method_not_found) -> -32601;
error_code(invalid_params) -> -32602;
error_code(internal_error) -> -32603;
error_code(server_error_start) -> -32099;
error_code(server_error_end) -> -32000;
error_code(N) when is_integer(N) -> N.

-module(erlide_server).

-export([main/1]).

-define(DEFAULT_PORT, 9000).

main(Args) ->
    io:format("Start erlide server. ~p~n", [Args]),
    case getopt:parse(cli_options(), Args) of
        {ok, {Opts, _Other}} ->
            io:format("    ~p~n", [Opts]),
            Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),

            ok = application:load(erlide_server),
            ok = application:set_env(erlide_server, port, Port),

            case application:ensure_all_started(erlide_server, permanent) of
                {ok, _R} ->
                    io:format("... ~p~n", [_R]),
                    receive stop -> ok end,
                    ok;
                _Err ->
                    io:format("??? ~p~n", [_Err]),
                    ok
            end;
        _Err ->
            io:format("Error: ~p~n", [_Err]),
            getopt:usage(cli_options(), "erlide_server")
    end,
    io:format("Finished-~n"),
    ok.

cli_options() ->
    [
     {port,    $p,        "port",    integer,               "LSP server port"},
     {verbose, $v,        "verbose", integer,               "Verbosity level"}
    ].
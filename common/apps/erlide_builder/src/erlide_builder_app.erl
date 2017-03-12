-module(erlide_builder_app).

-export([
     init/1
    ]).

init(MaxParallelBuilds) ->
  io:format("Start builder app~n"),
  spawn(fun()->
          erlide_pool:start(erlide_builder, MaxParallelBuilds),
          ok
      end),
  ok.

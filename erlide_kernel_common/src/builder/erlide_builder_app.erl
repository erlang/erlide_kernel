-module(erlide_builder_app).

-export([
     init/1
    ]).

init(MaxParallelBuilds) ->
  spawn(fun()->
          erlide_pool:start(erlide_builder, MaxParallelBuilds),
          ok
      end),
  ok.

-module(erlide_builder_app).

-export([
     init/0
    ]).

init() ->
  spawn(fun()->
          %% is started by erlide_common
          %% erlide_pool:start(erlide_builder),
          ok
      end),
  ok.

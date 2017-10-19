-module(mcache_app).

-behaviour(application).


-export([start/0, start/2, stop/1]).


start() ->
  mcache_sup:start_link().


start(_StartType, _StartArgs) ->
  mcache_sup:start_link().


stop(_State) ->
  ok.
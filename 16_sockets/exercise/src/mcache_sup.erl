-module(mcache_sup).

-behaviour(supervisor).


-export([start_link/0, init/1]).


start_link() ->
%%  io:format("supervisor:start_link~n", []),
  supervisor:start_link(?MODULE, []).


init(_Args) ->
  SupervisorSpecification = #{
    strategy => one_for_one,
    intensity => 10,
    period => 60},

  ChildSpecifications =
    [#{id => mcache_worker,
      start => {mcache_worker, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [mcache_worker]}
    ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.
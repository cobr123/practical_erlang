-module(sup_1).

-export([start_link/0, init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
  SupervisorSpecification = #{
    strategy => one_for_one,
    intensity => 10,
    period => 60},

  ChildSpecifications =
    [#{id => some_worker_1,
      start => {worker, start_link, [some_worker_1]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]},

      #{id => other_worker_1,
        start => {worker, start_link, [other_worker_1]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [worker]}
    ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.
-module(sup_2).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
  SupervisorSpecification = #{
    strategy => one_for_one,
    intensity => 10,
    period => 60},

  ChildSpecifications =
    [#{id => some_worker_2,
      start => {worker, start_link, [some_worker_2]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [worker]},

      #{id => other_worker_2,
        start => {worker, start_link, [other_worker_2]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [worker]}
    ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.


add_worker(WorkerID) ->
  supervisor:start_child(
    ?MODULE,
    {WorkerID,
      {worker, start_link, [WorkerID]},
      permanent,
      2000,
      worker,
      [worker]}).


remove_worker(WorkerID) ->
  supervisor:terminate_child(?MODULE, WorkerID),
  supervisor:delete_child(?MODULE, WorkerID).
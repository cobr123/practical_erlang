-module(my_crypt_sup).

-behaviour(supervisor).


-export([start_link/0, init/1]).


start_link() ->
  supervisor:start_link(?MODULE, []).


init(_Args) ->
  SupervisorSpecification = #{
    strategy => one_for_one,
    intensity => 10,
    period => 60},

  ChildSpecifications =
    [#{id => my_crypt,
      start => {my_crypt, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [my_crypt]}
    ],
  {ok, {SupervisorSpecification, ChildSpecifications}}.
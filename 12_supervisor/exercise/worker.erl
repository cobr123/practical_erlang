-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {worker_id}).


%%% module API


start_link(WorkerID) ->
  gen_server:start_link(?MODULE, [WorkerID], []).


ping(Pid) ->
  gen_server:call(Pid, {ping, Pid}).


%%% gen_server API


init([WorkerID]) ->
  {ok, #state{worker_id = WorkerID}}.


handle_call({ping, Pid}, _From, State) ->
  #state{worker_id = WorkerID} = State,
  {reply, {WorkerID, Pid}, State};

handle_call(Msg, {From, Tag}, State) ->
  io:format("ERROR: ~p receive unknown msg ~p~n from {~p,~p}", [self(), Msg, From, Tag]),
  {ok, State}.


handle_cast(Msg, State) ->
  io:format("ERROR: ~p receive unknown msg ~p~n", [self(), Msg]),
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).
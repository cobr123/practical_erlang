-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% module API


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_version() ->
  gen_server:call(?MODULE, {get_version}).


get_modules() ->
  gen_server:call(?MODULE, {get_modules}).


get_min_val() ->
  gen_server:call(?MODULE, {get_min_val}).


get_connection_timeout() ->
  gen_server:call(?MODULE, {get_connection_timeout}).


all_apps() ->
  gen_server:call(?MODULE, {all_apps}).


%%% gen_server API


init([]) ->
  {ok, []}.


handle_call({get_version}, _From, State) ->
  Map = lists:foldl(fun({Application, Description, Vsn}, Acc) ->
    maps:put(Application, #{description => Description, version => Vsn}, Acc)
                    end
    , maps:new()
    , application:which_applications()
  ),
  {ok, App} = application:get_application(),
  {reply, maps:get(version, maps:get(App, Map)), State};

handle_call({get_modules}, _From, State) ->
  {ok, Val} = application:get_key(modules),
  {reply, Val, State};

handle_call({get_min_val}, _From, State) ->
  {ok, Val} = application:get_env(min_val),
  {reply, Val, State};

handle_call({get_connection_timeout}, _From, State) ->
  {ok, Val} = application:get_env(connection_timeout),
  {reply, Val, State};

handle_call({all_apps}, _From, State) ->
  Map = lists:foldl(fun({Application, Description, Vsn}, Acc) ->
    maps:put(Application, #{description => Description, version => Vsn}, Acc)
                    end
    , maps:new()
    , application:which_applications()
  ),
  {reply, Map, State};

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
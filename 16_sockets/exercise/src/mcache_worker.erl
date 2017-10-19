-module(mcache_worker).
-behavior(gen_server).

-export([start_link/0, accept/1, handle_connection/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {listen_socket, storage}).


%%% module API


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


accept(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  handle_connection(ListenSocket, Socket).


handle_connection(ListenSocket, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Msg} ->
%%      io:format("Socket #~p got message: ~p~n", [Socket, Msg]),
      MsgWoNewLine = binary:part(Msg, 0, byte_size(Msg) - 2),
      {ok, Reply} = gen_server:call(?MODULE, {handle_msg, MsgWoNewLine}),
%%      io:format("Socket #~p send reply: ~p~n", [Socket, Reply]),
      gen_tcp:send(Socket, <<Reply/binary, "\r\n">>),
      handle_connection(ListenSocket, Socket);
    {error, closed} ->
      accept(ListenSocket)
  end.


%%% gen_server API


init(_Args) ->
  Port = 1234,
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
  InitState = #state{listen_socket = ListenSocket, storage = maps:new()},
  [spawn(?MODULE, accept, [ListenSocket]) || _Id <- lists:seq(1, 5)],
  {ok, InitState}.


handle_call({handle_msg, Msg}, _From, #state{storage = Storage} = State) ->
  {NewStorage, Reply} = handle_msg(Msg, Storage),
  NewState = State#state{storage = NewStorage},
  {reply, {ok, Reply}, NewState};

handle_call(Msg, {From, Tag}, State) ->
  io:format("ERROR: ~p receive unknown msg ~p~n from {~p,~p}", [self(), Msg, From, Tag]),
  {ok, State}.


handle_cast(Msg, State) ->
  io:format("ERROR in handle_cast: ~p receive unknown msg ~p~n", [self(), Msg]),
  {noreply, State}.


handle_info(Info, State) ->
  io:format("ERROR in handle_info: ~p receive unknown info ~p~n", [self(), Info]),
  {noreply, State}.


terminate(_Reason, State) ->
  ok.


code_change(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).


%%% internals


handle_msg(Msg, Storage) ->
  try
    [Cmd, Tail] = binary:split(Msg, [<<" ">>], []),
    handle_cmd(Cmd, Tail, Storage)
  catch
    error:_ -> {Storage, <<"UNKNOWN REQUEST">>}
  end.


handle_cmd(<<"SET">>, Tail, Storage) ->
  [Key, Val] = binary:split(Tail, [<<" ">>], []),
  NewStorage = maps:put(Key, Val, Storage),
  {NewStorage, <<"STORED">>};

handle_cmd(<<"GET">>, Key, Storage) ->
  case maps:find(Key, Storage) of
    {ok, Val} -> {Storage, <<"VALUE", " ", Key/binary, " ", Val/binary, "\r\nEND">>};
    error -> {Storage, <<"NOT FOUND">>}
  end;

handle_cmd(<<"GETS">>, Keys, Storage) ->
  KeyList = binary:split(Keys, [<<" ">>], [global]),
  BinList = lists:foldl(fun(Key, Acc) ->
    ValStr = case maps:find(Key, Storage) of
               {ok, Val} -> <<"VALUE", " ", Key/binary, " ", Val/binary, "\r\n">>;
               error -> <<"VALUE", " ", Key/binary, " ", "NOT FOUND", "\r\n">>
             end,
    [ValStr | Acc]
                        end
    , []
    , KeyList
  ),
  {Storage, list_to_binary(lists:reverse([<<"END">> | BinList]))};

handle_cmd(<<"DELETE">>, Key, Storage) ->
  case maps:find(Key, Storage) of
    {ok, _Val} ->
      NewStorage = maps:remove(Key, Storage),
      {NewStorage, <<"DELETED">>};
    error -> {Storage, <<"NOT FOUND">>}
  end;

handle_cmd(<<"ADD">>, Tail, Storage) ->
  [Key, NewVal] = binary:split(Tail, [<<" ">>], []),
  case maps:find(Key, Storage) of
    {ok, _Val} -> {Storage, <<"EXISTS">>};
    error ->
      NewStorage = maps:put(Key, NewVal, Storage),
      {NewStorage, <<"STORED">>}
  end;

handle_cmd(<<"REPLACE">>, Tail, Storage) ->
  [Key, NewVal] = binary:split(Tail, [<<" ">>], []),
  case maps:find(Key, Storage) of
    {ok, _Val} ->
      NewStorage = maps:put(Key, NewVal, Storage),
      {NewStorage, <<"STORED">>};
    error -> {Storage, <<"NOT FOUND">>}
  end;

handle_cmd(<<"APPEND">>, Tail, Storage) ->
  [Key, NewVal] = binary:split(Tail, [<<" ">>], []),
  case maps:find(Key, Storage) of
    {ok, Val} ->
      NewStorage = maps:put(Key, <<Val/binary, NewVal/binary>>, Storage),
      {NewStorage, <<"STORED">>};
    error -> {Storage, <<"NOT FOUND">>}
  end;

handle_cmd(<<"PREPEND">>, Tail, Storage) ->
  [Key, NewVal] = binary:split(Tail, [<<" ">>], []),
  case maps:find(Key, Storage) of
    {ok, Val} ->
      NewStorage = maps:put(Key, <<NewVal/binary, Val/binary>>, Storage),
      {NewStorage, <<"STORED">>};
    error -> {Storage, <<"NOT FOUND">>}
  end;

handle_cmd(_Cmd, _Tail, Storage) ->
  {Storage, <<"UNKNOWN REQUEST">>}.

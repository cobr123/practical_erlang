-module(my_crypt).
-behavior(gen_server).

-export([start_link/0, hash/1, encode/1, get_key/0, set_key/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% module API


start_link() ->
  {ok, BinKey} = application:get_env(crypt_key),
  gen_server:start_link({local, ?MODULE}, ?MODULE, BinKey, []).


hash(BinMsg) ->
  gen_server:call(?MODULE, {hash, BinMsg}).


encode(BinMsg) ->
  gen_server:call(?MODULE, {encode, BinMsg}).


get_key() ->
  gen_server:call(?MODULE, {get_key}).


set_key(BinKey) ->
  gen_server:call(?MODULE, {set_key, BinKey}).


%%% gen_server API


init(BinKey) ->
  {ok, BinKey}.


handle_call({hash, BinMsg}, _From, State) ->
  {ok, HashSize} = application:get_env(hash_size),
  {reply, hash(BinMsg, get_table(size(BinMsg)), HashSize), State};

handle_call({encode, BinMsg}, _From, State) ->
  BinKey = State,
  {reply, encode(BinMsg, BinKey), State};

handle_call({get_key}, _From, State) ->
  BinKey = State,
  {reply, BinKey, State};

handle_call({set_key, BinKey}, _From, _State) ->
  NewState = BinKey,
  {reply, ok, NewState};


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


%%% internals


hash(BinMsg, TableList, HashSize) ->
  BinMsgList = binary:bin_to_list(BinMsg),
  ResList = lists:foldl(fun(Iter, Acc) ->
    X0 = lists:nth(1, BinMsgList),
    Hash = lists:nth(((X0 + Iter) div 256) + 1, TableList),
    NewHash = lists:foldl(fun(Idx, AccHash) ->
      Xi = lists:nth(Idx, BinMsgList),
      lists:nth((AccHash bxor Xi) + 1, TableList)
                end, Hash, lists:seq(1, size(BinMsg))
    ),
    [NewHash | Acc]
              end, [], lists:seq(0, HashSize - 1)
  ),
  binary:list_to_bin(lists:reverse(ResList)).


get_table(Seed) ->
  random:seed(Seed),
  lists:foldl(fun(_, Acc) -> [random:uniform(256) | Acc] end, [], lists:seq(1, 256)).


encode(BinMsg, BinKey) ->
  BinKeyListSameSize = expand_list(binary:bin_to_list(BinKey), size(BinMsg)),
  BinMsgList = binary:bin_to_list(BinMsg),
  Zip = lists:zip(BinMsgList, BinKeyListSameSize),
  ResList = lists:foldl(fun({MsgElem, KeyElem}, Acc) -> [MsgElem bxor KeyElem | Acc] end, [], Zip),
  binary:list_to_bin(lists:reverse(ResList)).


expand_list(Orig, Len) ->
  if Len > length(Orig) -> expand_list([], Orig, Orig, Len);
    true -> lists:sublist(Orig, Len)
  end.


expand_list(Acc, _OrigLeft, _Orig, 0) -> lists:reverse(Acc);
expand_list(_Acc, _OrigLeft, [], _Len) -> [];
expand_list(Acc, [], Orig, Len) -> expand_list(Acc, Orig, Orig, Len);
expand_list(Acc, OrigLeft, Orig, Len) ->
  [Head | Tail] = OrigLeft,
  expand_list([Head | Acc], Tail, Orig, Len - 1).
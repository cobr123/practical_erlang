-module(main).

-export([parse/1]).


parse(Files) ->
  process_flag(trap_exit, true),
  Self = self(),
  FileRefFromPid = lists:foldl(fun(File, Acc) ->
    Ref = make_ref(),
    Param = {File, Ref, spawn_link(worker, get_data, [Ref, Self, File])},
    [Param | Acc] end
    , []
    , Files
  ),
  lists:foldl(fun({File, Ref, FromPid}, {ResAcc, ErrAcc}) ->
    receive
      {'EXIT', FromPid, normal} -> {ResAcc, ErrAcc};
      {'EXIT', FromPid, Reason} -> {ResAcc, maps:put(File, Reason, ErrAcc)};
      {result, Ref, List} -> {add_to_res(List, ResAcc), ErrAcc}
    end
              end
    , {maps:new(), maps:new()}
    , FileRefFromPid
  ).


add_to_res(List, Map) ->
  lists:foldl(fun({K, V}, Acc) ->
    case maps:find(K, Acc) of
      {ok, Value} -> maps:put(K, V + Value, Acc);
      error -> maps:put(K, V, Acc)
    end
              end
    , Map
    , List
  ).
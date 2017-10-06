-module(map_reduce).

-export([start/1]).


start(Files) ->
  %Self = self(),
  %Pids = [spawn(fun() -> Self ! readlines(File) end) || File <- Files],
  Words = lists:foldl(fun(File, Acc) -> lists:append(Acc, readlines(File)) end, [], Files),
  lists:foldl(fun(Word, Map) ->
    BinWord = binary:list_to_bin(Word),
    case maps:find(BinWord, Map) of
      {ok, Cnt} -> maps:put(BinWord, Cnt + 1, Map);
      error -> maps:put(BinWord, 1, Map)
    end
              end
    , maps:new()
    , Words
  ).

readlines(FileName) ->
  case file:read_file(FileName) of
    {ok, Data} -> re:split(binary:bin_to_list(Data), "[\t\s\n\r]", [{return, list}]);
    {error, _} -> []
  end.

-module(worker).

-export([get_data/3]).


get_data(Ref, FromPid, FileName) ->
  Lines = readlines(FileName),
  Vals = lists:foldl(fun(Line, Acc) -> [parse_line(Line) | Acc] end
    , []
    , Lines
  ),
  FromPid ! {result, Ref, Vals},
  ok.


parse_line(Line) ->
  [_, BinName, BinCnt, _] = binary:split(Line, [<<",">>], [global]),
  Cnt = binary_to_integer(BinCnt),
  {BinName, Cnt}.


readlines(FileName) ->
  {ok, Data} = file:read_file(FileName),
  binary:split(Data, [<<"\r">>, <<"\n">>], [global, trim_all]).
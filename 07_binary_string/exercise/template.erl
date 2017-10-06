-module(template).

-export([parse/2]).

val_to_bin(Val) when is_integer(Val) -> integer_to_binary(Val);
val_to_bin(Val) when is_list(Val) -> list_to_binary(Val);
val_to_bin(Val) -> Val.


remove_templates(BinStr) ->
  BeginList = binary:split(BinStr, [<<"{{">>]),
  EndList = binary:split(BinStr, [<<"}}">>]),
  if length(BeginList) =:= 2 andalso length(EndList) =:= 2 -> remove_templates(iolist_to_binary([lists:nth(1, BeginList), lists:nth(2, EndList)]));
  true -> BinStr
  end.


parse(Str, Data) when is_binary(Str) ->
  ResBinStr = lists:foldl(fun({Key, Val}, BinStr) -> binary:replace(BinStr, iolist_to_binary(["{{", Key, "}}"]), val_to_bin(Val), [global]) end
    , Str
    , maps:to_list(Data)
  ),
  remove_templates(ResBinStr).

-module(task_5).

-export([splitwith/2, zipwith/3]).

-include_lib("eunit/include/eunit.hrl").


%% implement lists:splitwith/2
%% http://www.erlang.org/doc/man/lists.html#splitwith-2
splitwith(_, []) -> {[], []};
splitwith(Pred, List) -> splitwith(Pred, List, []).

splitwith(_, [], Acc) -> {Acc, []};
splitwith(Pred, List, Acc) ->
  [Head | Tail] = List,
  case Pred(Head) of
    true -> splitwith(Pred, Tail, [Head | Acc]);
    false -> {task_2:reverse(Acc), List}
  end.


splitwith_test() ->
  F = fun(Val) -> Val rem 2 =:= 0 end,
  ?assertEqual({[], []}, splitwith(F, [])),
  ?assertEqual({[], [1]}, splitwith(F, [1])),
  ?assertEqual({[], [1, 2]}, splitwith(F, [1, 2])),
  ?assertEqual({[], [1, 2, 3]}, splitwith(F, [1, 2, 3])),
  ?assertEqual({[4, 6, 8], [1, 2, 3]}, splitwith(F, [4, 6, 8, 1, 2, 3])),
  ?assertEqual({[2], [3, 4, 5]}, splitwith(F, [2, 3, 4, 5])),
  ok.


%% implement lists:zipwith/3
%% http://www.erlang.org/doc/man/lists.html#zipwith-3
%% if two lists have different lengths don't throw exception but ignore the rest of longer list
zipwith(_, [], _) -> [];
zipwith(_, _, []) -> [];
zipwith(Pred, List1, List2) -> zipwith(Pred, List1, List2, []).

zipwith(_, [], _, Acc) -> task_2:reverse(Acc);
zipwith(_, _, [], Acc) -> task_2:reverse(Acc);
zipwith(Pred, List1, List2, Acc) ->
  [Head1 | Tail1] = List1,
  [Head2 | Tail2] = List2,
  zipwith(Pred, Tail1, Tail2, [Pred(Head1, Head2) | Acc]).


zipwith_test() ->
  F = fun(Val1, Val2) -> Val1 + Val2 end,
  ?assertEqual([7, 7, 7], zipwith(F, [1, 2, 3], [6, 5, 4])),
  ?assertEqual([], zipwith(F, [], [])),
  ?assertEqual([4], zipwith(F, [1], [3])),
  ?assertEqual([4, 6], zipwith(F, [1, 2], [3, 4])),
  ?assertEqual([2, 4, 6, 8, 10], zipwith(F, [1, 2, 3, 4, 5], [1, 2, 3, 4, 5])),
  ?assertEqual([2, 4, 6, 8], zipwith(F, [1, 2, 3, 4], [1, 2, 3, 4, 5, 6])),
  ?assertEqual([2, 4, 6, 8], zipwith(F, [1, 2, 3, 4, 5, 6], [1, 2, 3, 4])),
  ok.

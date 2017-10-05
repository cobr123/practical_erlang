-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
  {{f, f, f},
    {f, f, f},
    {f, f, f}}.


win(GameState) ->
  case GameState of
    {{x, _, _},
      {_, x, _},
      {_, _, x}} -> {win, x};
    {{_, _, x},
      {_, x, _},
      {x, _, _}} -> {win, x};

    {{x, _, _},
      {x, _, _},
      {x, _, _}} -> {win, x};
    {{_, x, _},
      {_, x, _},
      {_, x, _}} -> {win, x};
    {{_, _, x},
      {_, _, x},
      {_, _, x}} -> {win, x};

    {{x, x, x},
      {_, _, _},
      {_, _, _}} -> {win, x};
    {{_, _, _},
      {x, x, x},
      {_, _, _}} -> {win, x};
    {{_, _, _},
      {_, _, _},
      {x, x, x}} -> {win, x};

    {{o, _, _},
      {_, o, _},
      {_, _, o}} -> {win, o};
    {{_, _, o},
      {_, o, _},
      {o, _, _}} -> {win, o};

    {{o, _, _},
      {o, _, _},
      {o, _, _}} -> {win, o};
    {{_, o, _},
      {_, o, _},
      {_, o, _}} -> {win, o};
    {{_, _, o},
      {_, _, o},
      {_, _, o}} -> {win, o};

    {{o, o, o},
      {_, _, _},
      {_, _, _}} -> {win, o};
    {{_, _, _},
      {o, o, o},
      {_, _, _}} -> {win, o};
    {{_, _, _},
      {_, _, _},
      {o, o, o}} -> {win, o};
    
    {{_, _, _},
      {_, _, _},
      {_, _, _}} -> no_win
  end.


move(Cell, _, _) when Cell < 1 orelse Cell > 9 -> {error, invalid_move};
move(Cell, Player, GameState) ->
  case Cell of
    1 -> case GameState of
           {{f, C2, C3}, {C4, C5, C6}, {C7, C8, C9}} -> {ok, {{Player, C2, C3}, {C4, C5, C6}, {C7, C8, C9}}};
           {{_, _, _}, {_, _, _}, {_, _, _}} -> {error, invalid_move}
         end;
    2 -> case GameState of
           {{C1, f, C3}, {C4, C5, C6}, {C7, C8, C9}} -> {ok, {{C1, Player, C3}, {C4, C5, C6}, {C7, C8, C9}}};
           {{_, _, _}, {_, _, _}, {_, _, _}} -> {error, invalid_move}
         end;
    3 -> case GameState of
           {{C1, C2, f}, {C4, C5, C6}, {C7, C8, C9}} -> {ok, {{C1, C2, Player}, {C4, C5, C6}, {C7, C8, C9}}};
           {{_, _, _}, {_, _, _}, {_, _, _}} -> {error, invalid_move}
         end;

    4 -> case GameState of
           {{C1, C2, C3}, {f, C5, C6}, {C7, C8, C9}} -> {ok, {{C1, C2, C3}, {Player, C5, C6}, {C7, C8, C9}}};
           {{_, _, _}, {_, _, _}, {_, _, _}} -> {error, invalid_move}
         end;
    5 -> case GameState of
           {{C1, C2, C3}, {C4, f, C6}, {C7, C8, C9}} -> {ok, {{C1, C2, C3}, {C4, Player, C6}, {C7, C8, C9}}};
           {{_, _, _}, {_, _, _}, {_, _, _}} -> {error, invalid_move}
         end;
    6 -> case GameState of
           {{C1, C2, C3}, {C4, C5, f}, {C7, C8, C9}} -> {ok, {{C1, C2, C3}, {C4, C5, Player}, {C7, C8, C9}}};
           {{_, _, _}, {_, _, _}, {_, _, _}} -> {error, invalid_move}
         end;

    7 -> case GameState of
           {{C1, C2, C3}, {C4, C5, C6}, {f, C8, C9}} -> {ok, {{C1, C2, C3}, {C4, C5, C6}, {Player, C8, C9}}};
           {{_, _, _}, {_, _, _}, {_, _, _}} -> {error, invalid_move}
         end;
    8 -> case GameState of
           {{C1, C2, C3}, {C4, C5, C6}, {C7, f, C9}} -> {ok, {{C1, C2, C3}, {C4, C5, C6}, {C7, Player, C9}}};
           {{_, _, _}, {_, _, _}, {_, _, _}} -> {error, invalid_move}
         end;
    9 -> case GameState of
           {{C1, C2, C3}, {C4, C5, C6}, {C7, C8, f}} -> {ok, {{C1, C2, C3}, {C4, C5, C6}, {C7, C8, Player}}};
           {{_, _, _}, {_, _, _}, {_, _, _}} -> {error, invalid_move}
         end
  end.

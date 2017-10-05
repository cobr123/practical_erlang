-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
  %% init randomizer
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A, B, C}),
  State = {maps:new(), maps:new()},
  State.


create_short(LongLink, State) ->
  {LongAsKey, ShortAsKey} = State,
  case maps:find(LongLink, LongAsKey) of
    {ok, ShortLink} -> {ShortLink, State};
    error -> ShortLink = gen_short(LongLink, ShortAsKey),
      NewLongLink = maps:put(LongLink, ShortLink, LongAsKey),
      NewShortAsKey = maps:put(ShortLink, LongLink, ShortAsKey),
      {ShortLink, {NewLongLink, NewShortAsKey}}
  end.


get_long(ShortLink, State) ->
  {_, ShortAsKey} = State,
  case maps:find(ShortLink, ShortAsKey) of
    {ok, LongLink} -> {ok, LongLink};
    error -> {error, not_found}
  end.


gen_short(LongLink, ShortAsKey) ->
  ShortLink = rand_str(length(LongLink)),
  case maps:find(ShortLink, ShortAsKey) of
    {ok, _} -> gen_short(LongLink, ShortAsKey);
    error -> ShortLink
  end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
  lists:map(fun(Char) when Char > 83 -> Char + 13;
    (Char) when Char > 57 -> Char + 7;
    (Char) -> Char
            end,
    [crypto:rand_uniform(48, 110) || _ <- lists:seq(1, Length)]).

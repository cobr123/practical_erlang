-module(url_parser).

-export([parse/1]).


-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
  try
    [Protocol, Others] = extract_protocol(URL),
    [DomainAndPath, Query] = extract_query(Others),
    [Domain | Path] = binary:split(DomainAndPath, [<<"/">>], [global, trim_all]),
    {ok,
      #{protocol => Protocol,
        domain => Domain,
        path => Path,
        query => Query,
        date => extract_date(Path)
      }
    }
  catch
    throw:Reason -> {error, Reason}
  end.


extract_protocol(URL) ->
  case binary:split(URL, [<<"://">>], [trim_all]) of
    [Protocol, Others] -> [Protocol, Others];
    _ -> case binary:match(URL, [<<"://">>], []) of
           nomatch -> throw(invalid_protocol);
           _ -> throw(invalid_domain)
         end
  end.


extract_query(URL) ->
  case binary:split(URL, [<<"?">>], [trim_all]) of
    [DomainAndPath, Query] -> [DomainAndPath, Query];
    _ -> [URL, <<>>]
  end.


extract_date(Path) ->
  case Path of
    [Year, Month, Day, _] -> extract_date(binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day));
    _ -> undefined
  end.


extract_date(Year, Month, Day) ->
  if Month >= 1 andalso Month =< 12 andalso Day >= 1 andalso Day =< 31
    -> {Year, Month, Day};
    true -> undefined
  end.
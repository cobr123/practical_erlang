-module(chat_room_manager).

-export([start/0,
  create_room/2, remove_room/2, get_rooms/1,
  add_user/3, remove_user/3, get_users_list/2,
  send_message/4, get_messages_history/2]).

-export([loop/1, handle_call/2]).

-record(server_state, {rooms}).
-record(room, {name, users, history}).

start() ->
  InitState = #server_state{rooms = maps:new()},
  spawn(?MODULE, loop, [InitState]).


loop(State) ->
  receive
    {call, Ref, From, Msg} -> {Reply, NewState} = handle_call(Msg, State),
      From ! {reply, Ref, Reply},
      ?MODULE:loop(NewState);
    stop -> ok;
    Msg -> io:format("ERROR: server ~p receive unknown msg ~p~n", [self(), Msg]),
      ?MODULE:loop(State)
  end.


handle_call({create_room, RoomName}, State) ->
  #server_state{rooms = Rooms} = State,
  Size = maps:size(Rooms),
  if Size >= 5 -> {{error, room_limit}, State};
    true -> NewRoom = #room{name = RoomName, users = [], history = []},
      RoomId = make_ref(),
      NewRooms = maps:put(RoomId, NewRoom, Rooms),
      NewState = #server_state{rooms = NewRooms},
      {{ok, RoomId}, NewState}
  end;

handle_call({remove_room, RoomId}, State) ->
  #server_state{rooms = Rooms} = State,
  case maps:find(RoomId, Rooms) of
    {ok, _} ->
      NewRooms = maps:remove(RoomId, Rooms),
      NewState = #server_state{rooms = NewRooms},
      {ok, NewState};
    error -> {{error, room_not_found}, State}
  end;

handle_call({get_rooms}, State) ->
  #server_state{rooms = Rooms} = State,
  {maps:fold(fun(K, #room{name = Name}, Acc) -> [{K, Name} | Acc] end, [], Rooms), State};

handle_call({add_user, RoomId, UserName}, State) ->
  #server_state{rooms = Rooms} = State,
  case maps:find(RoomId, Rooms) of
    {ok, Room} -> #room{users = Users} = Room,
      UserExist = lists:member(UserName, Users),
      if UserExist -> {{error, user_is_in_room}, State};
        true ->
          NewRoom = Room#room{users = [UserName | Users]},
          NewRooms = maps:put(RoomId, NewRoom, Rooms),
          NewState = #server_state{rooms = NewRooms},
          {ok, NewState}
      end;
    error -> {{error, room_not_found}, State}
  end;

handle_call({remove_user, RoomId, UserName}, State) ->
  #server_state{rooms = Rooms} = State,
  case maps:find(RoomId, Rooms) of
    {ok, Room} -> #room{users = Users} = Room,
      UserExist = lists:member(UserName, Users),
      if UserExist ->
        NewRoom = Room#room{users = lists:delete(UserName, Users)},
        NewRooms = maps:put(RoomId, NewRoom, Rooms),
        NewState = #server_state{rooms = NewRooms},
        {ok, NewState};
        true -> {{error, user_not_in_room}, State}
      end;
    error -> {{error, room_not_found}, State}
  end;

handle_call({get_users_list, RoomId}, State) ->
  #server_state{rooms = Rooms} = State,
  case maps:find(RoomId, Rooms) of
    {ok, #room{users = Users}} -> {{ok, Users}, State};
    error -> {{error, room_not_found}, State}
  end;

handle_call({send_message, RoomId, UserName, Message}, State) ->
  #server_state{rooms = Rooms} = State,
  case maps:find(RoomId, Rooms) of
    {ok, Room} -> #room{users = Users, history = History} = Room,
      UserExist = lists:member(UserName, Users),
      if UserExist ->
        NewRoom = Room#room{history = [{UserName, Message} | History]},
        NewRooms = maps:put(RoomId, NewRoom, Rooms),
        NewState = #server_state{rooms = NewRooms},
        {ok, NewState};
        true -> {{error, user_not_in_room}, State}
      end;
    error -> {{error, room_not_found}, State}
  end;

handle_call({get_messages_history, RoomId}, State) ->
  #server_state{rooms = Rooms} = State,
  case maps:find(RoomId, Rooms) of
    {ok, #room{history = History}} -> {{ok, History}, State};
    error -> {{error, room_not_found}, State}
  end.


call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {call, Ref, self(), Msg},
  receive
    {reply, Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    erlang:demonitor(Ref, [flush]),
    noreply
  end.


create_room(Server, RoomName) ->
  call(Server, {create_room, RoomName}).


remove_room(Server, RoomId) ->
  call(Server, {remove_room, RoomId}).


get_rooms(Server) ->
  call(Server, {get_rooms}).


add_user(Server, RoomId, UserName) ->
  call(Server, {add_user, RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
  call(Server, {remove_user, RoomId, UserName}).


get_users_list(Server, RoomId) ->
  call(Server, {get_users_list, RoomId}).


send_message(Server, RoomId, UserName, Message) ->
  call(Server, {send_message, RoomId, UserName, Message}).


get_messages_history(Server, RoomId) ->
  call(Server, {get_messages_history, RoomId}).

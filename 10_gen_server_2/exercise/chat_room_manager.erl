-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
  create_room/1, get_rooms/0,
  add_user/3, remove_user/2, get_users/1,
  send_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% rooms = #{Pid => Name}
-record(state, {rooms}).


%%% gen_server API


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
  {ok, #state{rooms = maps:new()}}.


%% создает новую комнату. Принимает имя комнаты (binary) и возвращает комнату в виде кортежа {Name, Pid}.
handle_call({create_room, RoomName}, _From, State) ->
  #state{rooms = Rooms} = State,
  {ok, RoomPid} = chat_room:start_link(),
  NewRooms = maps:put(RoomPid, RoomName, Rooms),
  NewState = State#state{rooms = NewRooms},
  {reply, {RoomName, RoomPid}, NewState};

%% возвращает список комнат, где каждая комната представлена кортежем {Name, Pid}. Комнаты в списке могут идти в произвольном порядке.
handle_call({get_rooms}, _From, State) ->
  #state{rooms = Rooms} = State,
  {reply, maps:fold(fun(Pid, Name, Acc) -> [{Name, Pid} | Acc] end, [], Rooms), State};

%% добавляет нового пользователя в комнату. Принимает pid комнаты, имя пользователя (binary) и pid пользователя.
%% Возвращает атом ok, если выполнилась успешно. Или возвращет кортеж {error, room_not_found}, если заданая комната не найдена.
handle_call({add_user, RoomPid, UserName, UserPid}, _From, State) ->
  #state{rooms = Rooms} = State,
  case maps:find(RoomPid, Rooms) of
    {ok, _RoomName} -> chat_room:add_user(RoomPid, UserName, UserPid),
      {reply, ok, State};
    error -> {reply, {error, room_not_found}, State}
  end;

%% удаляет пользователя из комнаты. Принимает pid комнаты и pid пользователя.
%% Возвращает ok, если пользователь успешно удален, или {error, user_not_found}, если такого пользователя в комнате не было,
%% или {error, room_not_found}, если заданая комната не найдена.
handle_call({remove_user, RoomPid, UserPid}, _From, State) ->
  #state{rooms = Rooms} = State,
  case maps:find(RoomPid, Rooms) of
    {ok, _RoomName} -> Reply = chat_room:remove_user(RoomPid, UserPid),
      {reply, Reply, State};
    error -> {reply, {error, room_not_found}, State}
  end;

%% отдает список пользователей в комнате. Принимает pid комнаты.
%% В успешном случае возвращает кортеж {ok, Users}, Где Users -- список пользователей, представленых кортежем вида {Name, Pid}.
%% Или возвращет кортеж {error, room_not_found}, если заданая комната не найдена.
handle_call({get_users, RoomPid}, _From, State) ->
  #state{rooms = Rooms} = State,
  case maps:find(RoomPid, Rooms) of
    {ok, _RoomName} -> Users = chat_room:get_users(RoomPid),
      {reply, {ok, Users}, State};
    error -> {reply, {error, room_not_found}, State}
  end;

%% посылает новое сообщение в комнату. Принимает pid комнаты, имя автора сообщения (binary) и текст сообщения (binary).
%% В случае успеха возвращает атом ok. Или возвращет кортеж {error, room_not_found}, если заданая комната не найдена.
handle_call({send_message, RoomPid, UserFrom, MsgText}, _From, State) ->
  #state{rooms = Rooms} = State,
  case maps:find(RoomPid, Rooms) of
    {ok, _RoomName} -> Users = chat_room:add_message(RoomPid, UserFrom, MsgText),
      {reply, ok, State};
    error -> {reply, {error, room_not_found}, State}
  end;

%% отдает список всех сообщений, пришедших в комнату, сортированный от более старых сообщений к более новым. Принимает pid комнаты.
%% В случае успеха возвращает кортеж {ok, Messages}, где Messages -- список сообщений, представленых кортежем {Name, Text}.
%% Или возвращет кортеж {error, room_not_found}, если заданая комната не найдена.
handle_call({get_history, RoomPid}, _From, State) ->
  #state{rooms = Rooms} = State,
  case maps:find(RoomPid, Rooms) of
    {ok, _RoomName} -> Messages = chat_room:get_history(RoomPid),
      {reply, {ok, Messages}, State};
    error -> {reply, {error, room_not_found}, State}
  end;

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


code_change(_Arg0, _Arg1, _Arg3) ->
  erlang:error(not_implemented).


%%% module API


%% создает новую комнату. Принимает имя комнаты (binary) и возвращает комнату в виде кортежа {Name, Pid}.
create_room(RoomName) ->
  gen_server:call(?MODULE, {create_room, RoomName}).


%% возвращает список комнат, где каждая комната представлена кортежем {Name, Pid}. Комнаты в списке могут идти в произвольном порядке.
get_rooms() ->
  gen_server:call(?MODULE, {get_rooms}).


%% добавляет нового пользователя в комнату. Принимает pid комнаты, имя пользователя (binary) и pid пользователя.
%% Возвращает атом ok, если выполнилась успешно. Или возвращет кортеж {error, room_not_found}, если заданая комната не найдена.
add_user(RoomPid, UserName, UserPid) ->
  gen_server:call(?MODULE, {add_user, RoomPid, UserName, UserPid}).


%% удаляет пользователя из комнаты. Принимает pid комнаты и pid пользователя.
%% Возвращает ok, если пользователь успешно удален, или {error, user_not_found}, если такого пользователя в комнате не было,
%% или {error, room_not_found}, если заданая комната не найдена.
remove_user(RoomPid, UserPid) ->
  gen_server:call(?MODULE, {remove_user, RoomPid, UserPid}).


%% отдает список пользователей в комнате. Принимает pid комнаты.
%% В успешном случае возвращает кортеж {ok, Users}, Где Users -- список пользователей, представленых кортежем вида {Name, Pid}.
%% Или возвращет кортеж {error, room_not_found}, если заданая комната не найдена.
get_users(RoomPid) ->
  gen_server:call(?MODULE, {get_users, RoomPid}).


%% посылает новое сообщение в комнату. Принимает pid комнаты, имя автора сообщения (binary) и текст сообщения (binary).
%% В случае успеха возвращает атом ok. Или возвращет кортеж {error, room_not_found}, если заданая комната не найдена.
send_message(RoomPid, UserFrom, MsgText) ->
  gen_server:call(?MODULE, {send_message, RoomPid, UserFrom, MsgText}).


%% отдает список всех сообщений, пришедших в комнату, сортированный от более старых сообщений к более новым. Принимает pid комнаты.
%% В случае успеха возвращает кортеж {ok, Messages}, где Messages -- список сообщений, представленых кортежем {Name, Text}.
%% Или возвращет кортеж {error, room_not_found}, если заданая комната не найдена.
get_history(RoomPid) ->
  gen_server:call(?MODULE, {get_history, RoomPid}).

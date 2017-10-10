-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {users, history}).


start_link() ->
  gen_server:start_link(?MODULE, [], []).


%%% gen_server API


init([]) ->
  {ok, #state{users = maps:new(), history = []}}.


%% добавляет нового пользователя в комнату. Принимает pid комнаты, имя пользователя (binary) и pid пользователя.
%% Всегда возвращает атом ok.
handle_call({add_user, UserName, UserPid}, _From, State) ->
  #state{users = Users} = State,
  NewUsers = maps:put(UserPid, UserName, Users),
  NewState = State#state{users = NewUsers},
  {reply, ok, NewState};

%% удаляет пользователя из комнаты. Принимает pid комнаты и pid пользователя.
%% Возвращает ok, если пользователь успешно удален, или {error, user_not_found}, если такого пользователя в комнате не было.
handle_call({remove_user, UserPid}, _From, State) ->
  #state{users = Users} = State,
  case maps:find(UserPid, Users) of
    {ok, _UserName} -> NewUsers = maps:remove(UserPid, Users),
      NewState = State#state{users = NewUsers},
      {reply, ok, NewState};
    error -> {reply, {error, user_not_found}, State}
  end;

%% отдает список пользователей в комнате. Принимает pid комнаты,
%% возвращает список пользователей, где каждый пользователь представлен кортежем вида {Name, Pid}.
handle_call({get_users}, _From, State) ->
  #state{users = Users} = State,
  {reply, maps:fold(fun(Pid, Name, Acc) -> [{Name, Pid} | Acc] end, [], Users), State};

%% комната получает новое сообщение и рассылает его всем пользователям. Принимает pid комнаты, имя автора сообщения (binary) и текст сообщения (binary).
%% Всегда возвращает атом ok.
handle_call({add_message, UserFrom, MsgText}, _From, State) ->
  #state{users = Users, history = History} = State,
  maps:map(fun(Pid, _Name) -> chat_user:add_message(Pid, UserFrom, MsgText) end, Users),
  NewState = State#state{history = [{UserFrom, MsgText} | History]},
  {reply, ok, NewState};

%% отдает список всех сообщений, пришедших в комнату, сортированный от более старых сообщений к более новым.
%% Принимает pid комнаты, возвращает список сообщений, где каждое сообщение представлено кортежем {Name, Text}.
handle_call({get_history}, _From, State) ->
  #state{history = History} = State,
  {reply, lists:reverse(History), State};

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


%% добавляет нового пользователя в комнату. Принимает pid комнаты, имя пользователя (binary) и pid пользователя.
%% Всегда возвращает атом ok.
add_user(RoomPid, UserName, UserPid) ->
  gen_server:call(RoomPid, {add_user, UserName, UserPid}).


%% удаляет пользователя из комнаты. Принимает pid комнаты и pid пользователя.
%% Возвращает ok, если пользователь успешно удален, или {error, user_not_found}, если такого пользователя в комнате не было.
remove_user(RoomPid, UserPid) ->
  gen_server:call(RoomPid, {remove_user, UserPid}).


%% отдает список пользователей в комнате. Принимает pid комнаты,
%% возвращает список пользователей, где каждый пользователь представлен кортежем вида {Name, Pid}.
get_users(RoomPid) ->
  gen_server:call(RoomPid, {get_users}).


%% комната получает новое сообщение и рассылает его всем пользователям. Принимает pid комнаты, имя автора сообщения (binary) и текст сообщения (binary).
%% Всегда возвращает атом ok.
add_message(RoomPid, UserFrom, MsgText) ->
  gen_server:call(RoomPid, {add_message, UserFrom, MsgText}).


%% отдает список всех сообщений, пришедших в комнату, сортированный от более старых сообщений к более новым.
%% Принимает pid комнаты, возвращает список сообщений, где каждое сообщение представлено кортежем {Name, Text}.
get_history(RoomPid) ->
  gen_server:call(RoomPid, {get_history}).

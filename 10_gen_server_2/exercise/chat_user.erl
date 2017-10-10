-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {messages}).


start_link() ->
  gen_server:start_link(?MODULE, [], []).


%%% gen_server API


init([]) ->
  {ok, #state{messages = []}}.


%% пользователь получает новое сообщение и сохраняет его в своем состоянии.
%% На входе функция получает pid пользователя, имя автора сообщения (binary) и текст сообщения (binary).
%% Функция всегда возвращает атом ok.
handle_call({add_message, UserFrom, MsgText}, _From, State) ->
  #state{messages = Messages} = State,
  NewState = State#state{messages = [{UserFrom, MsgText} | Messages]},
  {reply, ok, NewState};

%% пользователь отдает список полученных сообщений, сортированный от более старых сообщений к более новым.
%% На входе функция получает pid пользователя, на выходе отдает список сообщений, где каждое сообщение представлено кортежем {Name, Text}.
handle_call({get_messages}, _From, State) ->
  #state{messages = Messages} = State,
  {reply, lists:reverse(Messages), State};

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


%% пользователь получает новое сообщение и сохраняет его в своем состоянии.
%% На входе функция получает pid пользователя, имя автора сообщения (binary) и текст сообщения (binary).
%% Функция всегда возвращает атом ok.
add_message(UserPid, UserFrom, MsgText) ->
  gen_server:call(UserPid, {add_message, UserFrom, MsgText}).


%% пользователь отдает список полученных сообщений, сортированный от более старых сообщений к более новым.
%% На входе функция получает pid пользователя, на выходе отдает список сообщений, где каждое сообщение представлено кортежем {Name, Text}.
get_messages(UserPid) ->
  gen_server:call(UserPid, {get_messages}).

-module(messenger_server).
-behaviour(gen_server).
-export([start_link/0, stop/0, send_message/4, register_user/2, 
         get_online_users/0, create_room/2, join_room/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    users = #{},           % UserID -> {PID, Username}
    rooms = #{},           % RoomID -> {Owner, Members}
    messages = queue:new() % История сообщений
}).

%% Публичный API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

register_user(UserId, Username) ->
    gen_server:call(?MODULE, {register_user, UserId, Username}).

send_message(From, To, Type, Content) ->
    gen_server:call(?MODULE, {send_message, From, To, Type, Content}).

get_online_users() ->
    gen_server:call(?MODULE, get_online_users).

create_room(UserId, RoomName) ->
    gen_server:call(?MODULE, {create_room, UserId, RoomName}).

join_room(UserId, RoomId, Password) ->
    gen_server:call(?MODULE, {join_room, UserId, RoomId, Password}).

%% Callbacks
init([]) ->
    {ok, #state{}}.

handle_call({register_user, UserId, Username}, {FromPid, _}, State) ->
    Users = State#state.users,
    case maps:is_key(UserId, Users) of
        true ->
            {reply, {error, user_exists}, State};
        false ->
            NewUsers = maps:put(UserId, {FromPid, Username}, Users),
            io:format("User ~s (~s) registered~n", [Username, UserId]),
            {reply, {ok, registered}, State#state{users = NewUsers}}
    end;

handle_call({send_message, From, To, Type, Content}, _From, State) ->
    Users = State#state.users,
    case maps:find(From, Users) of
        {ok, {FromPid, FromName}} ->
            case maps:find(To, Users) of
                {ok, {ToPid, ToName}} ->
                    Message = #{
                        from => From,
                        from_name => FromName,
                        to => To,
                        to_name => ToName,
                        type => Type,
                        content => Content,
                        timestamp => erlang:system_time(millisecond)
                    },
                    
                    % Отправляем сообщение получателю
                    ToPid ! {message_received, Message},
                    
                    % Сохраняем в историю
                    NewMessages = queue:in(Message, State#state.messages),
                    
                    io:format("Message from ~s to ~s: ~p~n", 
                             [FromName, ToName, Content]),
                    
                    {reply, {ok, sent}, State#state{messages = NewMessages}};
                error ->
                    {reply, {error, user_not_found}, State}
            end;
        error ->
            {reply, {error, sender_not_registered}, State}
    end;

handle_call(get_online_users, _From, State) ->
    UsersList = maps:fold(
        fun(UserId, {_Pid, Username}, Acc) ->
            [{UserId, Username} | Acc]
        end, [], State#state.users),
    {reply, {ok, UsersList}, State};

handle_call({create_room, UserId, RoomName}, _From, State) ->
    Rooms = State#state.rooms,
    RoomId = generate_room_id(),
    NewRooms = maps:put(RoomId, {UserId, []}, Rooms),
    io:format("Room ~s created by ~s~n", [RoomName, UserId]),
    {reply, {ok, RoomId, RoomName}, State#state{rooms = NewRooms}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Удаляем пользователя если процесс умер
    Users = State#state.users,
    NewUsers = maps:filter(fun(_Id, {UserPid, _}) -> UserPid =/= Pid end, Users),
    io:format("User disconnected~n"),
    {noreply, State#state{users = NewUsers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Messenger server stopping~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Вспомогательные функции
generate_room_id() ->
    list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

-ifdef(TEST).
generate_room_id() ->
    <<"test-room-id">>.
-endif.

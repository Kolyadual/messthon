-module(messenger).
-export([start/0, stop/0, register/2, send/3, online/0]).

%% API функции
start() ->
    Pid = spawn(fun() -> server_loop([]) end),
    register(messenger_server, Pid),
    {ok, Pid}.

stop() ->
    messenger_server ! stop.

register(Username, Pid) ->
    messenger_server ! {register, Username, Pid}.

send(From, To, Message) ->
    messenger_server ! {send, From, To, Message}.

online() ->
    messenger_server ! {online, self()},
    receive
        Users -> Users
    after 5000 ->
        {error, timeout}
    end.

%% Внутренние функции
server_loop(Users) ->
    receive
        {register, Username, Pid} ->
            NewUsers = [{Username, Pid} | lists:keydelete(Username, 1, Users)],
            Pid ! {registered, ok},
            io:format("~s зарегистрирован~n", [Username]),
            server_loop(NewUsers);
        
        {send, From, To, Message} ->
            case lists:keyfind(To, 1, Users) of
                {To, ToPid} ->
                    ToPid ! {message, From, Message},
                    io:format("Сообщение от ~s к ~s: ~s~n", [From, To, Message]),
                    server_loop(Users);
                false ->
                    io:format("Пользователь ~s не найден~n", [To]),
                    server_loop(Users)
            end;
        
        {online, FromPid} ->
            UserList = [Username || {Username, _} <- Users],
            FromPid ! UserList,
            server_loop(Users);
        
        {'DOWN', _, process, Pid, _} ->
            NewUsers = lists:filter(fun({_, UserPid}) -> UserPid =/= Pid end, Users),
            server_loop(NewUsers);
        
        stop ->
            io:format("Сервер остановлен~n"),
            ok;
        
        _ ->
            server_loop(Users)
    end.

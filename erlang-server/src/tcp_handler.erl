-module(tcp_handler).
-export([start/1, handle_client/2]).

start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, line},
        {reuseaddr, true},
        {active, true}
    ]),
    io:format("TCP сервер запущен на порту ~p~n", [Port]),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket, []) end),
    accept_loop(ListenSocket).

handle_client(Socket, Username) ->
    receive
        {tcp, Socket, Data} ->
            Message = binary_to_list(Data),
            handle_message(Socket, Username, string:strip(Message, both, $\n));
        {tcp_closed, Socket} ->
            io:format("Клиент ~s отключился~n", [Username]),
            ok;
        {message, From, Msg} ->
            Response = io_lib:format("~s: ~s~n", [From, Msg]),
            gen_tcp:send(Socket, Response),
            handle_client(Socket, Username)
    end.

handle_message(Socket, Username, Message) ->
    case Message of
        "REGISTER " ++ Name ->
            NewUsername = string:strip(Name),
            messenger:register(NewUsername, self()),
            gen_tcp:send(Socket, io_lib:format("REGISTERED как ~s~n", [NewUsername])),
            handle_client(Socket, NewUsername);
        
        "SEND " ++ Rest ->
            case string:tokens(Rest, " ") of
                [To | MsgParts] ->
                    Msg = string:join(MsgParts, " "),
                    messenger:send(Username, To, Msg),
                    gen_tcp:send(Socket, "SENT\n"),
                    handle_client(Socket, Username);
                _ ->
                    gen_tcp:send(Socket, "ERROR: Неверный формат~n"),
                    handle_client(Socket, Username)
            end;
        
        "ONLINE" ->
            Users = messenger:online(),
            Response = io_lib:format("ОНЛАЙН: ~p~n", [Users]),
            gen_tcp:send(Socket, Response),
            handle_client(Socket, Username);
        
        _ ->
            gen_tcp:send(Socket, "ERROR: Неизвестная команда~n"),
            handle_client(Socket, Username)
    end.

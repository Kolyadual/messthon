-module(start).
-export([main/0]).

main() ->
    io:format("=== Запуск Messthon Messenger ===~n"),
    
    % Запускаем сервер сообщений
    {ok, _} = messenger:start(),
    io:format("Сервер сообщений запущен~n"),
    
    % Запускаем TCP сервер
    Port = 8080,
    spawn(fun() -> tcp_handler:start(Port) end),
    io:format("TCP сервер запущен на порту ~p~n", [Port]),
    
    % Запускаем консольный интерфейс
    console_loop().

console_loop() ->
    io:format("Команды: online, stop~n> "),
    case io:get_line("") of
        "online\n" ->
            Users = messenger:online(),
            io:format("Онлайн пользователи: ~p~n", [Users]),
            console_loop();
        "stop\n" ->
            messenger:stop(),
            io:format("Сервер остановлен~n"),
            init:stop();
        _ ->
            io:format("Неизвестная команда~n"),
            console_loop()
    end.

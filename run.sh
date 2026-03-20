#!/bin/bash

# Messthon run.sh . Created by Kolyadual in 2025.

case $1 in
    server)
        echo "Запуск сервера Messthon..."
        python3 Server/server.py
        ;;
    client)
        echo "Запуск клиента Messthon..."
        python3 client.py
        ;;
    test)
        echo "Тестирование подключения..."
        python3 test_connection.py $2
        ;;
    *)
        echo "Использование: ./run.sh [server|client|test]"
        echo "  ./run.sh server        - запустить сервер"
        echo "  ./run.sh client        - запустить клиент"
        echo "  ./run.sh test [host]   - тест подключения (по умолчанию localhost)"
        ;;
esac

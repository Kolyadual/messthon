#!/bin/bash
echo "Starting Messthon..."

echo "Compilling..."
erlc -o ebin src/*.erl

echo "Запуск сервера..."
erl -pa ebin -s start main -noshell

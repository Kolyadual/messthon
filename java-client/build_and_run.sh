#!/bin/bash
echo "=== Сборка и запуск Java клиента ==="

# Компилируем
echo "Компиляция..."
javac -d . MessengerClient.java

# Собираем JAR
echo "Создание JAR..."
jar cfe MessengerClient.jar MessengerClient *.class

# Запускаем
echo "Запуск клиента..."
java -jar MessengerClient.jar

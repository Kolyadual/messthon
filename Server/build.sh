#!/bin/bash
echo "Building Messthon Server..."

# Удаляем старые сборки
rm -rf build
rm -rf dist

mkdir -p build
mkdir -p dist

# Компилируем только файлы из текущей директории src
javac -d build $(find src -name "*.java")

# Создаем JAR
cd build
jar -cvf ../dist/messthon-server.jar *
cd ..

echo "Server built successfully!"

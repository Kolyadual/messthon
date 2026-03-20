#!/bin/bash
echo "Building Messthon Client..."

JAVAFX_PATH="/usr/share/openjfx/lib"

mkdir -p build
mkdir -p dist

find src -name "*.java" > sources.txt

javac --module-path $JAVAFX_PATH --add-modules javafx.controls \
      -d build @sources.txt

cd build
jar -cvf ../dist/messthon-client.jar *
cd ..

rm sources.txt
echo "Client built successfully!"

#!/bin/bash
JAVAFX_PATH="/usr/share/openjfx/lib"
java --module-path $JAVAFX_PATH --add-modules javafx.controls \
     -cp dist/messthon-client.jar com.messthon.client.MainGUI

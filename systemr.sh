#!/bin/bash

sudo apt update

sudo apt install openjdk-17-jdk openjfx python3 python3-pip

pip3 install --break-system-packages -r requirements.txt

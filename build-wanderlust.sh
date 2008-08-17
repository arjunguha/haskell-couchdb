#!/bin/sh

./Setup.lhs configure --user --prefix=/Users/arjun/local
./Setup.lhs build
./Setup.lhs install

#!/bin/sh

./Setup.lhs configure --prefix=/pro/plt/ghc/6.8.3
./Setup.lhs build
./Setup.lhs install

#!/bin/sh
# rename this file to "install" and set the executable flag,
# then copy it into the bin directory of your release package
# (update the erts version number below to match your release)
ROOT=`pwd`
DIR=./erts-5.7.4/bin
sed s:%FINAL_ROOTDIR%:$ROOT: $DIR/erl.src > $DIR/erl

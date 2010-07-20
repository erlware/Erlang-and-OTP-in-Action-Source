#!/bin/sh
# rename this file to "simple_cache" and set the executable flag,
# then copy it into the bin directory of your release package
# (update the erts version number below to match your release)
./erts-5.7.4/bin/erl \
    -sname cache \
    -boot ./releases/0.2.0/start \
    -config ./releases/0.2.0/sys \
    -detached

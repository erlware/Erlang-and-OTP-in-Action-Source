#!/bin/bash

export LD_LIBRARY_PATH=../../yajl/build/yajl-1.0.9/lib

erl -pa ../json_parser/ebin -eval 'application:start(json_parser)'

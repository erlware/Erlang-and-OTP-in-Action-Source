#!/bin/sh
erlc -o ./simple_cache/ebin ./simple_cache/src/*.erl
erlc -o ./resource_discovery/ebin ./resource_discovery/src/*.erl
javac -cp /usr/local/lib/erlang/lib/jinterface-1.5.1/priv/OtpErlang.jar Test.java
javac -cp /usr/local/lib/erlang/lib/jinterface-1.5.1/priv/OtpErlang.jar JInterfaceExample.java
javac -cp hadoop-0.20.2/hadoop-0.20.2-core.jar:hbase-0.20.3/hbase-0.20.3.jar:/usr/local/lib/erlang/lib/jinterface-1.5.1/priv/OtpErlang.jar:./simple_cache/priv/java -d ./simple_cache/priv/java ./simple_cache/java_src/*.java

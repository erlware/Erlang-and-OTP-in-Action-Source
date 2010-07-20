#!/bin/sh
# Make sure to start HBase and create the 'cache' table before you run this

# start a contact node (this also ensures that EPMD is running)
erl -sname contact1 -detached -setcookie secret

# start the Java node (in the background - note the '&' at the end)
java -cp apache-log4j-1.2.16/log4j-1.2.16.jar:zookeeper-3.2.2/zookeeper-3.2.2.jar:commons-logging-1.1.1/commons-logging-1.1.1.jar:hadoop-0.20.2/hadoop-0.20.2-core.jar:hbase-0.20.3/hbase-0.20.3.jar:/usr/local/lib/erlang/lib/jinterface-1.5.1/priv/OtpErlang.jar:./simple_cache/priv/java HBaseNode hbase secret &

# start the simple_cache system
erl -sname mynode -pa ./simple_cache/ebin -pa ./resource_discovery/ebin/ -boot ./simple_cache -config sys -setcookie secret

#!/bin/sh
./hbase-0.20.3/bin/hbase shell <<EOF
create 'cache', {NAME => 'value'}
exit
EOF

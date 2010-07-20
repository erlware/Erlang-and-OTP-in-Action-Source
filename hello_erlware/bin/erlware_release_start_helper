#!/bin/sh

if [ $# -lt 3 ];then
	echo "usage $0 <rel-name> <rel-vsn> <erts-vsn> <config-file-name | no_config> [extra-args]"
	exit 1
fi

REL_NAME=$1; shift
REL_VSN=$1; shift
ERTS_VSN=$1; shift
CONFIG_FILE_NAME=$1

ERTS_DIR=$ROOTDIR/erts-$ERTS_VSN
export BINDIR=$ERTS_DIR/bin
export EMU=beam
export PROGNAME=erl
export LD_LIBRARY_PATH=$ERTS_DIR/lib

export REL_DIR=$ROOTDIR/releases/$REL_NAME-$REL_VSN

if [ "$CONFIG_FILE_NAME" = "no_config" ];then
	$BINDIR/erlexec -boot $REL_DIR/$REL_NAME $@
else
	shift
	$BINDIR/erlexec -config $REL_DIR/$CONFIG_FILE_NAME -boot $REL_DIR/$REL_NAME $@
fi

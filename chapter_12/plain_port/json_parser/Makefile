
#OTPROOT=/usr/lib/erlang
OTPROOT=/home/richardc/kred/otp/R13B04/lib/erlang

all:
	erlc -o ./ebin ./src/*.erl
	gcc -o ./priv/parser \
	  -I$(OTPROOT)/lib/erl_interface-3.6.5/include \
	  -L$(OTPROOT)/lib/erl_interface-3.6.5/lib \
	  ./c_src/parser.c \
	  -lerl_interface_st -lei_st

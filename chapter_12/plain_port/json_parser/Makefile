
#OTPROOT=/usr/lib/erlang
OTPROOT=/home/richardc/kred/otp/R13B04/lib/erlang
YAJLROOT=../../yajl/build/yajl-1.0.9
all:
	erlc -o ./ebin ./src/*.erl
	gcc -o ./priv/parser \
	  -I$(OTPROOT)/lib/erl_interface-3.6.5/include \
	  -I$(YAJLROOT)/include \
	  -L$(OTPROOT)/lib/erl_interface-3.6.5/lib \
	  -L$(YAJLROOT)/lib \
	  ./c_src/parser.c -lerl_interface_st -lei_st -lyajl

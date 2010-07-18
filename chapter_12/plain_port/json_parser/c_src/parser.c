#include "erl_interface.h"
#include "ei.h"

#include <yajl/yajl_parse.h>


static int handle_null(void * ctx);
static int handle_boolean(void * ctx, int boolean);
static int handle_integer(void * ctx, long integerVal);
static int handle_double(void * ctx, double doubleVal);
/* static int handle_number(void * ctx, const char * s, unsigned int l); */
static int handle_string(void * ctx, const unsigned char * stringVal,
                         unsigned int stringLen);
static int handle_map_key(void * ctx, const unsigned char * stringVal,
                          unsigned int stringLen);
static int handle_start_map(void * ctx);
static int handle_end_map(void * ctx);
static int handle_start_array(void * ctx);
static int handle_end_array(void * ctx);

static yajl_callbacks callbacks = {
    handle_null,
    handle_boolean,
    handle_integer, /* only handles long integers, not bignums */
    handle_double,
    NULL, /* any number - if defined, integer/double are not used */
    handle_string,
    handle_start_map,
    handle_map_key,
    handle_end_map,
    handle_start_array,
    handle_end_array
};


int main(int argc, char **argv)
{
    erl_init(NULL, 0);

    return 0;
}


static int handle_null(void * ctx)
{
    return 1;
}

static int handle_boolean(void * ctx, int boolean)
{
    return 1;
}

static int handle_integer(void * ctx, long integerVal)
{
    return 1;
}

static int handle_double(void * ctx, double doubleVal)
{
    return 1;
}

static int handle_string(void * ctx, const unsigned char * stringVal,
                           unsigned int stringLen)
{
    return 1;
}

static int handle_map_key(void * ctx, const unsigned char * stringVal,
                            unsigned int stringLen)
{
    return 1;
}

static int handle_start_map(void * ctx)
{
    return 1;
}


static int handle_end_map(void * ctx)
{
    return 1;
}

static int handle_start_array(void * ctx)
{
    return 1;
}

static int handle_end_array(void * ctx)
{
    return 1;
}

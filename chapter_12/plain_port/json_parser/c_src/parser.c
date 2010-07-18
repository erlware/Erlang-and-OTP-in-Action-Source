#include "erl_interface.h"
#include "ei.h"

#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>

int main(int argc, char **argv) {
    yajl_gen g;
    yajl_gen_config conf = { 1, "  " };
    g = yajl_gen_alloc(&conf, NULL);

    erl_init(NULL, 0);

    return 0;
}

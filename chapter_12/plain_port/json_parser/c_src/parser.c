/*
  Erlang port program for interfacing with the YAJL JSON parser
*/
#include <stdlib.h>
#include <stdio.h>

#include "erl_interface.h"
#include "ei.h"

#include <yajl/yajl_parse.h>

static int handle_null(void * ctx);
static int handle_boolean(void * ctx, int boolean);
static int handle_integer(void * ctx, long integerVal);
static int handle_double(void * ctx, double doubleVal);
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

/*
void *alloc_func(void *ctx, unsigned int sz)
{
  return malloc(sz);
}

void *realloc_func(void *ctx, void *ptr, unsigned int sz)
{
  return realloc(ptr, sz);
}

void free_func(void *ctx, void *ptr)
{
  free(ptr);
}

static yajl_alloc_funcs alloc_funcs = {
  alloc_func,
  realloc_func,
  free_func,
  NULL
};
*/

int main(int argc, char **argv)
{
  erl_init(NULL, 0); /* initialize erl_interface */
  
  yajl_parser_config cfg = {
    1, /* allow comments */
    0  /* don't check UTF-8 */
  };
  yajl_handle hand;
  yajl_status stat;
  static unsigned char fileData[65536];
  int done = 0;
  size_t rd;

  /* hand = yajl_alloc(&callbacks, &cfg, &alloc_funcs, NULL); */
  hand = yajl_alloc(&callbacks, &cfg, NULL, NULL);
  while (!done) {
    rd = fread((void *) fileData, 1, sizeof(fileData) - 1, stdin);
    fileData[rd] = 0; /* zero-terminate the read data */
    if (rd == 0) {
      if (!feof(stdin)) {
        fprintf(stderr, "read error\n");
        exit(1);
      }
      done = 1;
    }
    if (done) {
      stat = yajl_parse_complete(hand);
    } else {
      stat = yajl_parse(hand, fileData, rd);
    }
    if ((stat != yajl_status_ok)
        && (stat != yajl_status_insufficient_data)) {
      unsigned char *str = yajl_get_error(hand, 1, fileData, rd);
      fprintf(stderr, "%s\n", (const char *)str);
      yajl_free_error(hand, str);
    }
  }
  yajl_free(hand);

  return 0;
}


static int handle_null(void * ctx)
{
  /* TODO */
  printf("Null.\n");
  return 1;
}

static int handle_boolean(void * ctx, int boolean)
{
  /* TODO */
  printf("Boolean: %d\n", boolean);
  return 1;
}

static int handle_integer(void * ctx, long integerVal)
{
  /* TODO */
  printf("Integer: %ld\n", integerVal);
  return 1;
}

static int handle_double(void * ctx, double doubleVal)
{
  /* TODO */
  printf("Double: %f\n", doubleVal);
  return 1;
}

static int handle_string(void * ctx, const unsigned char * stringVal,
                         unsigned int stringLen)
{
  /* TODO */
  printf("String: '%.*s'\n", stringLen, stringVal);
  return 1;
}

static int handle_map_key(void * ctx, const unsigned char * stringVal,
                          unsigned int stringLen)
{
  /* TODO */
  printf("Map Key: '%.*s'\n", stringLen, stringVal);
  return 1;
}

static int handle_start_map(void * ctx)
{
  /* TODO */
  printf("Start map.\n");
  return 1;
}


static int handle_end_map(void * ctx)
{
  /* TODO */
  printf("End map.\n");
  return 1;
}

static int handle_start_array(void * ctx)
{
  /* TODO */
  printf("Start array.\n");
  return 1;
}

static int handle_end_array(void * ctx)
{
  /* TODO */
  printf("End array.\n");
  return 1;
}

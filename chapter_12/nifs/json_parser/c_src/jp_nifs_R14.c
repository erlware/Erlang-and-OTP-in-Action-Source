/*
  Erlang NIF interface to the YAJL JSON parser (Erlang/OTP R14 version)
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <erl_nif.h>

#include <yajl/yajl_parse.h>

/* yajl callback prototypes */
static int handle_null(void *ctx);
static int handle_boolean(void *ctx, int boolVal);
static int handle_integer(void *ctx, long integerVal);
static int handle_double(void *ctx, double doubleVal);
static int handle_string(void *ctx, const unsigned char *stringVal,
                         unsigned int stringLen);
static int handle_map_key(void *ctx, const unsigned char *stringVal,
                          unsigned int stringLen);
static int handle_start_map(void *ctx);
static int handle_end_map(void *ctx);
static int handle_start_array(void *ctx);
static int handle_end_array(void *ctx);

static yajl_callbacks callbacks = {
  handle_null,
  handle_boolean,
  handle_integer,  /* note: only handles long integers, not bignums */
  handle_double,
  NULL,  /* any number - if defined, integer/double are not used */
  handle_string,
  handle_start_map,
  handle_map_key,
  handle_end_map,
  handle_start_array,
  handle_end_array
};


typedef struct container_t {
  int count;    /* number of elements */
  int arraysz;  /* size of elements array */
  ERL_NIF_TERM *array;  /* elements array */
  struct container_t *next;
} container_t;

typedef struct {
  ErlNifEnv *env;  /* NIF environment */
  int key;         /* true if last term was a key */
  container_t *c;  /* innermost container */
  char errmsg[256];
} state_t;


static void *alloc_func(void *ctx, unsigned int sz)
{
  return enif_alloc(sz);
}

static void *realloc_func(void *ctx, void *ptr, unsigned int sz)
{
  return enif_realloc(ptr, sz);
}

static void free_func(void *ctx, void *ptr)
{
  enif_free(ptr);
}

static yajl_alloc_funcs alloc_funcs = {
  alloc_func,
  realloc_func,
  free_func,
  NULL  /* must be initialized below */
};


static const char *parse_json(state_t *st, unsigned char *buf, size_t len)
{
  yajl_parser_config cfg = {
    1, /* allow comments */
    0  /* don't check UTF-8 */
  };
  yajl_handle yh;
  yajl_status ys;
  const char *err=NULL;

  alloc_funcs.ctx = st->env; /* will be passed to alloc funcs */
  yh = yajl_alloc(&callbacks, &cfg, &alloc_funcs, st);
  ys = yajl_parse(yh, buf, len);
  if (ys == yajl_status_insufficient_data) {
    ys = yajl_parse_complete(yh);
  }
  if (ys == yajl_status_insufficient_data) {
    err = "unexpected end of document";
  } else if (ys != yajl_status_ok) {
    unsigned char *msg = yajl_get_error(yh, 0, NULL, 0);
    strncpy(st->errmsg, (char *)msg, sizeof(st->errmsg)-1);
    yajl_free_error(yh, msg);
    st->errmsg[sizeof(st->errmsg)] = 0;
    err = st->errmsg;
  }
  yajl_free(yh);
  return err;
}


static ERL_NIF_TERM parse_document_1(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[])
{
  /* initialize the state, with a dummy top level container */
  state_t st;
  st.env = env; /* keep NIF environment in state */
  st.key = 0;
  ERL_NIF_TERM term;
  container_t c = { 0, 1, &term, NULL };
  st.c = &c;
  
  ErlNifBinary bin;
  if (argc != 1 || !enif_is_binary(env, argv[0]))
    return enif_make_badarg(env);
  if (!enif_inspect_binary(env, argv[0], &bin))
    return enif_make_badarg(env);
  
  const char *err;
  if ((err = parse_json(&st, bin.data, bin.size)) != NULL) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                            enif_make_string(env, err, ERL_NIF_LATIN1));
  }
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
}


static ErlNifFunc json_parser_NIFs[] = {
  {"parse_document", 1, &parse_document_1}
};

ERL_NIF_INIT(json_parser, json_parser_NIFs, NULL, NULL, NULL, NULL);


/*
 * JSON                            Erlang json() representation
 * ----                            ----------------------------
 * true                            'true'
 * false                           'false'
 * null                            'undefined'
 * number (integers and floats)    number()
 * string: "..."                   binary()
 * array/list: [ value, ... ]      {json()}
 * map: { label: value, ... }      [{binary(), json()}]
 */

static void add_element(state_t *st, ERL_NIF_TERM t)
{
  container_t *c = st->c;
  if (c != NULL) {
    if (c->count >= c->arraysz) {
      c->arraysz *= 2;
      c->array = enif_realloc(c->array, c->arraysz);
    }
    if (st->key) {
      /* the previous stored element was a key, so replace the entry
         with a key/value pair, and don't count it twice */
      c->array[c->count-1] = enif_make_tuple2(st->env,
                                              c->array[c->count-1],
                                              t);
      st->key = 0;
    } else {
      c->array[c->count] = t;
      ++(c->count);
    }
  }
}

static int handle_null(void *ctx)
{
  state_t *st = (state_t *)ctx;
  add_element(st, enif_make_atom(st->env, "undefined"));
  return 1;
}

static int handle_boolean(void *ctx, int boolVal)
{
  state_t *st = (state_t *)ctx;
  if (boolVal) {
    add_element(st, enif_make_atom(st->env, "true"));
  } else {
    add_element(st, enif_make_atom(st->env, "false"));
  }
  return 1;
}

static int handle_integer(void *ctx, long integerVal)
{
  state_t *st = (state_t *)ctx;
  add_element(st, enif_make_long(st->env, integerVal));
  return 1;
}

static int handle_double(void *ctx, double doubleVal)
{
  state_t *st = (state_t *)ctx;
  add_element(st, enif_make_double(st->env, doubleVal));
  return 1;
}

static int handle_string(void *ctx, const unsigned char *stringVal,
                         unsigned int stringLen)
{
  state_t *st = (state_t *)ctx;
  ErlNifBinary bin;
  enif_alloc_binary(stringLen, &bin);
  strncpy((char *)bin.data, (char *)stringVal, stringLen);
  add_element(st, enif_make_binary(st->env, &bin));
  return 1;
}

static int handle_map_key(void *ctx, const unsigned char *stringVal,
                          unsigned int stringLen)
{
  state_t *st = (state_t *)ctx;
  ErlNifBinary bin;
  enif_alloc_binary(stringLen, &bin);
  strncpy((char *)bin.data, (char *)stringVal, stringLen);
  add_element(st, enif_make_binary(st->env, &bin));
  st->key = 1; /* note that the next term will be the value */
  return 1;
}

static int handle_start(void *ctx, int array)
{
  state_t *st = (state_t *)ctx;
  container_t *c = enif_alloc(sizeof(container_t));
  /* link and initialize container struct */
  c->next = st->c;
  st->c = c;
  c->count = 0;
  c->arraysz = 32;  /* initial term buffer size */
  c->array = enif_alloc(c->arraysz);
  return 1;
}

static int handle_start_map(void *ctx)
{
  return handle_start(ctx, 0);
}

static int handle_start_array(void *ctx)
{
  return handle_start(ctx, 1);
}

static int handle_end(void *ctx, int array)
{
  state_t *st = (state_t *)ctx;
  container_t *c = st->c;
  /* unlink container struct from state */
  st->c = c->next;
  /* create and add container term */
  if (array) {
    add_element(st, enif_make_tuple_from_array(st->env, c->array,
                                               c->count));
  } else {
    add_element(st, enif_make_list_from_array(st->env, c->array,
                                              c->count));
  }
  /* decallocate used container struct */
  enif_free(c);
  return 1;
}

static int handle_end_map(void *ctx)
{
  return handle_end(ctx, 0);
}

static int handle_end_array(void *ctx)
{
  return handle_end(ctx, 1);
}

/*
  Erlang linked-in port driver for interfacing with the YAJL JSON parser
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <erl_driver.h>
#include <ei.h>

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
  int index;    /* offset of container header */
  int count;    /* number of elements */
  struct container_t *next;
} container_t;

typedef struct {
  ei_x_buff x;     /* Erl Interface dynamic buffer */
  container_t *c;  /* innermost container */
  char errmsg[256];
} state_t;


static ErlDrvData drv_start(ErlDrvPort port, char *command);

static void drv_stop(ErlDrvData handle);

static void drv_output(ErlDrvData handle, char *buf, int sz);

static ErlDrvEntry jp_driver_entry = {
    NULL,                            /* init */
    drv_start,                       /* start */
    drv_stop,                        /* stop */
    drv_output,                      /* output */
    NULL,                            /* ready_input */
    NULL,                            /* ready_output */
    "jp_driver",                     /* driver_name */
    NULL,                            /* finish */
    NULL,                            /* handle (reserved) */
    NULL,                            /* control */
    NULL,                            /* timeout */
    NULL,                            /* outputv */
    NULL,                            /* ready_async */
    NULL,                            /* flush */
    NULL,                            /* call */
    NULL,                            /* event */
    ERL_DRV_EXTENDED_MARKER,         /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,  /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,  /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING    /* ERL_DRV_FLAGs */
};

DRIVER_INIT(jp_driver) /* must match name in driver_entry */
{
    return &jp_driver_entry;
}

typedef struct {
  ErlDrvPort port;
} drv_data_t;


static void *alloc_func(void *ctx, unsigned int sz)
{
  return driver_alloc(sz);
}

static void *realloc_func(void *ctx, void *ptr, unsigned int sz)
{
  return driver_realloc(ptr, sz);
}

static void free_func(void *ctx, void *ptr)
{
  driver_free(ptr);
}

static yajl_alloc_funcs alloc_funcs = {
  alloc_func,
  realloc_func,
  free_func,
  NULL
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


static void make_error(state_t *st, const char *text)
{
  /* replace the old output buffer */
  ei_x_free(&st->x);
  ei_x_new_with_version(&st->x);
  /* encode an error tuple with the text as an atom */
  ei_x_encode_tuple_header(&st->x, 2);
  ei_x_encode_atom(&st->x, "error");
  ei_x_encode_string(&st->x, text);
}

static void process_data(drv_data_t *d, unsigned char *buf)
{
  /* initialize the state and the output buffer */
  state_t st;
  st.c = NULL;
  ei_x_new_with_version(&st.x);

  int index = 0;
  int ver = 0, type = 0, size = 0;

  if (ei_decode_version((char *)buf, &index, &ver)) {
    make_error(&st, "data encoding version mismatch");
  } else if (ei_get_type((char *)buf, &index, &type, &size)
             || type != ERL_BINARY_EXT) {
    make_error(&st, "data must be a binary");
  } else {
    ei_x_encode_tuple_header(&st.x, 2); /* begin ok-result tuple */
    ei_x_encode_atom(&st.x, "ok");
    const char *err;
    if ((err = parse_json(&st, &buf[index+5], size)) != NULL) {
      make_error(&st, err);
    }
  }
  driver_output(d->port, st.x.buff, st.x.buffsz);
  ei_x_free(&st.x);
}


static ErlDrvData drv_start(ErlDrvPort port, char *command)
{
  drv_data_t *d = (drv_data_t *)driver_alloc(sizeof(drv_data_t));
  d->port = port;
  return (ErlDrvData)d;
}

static void drv_stop(ErlDrvData handle) {
  driver_free((char *)handle);
}

static void drv_output(ErlDrvData handle, char *buf, int sz)
{
  process_data((drv_data_t *)handle, (unsigned char *)buf); 
}


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

static void count_element(state_t *st)
{
  container_t *c = st->c;
  if (c != NULL) ++(c->count);
}

static int handle_null(void *ctx)
{
  state_t *st = (state_t *)ctx;
  count_element(st);
  ei_x_encode_atom(&st->x, "undefined");
  return 1;
}

static int handle_boolean(void *ctx, int boolVal)
{
  state_t *st = (state_t *)ctx;
  count_element(st);
  ei_x_encode_boolean(&st->x, boolVal);
  return 1;
}

static int handle_integer(void *ctx, long integerVal)
{
  state_t *st = (state_t *)ctx;
  count_element(st);
  ei_x_encode_long(&st->x, integerVal);
  return 1;
}

static int handle_double(void *ctx, double doubleVal)
{
  state_t *st = (state_t *)ctx;
  count_element(st);
  ei_x_encode_double(&st->x, doubleVal);
  return 1;
}

static int handle_string(void *ctx, const unsigned char *stringVal,
                         unsigned int stringLen)
{
  state_t *st = (state_t *)ctx;
  count_element(st);
  ei_x_encode_binary(&st->x, stringVal, stringLen);
  return 1;
}

static int handle_map_key(void *ctx, const unsigned char *stringVal,
                          unsigned int stringLen)
{
  state_t *st = (state_t *)ctx;
  /* Begin a 2-tuple with the key string as first element.
     The next called callback function will encode the value.
     Don't count the keys, only the values. */
  ei_x_encode_tuple_header(&st->x, 2);
  ei_x_encode_binary(&st->x, stringVal, stringLen);
  return 1;
}

static int handle_start(void *ctx, int array)
{
  state_t *st = (state_t *)ctx;
  count_element(st);
  container_t *c = driver_alloc(sizeof(container_t));
  /* link and initialize container struct */
  c->next = st->c;
  st->c = c;
  c->count = 0;
  c->index = st->x.index; /* save current position */
  /* write a dummy header for now */
  if (array) {
    ei_x_encode_tuple_header(&st->x, 1);
  } else {
    ei_x_encode_list_header(&st->x, 1);
  }
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
  /* back-patch the header */
  if (array) {
    ei_encode_tuple_header(st->x.buff, &c->index, c->count);
  } else {
    ei_encode_list_header(st->x.buff, &c->index, c->count);
    ei_x_encode_empty_list(&st->x);  /* also terminate the list */
  }
  /* unlink and decallocate container struct */
  st->c = c->next;
  driver_free(c);
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

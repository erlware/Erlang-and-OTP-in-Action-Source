/*
  Erlang port program for interfacing with the YAJL JSON parser
*/
#include <stdlib.h>
#include <stdio.h>

#include <erl_interface.h>
#include <ei.h>

#include <yajl/yajl_parse.h>

/* yajl callback prototypes */
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


struct container {
  int index;    /* offset of container header */
  int count;    /* number of elements */
  struct container *next;
};

struct state {
  ei_x_buff x;         /* the dynamic buffer */
  struct container *c; /* innermost container */
};


#define ERR_READ         10
#define ERR_READ_HEADER  11
#define ERR_PACKET_SIZE  12
#define ERR_DECODE       13
#define ERR_BADARG       14

size_t read_bytes(unsigned char *buf, size_t max, FILE *fd)
{
  size_t n;
  n = fread(buf, 1, max, fd);
  if ((n == 0) && !feof(fd)) {
    exit(ERR_READ);
  }
  buf[n] = 0; /* zero-terminate the read data */
  return n;
}

static void write_packet(ei_x_buff *x, FILE *fd)
{
  uint8_t hd[4];
  int sz = x->buffsz;
  
  /* the packet header must be in Network Byte Order */
  hd[0] = (sz >> 24) & 0xff;
  hd[1] = (sz >> 16) & 0xff;
  hd[2] = (sz >> 8) & 0xff;
  hd[3] = sz & 0xff;
  fwrite(hd, 1, 4, fd);

  fwrite(x->buff, 1, sz, fd);
  fflush(fd);
}

/* returns an ETERM pointer to a binary term, or NULL */
static ETERM *read_document(unsigned char *buf, size_t max, FILE *fd)
{
  size_t n, sz;
  uint8_t hd[4];
  ETERM *t;
  
  n = read_bytes(hd, 4, fd);
  if (n != 4) {
    if (n == 0 && feof(fd)) return NULL;
    exit(ERR_READ_HEADER);
  }
  /* the packet header is always in Network Byte Order */
  sz = (hd[0] << 24) + (hd[1] << 16) + (hd[2] << 8) + hd[3];
  if (sz > max) {
    exit(ERR_PACKET_SIZE);
  }
  n = read_bytes(buf, sz, fd);
  if (n != sz) {
    exit(ERR_READ);
  }
  t = erl_decode(buf);
  if (t == NULL) {
    exit(ERR_DECODE);
  }
  if (!ERL_IS_BINARY(t)) {
    exit(ERR_BADARG);
  }
  return t;
}

void write_error(const char *text, FILE *fd)
{
  ei_x_buff x;
  ei_x_new_with_version(&x);
  ei_x_encode_tuple_header(&x, 2);
  ei_x_encode_atom(&x, "error");
  ei_x_encode_string(&x, text);
  write_packet(&x, fd);
  ei_x_free(&x);
}


int main(int argc, char **argv)
{
  erl_init(NULL, 0); /* initialize erl_interface */

  yajl_parser_config cfg = {
    1, /* allow comments */
    0  /* don't check UTF-8 */
  };
  yajl_handle hand;
  yajl_status stat;

  static unsigned char buf[65536];
  struct state st;
  ETERM *t;

  while ((t = read_document(buf, sizeof(buf) - 1, stdin)) != NULL) {
    /* initialize the state and the output buffer */
    st.c = NULL;
    ei_x_new_with_version(&st.x);

    ei_x_encode_tuple_header(&st.x, 2); /* begin ok-result tuple */
    ei_x_encode_atom(&st.x, "ok");

    hand = yajl_alloc(&callbacks, &cfg, NULL, &st);
    stat = yajl_parse(hand, ERL_BIN_PTR(t), ERL_BIN_SIZE(t));
    if (stat == yajl_status_insufficient_data) {
      stat = yajl_parse_complete(hand);
    }
    if (stat == yajl_status_insufficient_data) {
      write_error("unexpected end of document", stdout);
    } else if (stat != yajl_status_ok) {
      unsigned char *msg = yajl_get_error(hand, 0, NULL, 0);
      write_error(msg, stdout);
      yajl_free_error(hand, msg);
    } else {
      write_packet(&st.x, stdout);
    }

    yajl_free(hand);
    ei_x_free(&st.x);
    erl_free(t);
  }
  return 0;
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

static void count_element(struct state *st)
{
  struct container *c = st->c;
  if (c != NULL) ++(c->count);
}

static int handle_null(void * ctx)
{
  struct state *st = (struct state *)ctx;
  count_element(st);
  ei_x_encode_atom(&st->x, "undefined");
  return 1;
}

static int handle_boolean(void * ctx, int boolean)
{
  struct state *st = (struct state *)ctx;
  count_element(st);
  ei_x_encode_boolean(&st->x, boolean);
  return 1;
}

static int handle_integer(void * ctx, long integerVal)
{
  struct state *st = (struct state *)ctx;
  count_element(st);
  ei_x_encode_long(&st->x, integerVal);
  return 1;
}

static int handle_double(void * ctx, double doubleVal)
{
  struct state *st = (struct state *)ctx;
  count_element(st);
  ei_x_encode_double(&st->x, doubleVal);
  return 1;
}

static int handle_string(void * ctx, const unsigned char * stringVal,
                         unsigned int stringLen)
{
  struct state *st = (struct state *)ctx;
  count_element(st);
  ei_x_encode_binary(&st->x, stringVal, stringLen);
  return 1;
}

static int handle_map_key(void * ctx, const unsigned char * stringVal,
                          unsigned int stringLen)
{
  struct state *st = (struct state *)ctx;
  /* Begin a 2-tuple with the key string as first element.
     The next called callback function will encode the value.
     Don't count the keys, only the values. */
  ei_x_encode_tuple_header(&st->x, 2);
  ei_x_encode_binary(&st->x, stringVal, stringLen);
  return 1;
}

static int handle_start(void * ctx, int array)
{
  struct state *st = (struct state *)ctx;
  struct container *c = malloc(sizeof(struct container));
  count_element(st);
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

static int handle_start_map(void * ctx)
{
  handle_start(ctx, 0);
}

static int handle_start_array(void * ctx)
{
  handle_start(ctx, 1);
}

static int handle_end(void * ctx, int array)
{
  struct state *st = (struct state *)ctx;
  struct container *c = st->c;
  int index = st->c->index; /* temporary variable needed */
  /* back-patch the header */
  if (array) {
    ei_encode_tuple_header(st->x.buff, &index, st->c->count);
  } else {
    ei_encode_list_header(st->x.buff, &index, st->c->count);
    ei_x_encode_empty_list(&st->x);  /* also terminate the list */
  }
  /* unlink and decallocate container struct */
  st->c = c->next;
  free(c);
  return 1;
}

static int handle_end_map(void * ctx)
{
  handle_end(ctx, 0);
}

static int handle_end_array(void * ctx)
{
  handle_end(ctx, 1);
}

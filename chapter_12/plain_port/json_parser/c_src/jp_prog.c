/*
  Erlang port program for interfacing with the YAJL JSON parser
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <ei.h>

#include <yajl/yajl_parse.h>

#define BUFSIZE 65536

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


#define ERR_READ         10
#define ERR_READ_HEADER  11
#define ERR_PACKET_SIZE  12

static void write_packet(char *buf, int sz, FILE *fd)
{
  uint8_t hd[4];
  
  /* the packet header must be in Network Byte Order */
  hd[0] = (sz >> 24) & 0xff;
  hd[1] = (sz >> 16) & 0xff;
  hd[2] = (sz >> 8) & 0xff;
  hd[3] = sz & 0xff;
  fwrite(hd, 1, 4, fd);

  fwrite(buf, 1, sz, fd);
  fflush(fd);
}

static size_t read_bytes(unsigned char *buf, size_t max, FILE *fd)
{
  size_t n;
  n = fread(buf, 1, max, fd);
  if ((n == 0) && !feof(fd)) {
    exit(ERR_READ);
  }
  return n;
}

static void read_packet(unsigned char *buf, size_t max, FILE *fd)
{
  size_t n, sz;
  uint8_t hd[4];
  
  n = read_bytes(hd, 4, fd);
  if (n == 0 && feof(fd)) exit(EXIT_SUCCESS); /* end of input */
  if (n != 4) exit(ERR_READ_HEADER);
  /* the packet header is always in Network Byte Order */
  sz = (hd[0] << 24) + (hd[1] << 16) + (hd[2] << 8) + hd[3];
  if (sz > max) {
    exit(ERR_PACKET_SIZE);
  }
  n = read_bytes(buf, sz, fd);
  if (n != sz) {
    exit(ERR_READ);
  }
}


static const char *parse_json(state_t *st, unsigned char *buf, size_t len)
{
  yajl_parser_config cfg = {
    1, /* allow comments */
    0  /* don't check UTF-8 */
  };
  yajl_handle yh;
  yajl_status ys;
  const char *err=NULL;

  yh = yajl_alloc(&callbacks, &cfg, NULL, st);
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

static void process_data(unsigned char *buf)
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
  write_packet(st.x.buff, st.x.buffsz, stdout); /* output result */
  ei_x_free(&st.x);
}


int main(int argc, char **argv)
{
  static unsigned char buf[BUFSIZE];
  for (;;) {
    read_packet(buf, sizeof(buf)-1, stdin);
    buf[sizeof(buf)-1] = 0;  /* zero-terminate the data */
    process_data(buf);
  }
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
  container_t *c = malloc(sizeof(container_t));
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
  free(c);
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

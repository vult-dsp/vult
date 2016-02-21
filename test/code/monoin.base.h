#ifndef ._H
#define ._H
#include <stdint.h>
#include <math.h>

typedef struct _ctx_type_0 {
   int pre;
   int n4;
   int n3;
   int n2;
   int n1;
   int count;
} _ctx_type_0;

typedef _ctx_type_0 noteOn_type;

_ctx_type_0 _ctx_type_0_init();

_ctx_type_0 noteOn_init();

int noteOn(_ctx_type_0 &_ctx, int n);

typedef _ctx_type_0 noteOff_type;

_ctx_type_0 noteOff_init();

int noteOff(_ctx_type_0 &_ctx, int n);

typedef _ctx_type_0 isGateOn_type;

_ctx_type_0 isGateOn_init();

uint8_t isGateOn(_ctx_type_0 &_ctx);



#endif // ._H

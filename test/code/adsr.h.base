#ifndef ADSR_H
#define ADSR_H
#include <stdint.h>
#include <math.h>

typedef struct _ctx_type_0 {
   int count;
} _ctx_type_0;

typedef _ctx_type_0 each_type;

_ctx_type_0 _ctx_type_0_init();

_ctx_type_0 each_init();

uint8_t each(_ctx_type_0 &_ctx, int n);

typedef struct _ctx_type_1 {
   uint8_t pre_x;
} _ctx_type_1;

typedef _ctx_type_1 bchange_type;

_ctx_type_1 _ctx_type_1_init();

_ctx_type_1 bchange_init();

uint8_t bchange(_ctx_type_1 &_ctx, uint8_t x);

typedef struct _ctx_type_2 {
   float pre_x;
} _ctx_type_2;

typedef _ctx_type_2 lpfilter_type;

_ctx_type_2 _ctx_type_2_init();

_ctx_type_2 lpfilter_init();

float lpfilter(_ctx_type_2 &_ctx, float x);

typedef struct _ctx_type_3 {
   float value;
   float sustainLevel;
   int state;
   float releaseRate;
   uint8_t gate;
   float decayRate;
   float attackRate;
   _ctx_type_2 _inst2;
   _ctx_type_1 _inst1;
   _ctx_type_0 _inst0;
} _ctx_type_3;

typedef _ctx_type_3 adsr_type;

_ctx_type_3 _ctx_type_3_init();

_ctx_type_3 adsr_init();

float adsr(_ctx_type_3 &_ctx, uint8_t gate, float attack, float decay, float sustain, float release);



#endif // ADSR_H

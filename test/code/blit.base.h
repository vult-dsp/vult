#ifndef BLIT_H
#define BLIT_H
#include <stdint.h>
#include <math.h>

uint8_t near_zero(float x);

typedef struct _ctx_type_1 {
   float pre_x;
} _ctx_type_1;

typedef _ctx_type_1 change_type;

_ctx_type_1 _ctx_type_1_init();

_ctx_type_1 change_init();

uint8_t change(_ctx_type_1 &_ctx, float x);

typedef struct _ctx_type_2 {
   float y1;
   float x1;
} _ctx_type_2;

typedef _ctx_type_2 dcblock_type;

_ctx_type_2 _ctx_type_2_init();

_ctx_type_2 dcblock_init();

float dcblock(_ctx_type_2 &_ctx, float x0);

float pitchToRate(float d);

float pulse_train(float m, float phase);

typedef struct _ctx_type_5 {
   float state_triang;
   float state_saw;
   float state_pulse;
   float rate;
   float phase;
   float output;
   float m;
   _ctx_type_2 _inst1;
   _ctx_type_1 _inst0;
} _ctx_type_5;

typedef _ctx_type_5 osc_type;

_ctx_type_5 _ctx_type_5_init();

_ctx_type_5 osc_init();

float osc(_ctx_type_5 &_ctx, float pitch, float pw, float wave);



#endif // BLIT_H
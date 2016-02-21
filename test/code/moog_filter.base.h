#ifndef MOOG_FILTER_H
#define MOOG_FILTER_H
#include <stdint.h>
#include <math.h>

typedef struct _ctx_type_0 {
   float pre_x;
} _ctx_type_0;

typedef _ctx_type_0 change_type;

_ctx_type_0 _ctx_type_0_init();

_ctx_type_0 change_init();

uint8_t change(_ctx_type_0 &_ctx, float x);

float min(float a, float b);

float max(float a, float b);

float clip(float value, float low, float high);

float samplerate();

float PI();

float thermal();

typedef struct _ctx_type_7 {
   float tw2;
   float tw1;
   float tw0;
   float dw3;
   float dw2;
   float dw1;
   float dw0;
} _ctx_type_7;

typedef _ctx_type_7 moog_step_type;

_ctx_type_7 _ctx_type_7_init();

_ctx_type_7 moog_step_init();

float moog_step(_ctx_type_7 &_ctx, float input, float resFixed, float tune, float output);

typedef struct _ctx_type_8 {
   float tune;
   float resFixed;
   _ctx_type_7 filter;
   float dx1;
   _ctx_type_0 _inst1;
   _ctx_type_0 _inst0;
} _ctx_type_8;

typedef _ctx_type_8 moog_type;

_ctx_type_8 _ctx_type_8_init();

_ctx_type_8 moog_init();

float moog(_ctx_type_8 &_ctx, float input, float cut, float res);

int n = 0;
while((n < 44100)){
   float kk = moog(_ctx.x,1.f,2000.f,0.1f);
   n = (n + 1);
}
return 0;


#endif // MOOG_FILTER_H

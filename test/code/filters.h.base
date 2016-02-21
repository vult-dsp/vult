#ifndef FILTERS_H
#define FILTERS_H
#include <stdint.h>
#include <math.h>

float samplerate();

float pi();

float min(float a, float b);

float max(float a, float b);

float clip(float low, float high, float value);

typedef struct _ctx_type_5 {
   float pre_x;
} _ctx_type_5;

typedef _ctx_type_5 change_type;

_ctx_type_5 _ctx_type_5_init();

_ctx_type_5 change_init();

uint8_t change(_ctx_type_5 &_ctx, float x);

typedef struct _ctx_type_6 {
   float w2;
   float w1;
} _ctx_type_6;

typedef _ctx_type_6 biquad_type;

_ctx_type_6 _ctx_type_6_init();

_ctx_type_6 biquad_init();

float biquad(_ctx_type_6 &_ctx, float x, float b0, float b1, float b2, float a1, float a2);

typedef struct _ctx_type_7 {
   float k;
   float fc;
   _ctx_type_6 _inst1;
   _ctx_type_5 _inst0;
} _ctx_type_7;

typedef _ctx_type_7 lp6_type;

_ctx_type_7 _ctx_type_7_init();

_ctx_type_7 lp6_init();

float lp6(_ctx_type_7 &_ctx, float x, float fc);

typedef struct _ctx_type_8 {
   float b2;
   float b1;
   float b0;
   float a2;
   float a1;
   _ctx_type_6 _inst2;
   _ctx_type_5 _inst1;
   _ctx_type_5 _inst0;
} _ctx_type_8;

typedef _ctx_type_8 lp12_type;

_ctx_type_8 _ctx_type_8_init();

_ctx_type_8 lp12_init();

float lp12(_ctx_type_8 &_ctx, float x, float fc, float q);

typedef struct _ctx_type_9 {
   _ctx_type_6 _inst0;
} _ctx_type_9;

typedef _ctx_type_9 hp6_type;

_ctx_type_9 _ctx_type_9_init();

_ctx_type_9 hp6_init();

float hp6(_ctx_type_9 &_ctx, float x, float fc);

typedef struct _ctx_type_10 {
   _ctx_type_6 _inst0;
} _ctx_type_10;

typedef _ctx_type_10 allp6_type;

_ctx_type_10 _ctx_type_10_init();

_ctx_type_10 allp6_init();

float allp6(_ctx_type_10 &_ctx, float x, float fc);

typedef struct _ctx_type_11 {
   float b2;
   float b1;
   float b0;
   float a2;
   float a1;
   _ctx_type_6 _inst2;
   _ctx_type_5 _inst1;
   _ctx_type_5 _inst0;
} _ctx_type_11;

typedef _ctx_type_11 hp12_type;

_ctx_type_11 _ctx_type_11_init();

_ctx_type_11 hp12_init();

float hp12(_ctx_type_11 &_ctx, float x, float fc, float q);

typedef struct _ctx_type_12 {
   float b2;
   float b1;
   float b0;
   float a2;
   float a1;
   _ctx_type_6 _inst2;
   _ctx_type_5 _inst1;
   _ctx_type_5 _inst0;
} _ctx_type_12;

typedef _ctx_type_12 bp12_type;

_ctx_type_12 _ctx_type_12_init();

_ctx_type_12 bp12_init();

float bp12(_ctx_type_12 &_ctx, float x, float fc, float q);

typedef struct _ctx_type_13 {
   float b2;
   float b1;
   float b0;
   float a2;
   float a1;
   _ctx_type_6 _inst2;
   _ctx_type_5 _inst1;
   _ctx_type_5 _inst0;
} _ctx_type_13;

typedef _ctx_type_13 notch12_type;

_ctx_type_13 _ctx_type_13_init();

_ctx_type_13 notch12_init();

float notch12(_ctx_type_13 &_ctx, float x, float fc, float q);

typedef struct _ctx_type_14 {
   float b2;
   float b1;
   float b0;
   float a2;
   float a1;
   _ctx_type_6 _inst2;
   _ctx_type_5 _inst1;
   _ctx_type_5 _inst0;
} _ctx_type_14;

typedef _ctx_type_14 allp12_type;

_ctx_type_14 _ctx_type_14_init();

_ctx_type_14 allp12_init();

float allp12(_ctx_type_14 &_ctx, float x, float fc, float q);

allp12(_ctx._inst3,0.f,100.f,0.5f);


#endif // FILTERS_H

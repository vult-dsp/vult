#ifndef ._H
#define ._H
#include <stdint.h>
#include <math.h>

typedef struct _ctx_type_0 {
   float x;
} _ctx_type_0;

typedef _ctx_type_0 smooth_type;

_ctx_type_0 _ctx_type_0_init();

_ctx_type_0 smooth_init();

float smooth(_ctx_type_0 &_ctx, float input);

typedef struct _ctx_type_1 {
   float pre_x;
} _ctx_type_1;

typedef _ctx_type_1 change_type;

_ctx_type_1 _ctx_type_1_init();

_ctx_type_1 change_init();

uint8_t change(_ctx_type_1 &_ctx, float x);

typedef struct _ctx_type_2 {
   int pre_x;
} _ctx_type_2;

typedef _ctx_type_2 edge_type;

_ctx_type_2 _ctx_type_2_init();

_ctx_type_2 edge_init();

uint8_t edge(_ctx_type_2 &_ctx, int x);

typedef struct _ctx_type_3 {
   int count;
} _ctx_type_3;

typedef _ctx_type_3 each_type;

_ctx_type_3 _ctx_type_3_init();

_ctx_type_3 each_init();

uint8_t each(_ctx_type_3 &_ctx, int n);

float pitchToRate(float d);

typedef struct _ctx_type_5 {
   float rate;
   float phase;
   _ctx_type_1 _inst0;
} _ctx_type_5;

typedef _ctx_type_5 phasor_type;

_ctx_type_5 _ctx_type_5_init();

_ctx_type_5 phasor_init();

float phasor(_ctx_type_5 &_ctx, float pitch, uint8_t reset);

typedef struct _ctx_type_6 {
   float phase;
   _ctx_type_2 _inst0;
} _ctx_type_6;

typedef _ctx_type_6 lfo_type;

_ctx_type_6 _ctx_type_6_init();

_ctx_type_6 lfo_init();

float lfo(_ctx_type_6 &_ctx, float f, int gate);

typedef struct _ctx_type_7 {
   float volume;
   float pre_phase1;
   float pitch;
   float n4;
   float n3;
   float n2;
   float n1;
   float lfo_rate;
   float lfo_amt;
   int gate;
   float detune;
   int count;
   _ctx_type_0 _inst5;
   _ctx_type_0 _inst4;
   _ctx_type_5 _inst3;
   _ctx_type_0 _inst2;
   _ctx_type_5 _inst1;
   _ctx_type_6 _inst0;
} _ctx_type_7;

typedef _ctx_type_7 process_type;

_ctx_type_7 _ctx_type_7_init();

_ctx_type_7 process_init();

float process(_ctx_type_7 &_ctx, float input);

typedef _ctx_type_7 noteOn_type;

_ctx_type_7 noteOn_init();

void noteOn(_ctx_type_7 &_ctx, float note, int velocity);

typedef _ctx_type_7 noteOff_type;

_ctx_type_7 noteOff_init();

void noteOff(_ctx_type_7 &_ctx, float note);

typedef _ctx_type_7 controlChange_type;

_ctx_type_7 controlChange_init();

void controlChange(_ctx_type_7 &_ctx, int control, float value);

typedef _ctx_type_7 default_type;

_ctx_type_7 default_init();

void default_(_ctx_type_7 &_ctx);



#endif // ._H

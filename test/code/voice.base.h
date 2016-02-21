#ifndef ._H
#define ._H
#include <stdint.h>
#include <math.h>

float minFixed();

typedef struct _ctx_type_1 {
   float pre_x;
} _ctx_type_1;

typedef _ctx_type_1 change_type;

_ctx_type_1 _ctx_type_1_init();

_ctx_type_1 change_init();

uint8_t change(_ctx_type_1 &_ctx, float x);

typedef struct _ctx_type_2 {
   uint8_t pre_x;
} _ctx_type_2;

typedef _ctx_type_2 bchange_type;

_ctx_type_2 _ctx_type_2_init();

_ctx_type_2 bchange_init();

uint8_t bchange(_ctx_type_2 &_ctx, uint8_t x);

typedef struct _ctx_type_3 {
   uint8_t pre_x;
} _ctx_type_3;

typedef _ctx_type_3 edge_type;

_ctx_type_3 _ctx_type_3_init();

_ctx_type_3 edge_init();

uint8_t edge(_ctx_type_3 &_ctx, uint8_t x);

typedef struct _ctx_type_4 {
   int count;
} _ctx_type_4;

typedef _ctx_type_4 each_type;

_ctx_type_4 _ctx_type_4_init();

_ctx_type_4 each_init();

uint8_t each(_ctx_type_4 &_ctx, int n);

uint8_t near_zero(float x);

typedef struct _ctx_type_6 {
   float y1;
   float x1;
} _ctx_type_6;

typedef _ctx_type_6 dcblock_type;

_ctx_type_6 _ctx_type_6_init();

_ctx_type_6 dcblock_init();

float dcblock(_ctx_type_6 &_ctx, float x0);

typedef struct _ctx_type_7 {
   float pre_x;
} _ctx_type_7;

typedef _ctx_type_7 lpfilter_type;

_ctx_type_7 _ctx_type_7_init();

_ctx_type_7 lpfilter_init();

float lpfilter(_ctx_type_7 &_ctx, float x);

float pitchToRate(float d);

float pulse_train(float m, float phase);

typedef struct _ctx_type_10 {
   float state_triang;
   float state_saw;
   float state_pulse;
   float rate;
   float phase;
   float output;
   float m;
   _ctx_type_6 _inst1;
   _ctx_type_1 _inst0;
} _ctx_type_10;

typedef _ctx_type_10 osc_type;

_ctx_type_10 _ctx_type_10_init();

_ctx_type_10 osc_init();

float osc(_ctx_type_10 &_ctx, float pitch, float pw, float wave);

typedef struct _ctx_type_11 {
   float dlow;
   float dband;
} _ctx_type_11;

typedef _ctx_type_11 svf_step_type;

_ctx_type_11 _ctx_type_11_init();

_ctx_type_11 svf_step_init();

float svf_step(_ctx_type_11 &_ctx, float input, float g, float q, int sel);

typedef struct _ctx_type_12 {
   _ctx_type_11 step;
   float g;
   _ctx_type_1 _inst0;
} _ctx_type_12;

typedef _ctx_type_12 svf_type;

_ctx_type_12 _ctx_type_12_init();

_ctx_type_12 svf_init();

float svf(_ctx_type_12 &_ctx, float input, float fc, float q, int sel);

typedef struct _ctx_type_13 {
   float value;
   float sustainLevel;
   int state;
   float releaseRate;
   uint8_t gate;
   float decayRate;
   float attackRate;
   _ctx_type_7 _inst2;
   _ctx_type_2 _inst1;
   _ctx_type_4 _inst0;
} _ctx_type_13;

typedef _ctx_type_13 adsr_type;

_ctx_type_13 _ctx_type_13_init();

_ctx_type_13 adsr_init();

float adsr(_ctx_type_13 &_ctx, uint8_t gate, float attack, float decay, float sustain, float release);

typedef struct _ctx_type_14 {
   float phase;
   _ctx_type_4 _inst1;
   _ctx_type_3 _inst0;
} _ctx_type_14;

typedef _ctx_type_14 lfo_type;

_ctx_type_14 _ctx_type_14_init();

_ctx_type_14 lfo_init();

float lfo(_ctx_type_14 &_ctx, float f, uint8_t gate);

typedef struct _ctx_type_15 {
   int pre;
   int n4;
   int n3;
   int n2;
   int n1;
   int count;
} _ctx_type_15;

typedef _ctx_type_15 noteOn_type;

_ctx_type_15 _ctx_type_15_init();

_ctx_type_15 noteOn_init();

int noteOn(_ctx_type_15 &_ctx, int n);

typedef _ctx_type_15 noteOff_type;

_ctx_type_15 noteOff_init();

int noteOff(_ctx_type_15 &_ctx, int n);

typedef _ctx_type_15 isGateOn_type;

_ctx_type_15 isGateOn_init();

uint8_t isGateOn(_ctx_type_15 &_ctx);

typedef struct _ctx_type_16 {
   float param9;
   float param8;
   float param7;
   float param6;
   float param5;
   float param4;
   float param3;
   float param2;
   float param16;
   float param15;
   float param14;
   float param13;
   float param12;
   float param11;
   float param10;
   float param1;
   _ctx_type_15 monoin;
   _ctx_type_12 _inst4;
   _ctx_type_13 _inst3;
   _ctx_type_13 _inst2;
   _ctx_type_10 _inst1;
   _ctx_type_14 _inst0;
} _ctx_type_16;

typedef _ctx_type_16 process_type;

_ctx_type_16 _ctx_type_16_init();

_ctx_type_16 process_init();

float process(_ctx_type_16 &_ctx, float i);

typedef _ctx_type_16 process_noteOn_type;

_ctx_type_16 process_noteOn_init();

void process_noteOn(_ctx_type_16 &_ctx, int n);

typedef _ctx_type_16 process_noteOff_type;

_ctx_type_16 process_noteOff_init();

void process_noteOff(_ctx_type_16 &_ctx, int n);



#endif // ._H

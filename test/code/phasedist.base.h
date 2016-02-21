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

float pitchToRate(float d);

typedef struct _ctx_type_3 {
   float rate;
   float phase;
   _ctx_type_1 _inst0;
} _ctx_type_3;

typedef _ctx_type_3 phasor_type;

_ctx_type_3 _ctx_type_3_init();

_ctx_type_3 phasor_init();

float phasor(_ctx_type_3 &_ctx, float pitch, uint8_t reset);

typedef struct _ctx_type_4 {
   float volume;
   float pre_phase1;
   float pitch;
   float detune;
   _ctx_type_0 _inst3;
   _ctx_type_3 _inst2;
   _ctx_type_0 _inst1;
   _ctx_type_3 _inst0;
} _ctx_type_4;

typedef _ctx_type_4 process_type;

_ctx_type_4 _ctx_type_4_init();

_ctx_type_4 process_init();

float process(_ctx_type_4 &_ctx, float input);

typedef _ctx_type_4 noteOn_type;

_ctx_type_4 noteOn_init();

void noteOn(_ctx_type_4 &_ctx, int note, int velocity);

typedef _ctx_type_4 noteOff_type;

_ctx_type_4 noteOff_init();

void noteOff(_ctx_type_4 &_ctx, int note);

typedef _ctx_type_4 controlChange_type;

_ctx_type_4 controlChange_init();

void controlChange(_ctx_type_4 &_ctx, int control, int value);

typedef _ctx_type_4 default_type;

_ctx_type_4 default_init();

void default_(_ctx_type_4 &_ctx);



#endif // ._H

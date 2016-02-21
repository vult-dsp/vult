#include "blit.h"

uint8_t near_zero(float x){
   return (fabsf(x) < 0.02f);
}

_ctx_type_1 _ctx_type_1_init(){
   _ctx_type_1 _ctx;
   _ctx.pre_x = 0.f;
   return _ctx;
}

_ctx_type_1 change_init(){ return _ctx_type_1_init();}

uint8_t change(_ctx_type_1 &_ctx, float x){
   uint8_t v = (_ctx.pre_x != x);
   _ctx.pre_x = x;
   return v;
}

_ctx_type_2 _ctx_type_2_init(){
   _ctx_type_2 _ctx;
   _ctx.y1 = 0.f;
   _ctx.x1 = 0.f;
   return _ctx;
}

_ctx_type_2 dcblock_init(){ return _ctx_type_2_init();}

float dcblock(_ctx_type_2 &_ctx, float x0){
   float y0 = ((x0 - _ctx.x1) + (_ctx.y1 * 0.995f));
   _ctx.x1 = x0;
   _ctx.y1 = y0;
   return y0;
}

float pitchToRate(float d){
   return ((8.1758f * expf((0.0577623f * d))) / 44100.f);
}

float pulse_train(float m, float phase){
   float pi_phase = (phase * 3.14159265359f);
   float denominator1 = sinf(pi_phase);
   float tmp1 = 0.f;
   if(near_zero(denominator1)){
      tmp1 = 1.f;
   }
   else
   {
      tmp1 = sinf((m * pi_phase));
      tmp1 = (tmp1 / (m * denominator1));
   }
   return tmp1;
}

_ctx_type_5 _ctx_type_5_init(){
   _ctx_type_5 _ctx;
   _ctx.state_triang = 0.f;
   _ctx.state_saw = 0.f;
   _ctx.state_pulse = 0.f;
   _ctx.rate = 0.f;
   _ctx.phase = 0.f;
   _ctx.output = 0.f;
   _ctx.m = 0.f;
   _ctx._inst1 = _ctx_type_2_init();
   _ctx._inst0 = _ctx_type_1_init();
   return _ctx;
}

_ctx_type_5 osc_init(){ return _ctx_type_5_init();}

float osc(_ctx_type_5 &_ctx, float pitch, float pw, float wave){
   float fixed_pitch = 0.f;
   if(wave < (2.f / 3.f)){
      fixed_pitch = pitch;
   }
   else
   {
      fixed_pitch = (pitch + 12.f);
   }
   if(change(_ctx._inst0,fixed_pitch)){
      _ctx.rate = pitchToRate(fixed_pitch);
      float p = (1.f / _ctx.rate);
      float maxHarmonics = floorf((p / 2.f));
      _ctx.m = ((2.f * maxHarmonics) + 1.f);
   }
   float shift05 = (0.5f + (pw * 0.49f));
   float shift = (_ctx.phase + shift05);
   if(shift > 1.f){
      shift = (shift - 1.f);
   }
   float tmp1 = pulse_train(_ctx.m,_ctx.phase);
   float tmp2 = pulse_train(_ctx.m,shift);
   _ctx.phase = (_ctx.phase + _ctx.rate);
   if(_ctx.phase > 1.f){
      _ctx.phase = (_ctx.phase - 1.f);
   }
   _ctx.state_pulse = clip_float((((_ctx.state_pulse * 0.9995f) + tmp1) - tmp2),(- 1.f),1.f);
   _ctx.state_saw = clip_float(((_ctx.state_saw * 0.9995f) + ((((tmp1 + tmp2) - (2.f * _ctx.rate)) / shift05) / 2.f)),(- 1.f),1.f);
   _ctx.state_triang = clip_float(((_ctx.state_triang * 0.9995f) + ((2.f * _ctx.state_pulse) * _ctx.rate)),(- 1.f),1.f);
   if(wave < (1.f / 3.f)){
      _ctx.output = _ctx.state_pulse;
   }
   else
   {
      if(wave < (2.f / 3.f)){
         _ctx.output = (2.f * _ctx.state_saw);
      }
      else
      {
         _ctx.output = ((2.f * _ctx.state_triang) * (1.f + pw));
      }
   }
   _ctx.output = dcblock(_ctx._inst1,_ctx.output);
   return clip_float((_ctx.output / 4.f),(- 1.f),1.f);
}


#include "..h"

_ctx_type_0 _ctx_type_0_init(){
   _ctx_type_0 _ctx;
   _ctx.x = 0.f;
   return _ctx;
}

_ctx_type_0 smooth_init(){ return _ctx_type_0_init();}

float smooth(_ctx_type_0 &_ctx, float input){
   _ctx.x = (_ctx.x + ((input - _ctx.x) * 0.005f));
   return _ctx.x;
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
   _ctx.pre_x = 0;
   return _ctx;
}

_ctx_type_2 edge_init(){ return _ctx_type_2_init();}

uint8_t edge(_ctx_type_2 &_ctx, int x){
   uint8_t v = ((_ctx.pre_x != x) && (_ctx.pre_x == 0));
   _ctx.pre_x = x;
   return v;
}

_ctx_type_3 _ctx_type_3_init(){
   _ctx_type_3 _ctx;
   _ctx.count = 0;
   return _ctx;
}

_ctx_type_3 each_init(){ return _ctx_type_3_init();}

uint8_t each(_ctx_type_3 &_ctx, int n){
   uint8_t ret = (_ctx.count == 0);
   _ctx.count = ((_ctx.count + 1) % n);
   return ret;
}

float pitchToRate(float d){
   return ((8.1758f * expf((0.0577623f * d))) / 44100.f);
}

_ctx_type_5 _ctx_type_5_init(){
   _ctx_type_5 _ctx;
   _ctx.rate = 0.f;
   _ctx.phase = 0.f;
   _ctx._inst0 = _ctx_type_1_init();
   return _ctx;
}

_ctx_type_5 phasor_init(){ return _ctx_type_5_init();}

float phasor(_ctx_type_5 &_ctx, float pitch, uint8_t reset){
   if(change(_ctx._inst0,pitch)){
      _ctx.rate = pitchToRate(pitch);
   }
   _ctx.phase = (reset?0.f:fmodf((_ctx.phase + _ctx.rate),1.f));
   return _ctx.phase;
}

_ctx_type_6 _ctx_type_6_init(){
   _ctx_type_6 _ctx;
   _ctx.phase = 0.f;
   _ctx._inst0 = _ctx_type_2_init();
   return _ctx;
}

_ctx_type_6 lfo_init(){ return _ctx_type_6_init();}

float lfo(_ctx_type_6 &_ctx, float f, int gate){
   float rate = ((f * 10.f) / 44100.f);
   if(edge(_ctx._inst0,gate)){
      _ctx.phase = 0.f;
   }
   _ctx.phase = (_ctx.phase + rate);
   if(_ctx.phase > 1.f){
      _ctx.phase = (_ctx.phase - 1.f);
   }
   return (sinf(((_ctx.phase * 2.f) * 3.14159265359f)) - 0.5f);
}

_ctx_type_7 _ctx_type_7_init(){
   _ctx_type_7 _ctx;
   _ctx.volume = 0.f;
   _ctx.pre_phase1 = 0.f;
   _ctx.pitch = 0.f;
   _ctx.n4 = 0.f;
   _ctx.n3 = 0.f;
   _ctx.n2 = 0.f;
   _ctx.n1 = 0.f;
   _ctx.lfo_rate = 0.f;
   _ctx.lfo_amt = 0.f;
   _ctx.gate = 0;
   _ctx.detune = 0.f;
   _ctx.count = 0;
   _ctx._inst5 = _ctx_type_0_init();
   _ctx._inst4 = _ctx_type_0_init();
   _ctx._inst3 = _ctx_type_5_init();
   _ctx._inst2 = _ctx_type_0_init();
   _ctx._inst1 = _ctx_type_5_init();
   _ctx._inst0 = _ctx_type_6_init();
   return _ctx;
}

_ctx_type_7 process_init(){ return _ctx_type_7_init();}

float process(_ctx_type_7 &_ctx, float input){
   float lfo_val = (lfo(_ctx._inst0,_ctx.lfo_rate,_ctx.gate) * _ctx.lfo_amt);
   float phase1 = phasor(_ctx._inst1,_ctx.pitch,0);
   float comp = (1.f - phase1);
   uint8_t reset = ((_ctx.pre_phase1 - phase1) > 0.5f);
   _ctx.pre_phase1 = phase1;
   float phase2 = phasor(_ctx._inst3,(_ctx.pitch + (smooth(_ctx._inst2,(_ctx.detune + lfo_val)) * 32.f)),reset);
   float sine = sinf(((2.f * 3.14159265359f) * phase2));
   float gate_value = ((_ctx.gate > 0)?1.f:0.f);
   return ((smooth(_ctx._inst4,_ctx.volume) * (sine * comp)) * smooth(_ctx._inst5,gate_value));
}

_ctx_type_7 noteOn_init(){ return _ctx_type_7_init();}

void noteOn(_ctx_type_7 &_ctx, float note, int velocity){
   if(_ctx.count == 0){
      _ctx.n1 = note;
      _ctx.pitch = note;
   }
   else
   {
      if(_ctx.count == 1){
         _ctx.n2 = note;
         _ctx.pitch = note;
      }
      else
      {
         if(_ctx.count == 2){
            _ctx.n3 = note;
            _ctx.pitch = note;
         }
         else
         {
            if(_ctx.count == 3){
               _ctx.n4 = note;
               _ctx.pitch = note;
            }
         }
      }
   }
   if(_ctx.count <= 4){
      _ctx.count = (_ctx.count + 1);
   }
   _ctx.gate = ((_ctx.count > 0)?1:0);
}

_ctx_type_7 noteOff_init(){ return _ctx_type_7_init();}

void noteOff(_ctx_type_7 &_ctx, float note){
   uint8_t found = 0;
   if(note == _ctx.n1){
      float _tmp_0 = _ctx.n2;
      float _tmp_1 = _ctx.n3;
      float _tmp_2 = _ctx.n4;
      _ctx.n1 = _tmp_0;
      _ctx.n2 = _tmp_1;
      _ctx.n3 = _tmp_2;
      found = 1;
   }
   else
   {
      if(note == _ctx.n2){
         float _tmp_0 = _ctx.n3;
         float _tmp_1 = _ctx.n4;
         _ctx.n2 = _tmp_0;
         _ctx.n3 = _tmp_1;
         found = 1;
      }
      else
      {
         if(note == _ctx.n3){
            _ctx.n3 = _ctx.n4;
            found = 1;
         }
         else
         {
            if(note == _ctx.n4){
               found = 1;
            }
         }
      }
   }
   if(found && (_ctx.count > 0)){
      _ctx.count = (_ctx.count - 1);
   }
   _ctx.gate = ((_ctx.count > 0)?1:0);
   if(_ctx.count == 1){
      _ctx.pitch = _ctx.n1;
   }
   if(_ctx.count == 2){
      _ctx.pitch = _ctx.n2;
   }
   if(_ctx.count == 3){
      _ctx.pitch = _ctx.n3;
   }
   if(_ctx.count == 4){
      _ctx.pitch = _ctx.n4;
   }
}

_ctx_type_7 controlChange_init(){ return _ctx_type_7_init();}

void controlChange(_ctx_type_7 &_ctx, int control, float value){
   if(control == 30){
      _ctx.volume = (value / 127.f);
   }
   if(control == 31){
      _ctx.detune = (value / 127.f);
   }
   if(control == 32){
      _ctx.lfo_rate = (value / 127.f);
   }
   if(control == 33){
      _ctx.lfo_amt = (2.f * ((((float)value) / 127.f) - 0.5f));
   }
}

_ctx_type_7 default_init(){ return _ctx_type_7_init();}

void default_(_ctx_type_7 &_ctx){
   _ctx.volume = 0.f;
   _ctx.pitch = 45.f;
   _ctx.detune = 0.8f;
   _ctx.lfo_rate = 0.07f;
   _ctx.lfo_amt = (- 0.8f);
}



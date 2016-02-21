#include "volume.h"

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
   _ctx.volume = 0.f;
   _ctx._inst0 = _ctx_type_0_init();
   return _ctx;
}

_ctx_type_1 process_init(){ return _ctx_type_1_init();}

float process(_ctx_type_1 &_ctx, float input){
   return (input * smooth(_ctx._inst0,_ctx.volume));
}

_ctx_type_1 noteOn_init(){ return _ctx_type_1_init();}

void noteOn(_ctx_type_1 &_ctx, int note, int velocity){
}

_ctx_type_1 noteOff_init(){ return _ctx_type_1_init();}

void noteOff(_ctx_type_1 &_ctx, int note){
}

_ctx_type_1 controlChange_init(){ return _ctx_type_1_init();}

void controlChange(_ctx_type_1 &_ctx, int control, int value){
   if(control == 30){
      _ctx.volume = (((float)value) / 127.f);
   }
}

_ctx_type_1 default_init(){ return _ctx_type_1_init();}

void default_(_ctx_type_1 &_ctx){
   _ctx.volume = 0.f;
}



#include "monoin.h"

_ctx_type_0 _ctx_type_0_init(){
   _ctx_type_0 _ctx;
   _ctx.pre = 0;
   _ctx.n4 = 0;
   _ctx.n3 = 0;
   _ctx.n2 = 0;
   _ctx.n1 = 0;
   _ctx.count = 0;
   return _ctx;
}

_ctx_type_0 noteOn_init(){ return _ctx_type_0_init();}

int noteOn(_ctx_type_0 &_ctx, int n){
   if(_ctx.count == 0){
      _ctx.n1 = n;
      _ctx.pre = n;
   }
   else
   {
      if(_ctx.count == 1){
         _ctx.n2 = n;
         _ctx.pre = n;
      }
      else
      {
         if(_ctx.count == 2){
            _ctx.n3 = n;
            _ctx.pre = n;
         }
         else
         {
            if(_ctx.count == 3){
               _ctx.n4 = n;
               _ctx.pre = n;
            }
         }
      }
   }
   if(_ctx.count <= 4){
      _ctx.count = (_ctx.count + 1);
   }
   return _ctx.pre;
}

_ctx_type_0 noteOff_init(){ return _ctx_type_0_init();}

int noteOff(_ctx_type_0 &_ctx, int n){
   uint8_t found = 0;
   if(n == _ctx.n1){
      int _tmp_0 = _ctx.n2;
      int _tmp_1 = _ctx.n3;
      int _tmp_2 = _ctx.n4;
      _ctx.n1 = _tmp_0;
      _ctx.n2 = _tmp_1;
      _ctx.n3 = _tmp_2;
      found = 1;
   }
   else
   {
      if(n == _ctx.n2){
         int _tmp_0 = _ctx.n3;
         int _tmp_1 = _ctx.n4;
         _ctx.n2 = _tmp_0;
         _ctx.n3 = _tmp_1;
         found = 1;
      }
      else
      {
         if(n == _ctx.n3){
            _ctx.n3 = _ctx.n4;
            found = 1;
         }
         else
         {
            if(n == _ctx.n4){
               found = 1;
            }
         }
      }
   }
   if(found && (_ctx.count > 0)){
      _ctx.count = (_ctx.count - 1);
   }
   if(_ctx.count == 1){
      _ctx.pre = _ctx.n1;
   }
   if(_ctx.count == 2){
      _ctx.pre = _ctx.n2;
   }
   if(_ctx.count == 3){
      _ctx.pre = _ctx.n3;
   }
   if(_ctx.count == 4){
      _ctx.pre = _ctx.n4;
   }
   return _ctx.pre;
}

_ctx_type_0 isGateOn_init(){ return _ctx_type_0_init();}

uint8_t isGateOn(_ctx_type_0 &_ctx){
   return (_ctx.count > 0);
}



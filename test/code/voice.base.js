function vultProcess(){
    this.clip = function(x,low,high) { return x<low?low:(x>high?high:x); };
    this.not  = function(x)          { return x==0?1:0; };
    this.real = function(x)          { return x; };
    this.int  = function(x)          { return x|0; };
    this.sin  = function(x)          { return Math.sin(x); };
    this.cos  = function(x)          { return Math.cos(x); };
    this.abs  = function(x)          { return Math.abs(x); };
    this.exp  = function(x)          { return Math.exp(x); };
    this.floor= function(x)          { return Math.floor(x); };
    this.tan  = function(x)          { return Math.tan(x); };
    this.tanh = function(x)          { return Math.tanh(x); };
    this.sqrt = function(x)          { return x; };
    this.process_init = null;
    this.default_ = null;
this.minFixed = function(){
   return 1.52588e-05;
}


this._ctx_type_1_init = function(){
   var _ctx = {};
   _ctx.pre_x = 0.;
   return _ctx;
}
this. change_init = function() { return this._ctx_type_1_init();}
this.change = function(_ctx,x){
   var v = (_ctx.pre_x != x);
   _ctx.pre_x = x;
   return v;
}


this._ctx_type_2_init = function(){
   var _ctx = {};
   _ctx.pre_x = false;
   return _ctx;
}
this. bchange_init = function() { return this._ctx_type_2_init();}
this.bchange = function(_ctx,x){
   var v = (_ctx.pre_x != x);
   _ctx.pre_x = x;
   return v;
}


this._ctx_type_3_init = function(){
   var _ctx = {};
   _ctx.pre_x = false;
   return _ctx;
}
this. edge_init = function() { return this._ctx_type_3_init();}
this.edge = function(_ctx,x){
   var v = ((_ctx.pre_x != x) && (_ctx.pre_x == true));
   _ctx.pre_x = x;
   return v;
}


this._ctx_type_4_init = function(){
   var _ctx = {};
   _ctx.count = ((0|0)|0);
   return _ctx;
}
this. each_init = function() { return this._ctx_type_4_init();}
this.each = function(_ctx,n){
   var ret = (_ctx.count == (0|0));
   _ctx.count = (((_ctx.count + (1|0)) % n)|0);
   return ret;
}
this.near_zero = function(x){
   return (this.abs(x) < 0.02);
}


this._ctx_type_6_init = function(){
   var _ctx = {};
   _ctx.y1 = 0.;
   _ctx.x1 = 0.;
   return _ctx;
}
this. dcblock_init = function() { return this._ctx_type_6_init();}
this.dcblock = function(_ctx,x0){
   var y0 = ((x0 - _ctx.x1) + (_ctx.y1 * 0.995));
   _ctx.x1 = x0;
   _ctx.y1 = y0;
   return y0;
}


this._ctx_type_7_init = function(){
   var _ctx = {};
   _ctx.pre_x = 0.;
   return _ctx;
}
this. lpfilter_init = function() { return this._ctx_type_7_init();}
this.lpfilter = function(_ctx,x){
   var ret = ((x + _ctx.pre_x) / 2.);
   _ctx.pre_x = x;
   return ret;
}
this.pitchToRate = function(d){
   return ((8.1758 * this.exp((0.0577623 * d))) / 44100.);
}
this.pulse_train = function(m,phase){
   var pi_phase = (phase * 3.14159265359);
   var denominator1 = this.sin(pi_phase);
   var tmp1 = 0.;
   if(this.near_zero(denominator1)){
      tmp1 = 1.;
   }
   else
   {
      tmp1 = this.sin((m * pi_phase));
      tmp1 = (tmp1 / (m * denominator1));
   }
   return tmp1;
}


this._ctx_type_10_init = function(){
   var _ctx = {};
   _ctx.state_triang = 0.;
   _ctx.state_saw = 0.;
   _ctx.state_pulse = 0.;
   _ctx.rate = 0.;
   _ctx.phase = 0.;
   _ctx.output = 0.;
   _ctx.m = 0.;
   _ctx._inst1 = this._ctx_type_6_init();
   _ctx._inst0 = this._ctx_type_1_init();
   return _ctx;
}
this. osc_init = function() { return this._ctx_type_10_init();}
this.osc = function(_ctx,pitch,pw,wave){
   var fixed_pitch = 0.;
   if((wave < (2. / 3.))){
      fixed_pitch = pitch;
   }
   else
   {
      fixed_pitch = (pitch + 12.);
   }
   if(this.change(_ctx._inst0,fixed_pitch)){
      _ctx.rate = this.pitchToRate(fixed_pitch);
      var p = (1. / _ctx.rate);
      var maxHarmonics = this.floor((p / 2.));
      _ctx.m = ((2. * maxHarmonics) + 1.);
   }
   var shift05 = (0.5 + (pw * 0.49));
   var shift = (_ctx.phase + shift05);
   if((shift > 1.)){
      shift = (shift - 1.);
   }
   var tmp1 = this.pulse_train(_ctx.m,_ctx.phase);
   var tmp2 = this.pulse_train(_ctx.m,shift);
   _ctx.phase = (_ctx.phase + _ctx.rate);
   if((_ctx.phase > 1.)){
      _ctx.phase = (_ctx.phase - 1.);
   }
   _ctx.state_pulse = this.clip((((_ctx.state_pulse * 0.9995) + tmp1) - tmp2),(- 1.),1.);
   _ctx.state_saw = this.clip(((_ctx.state_saw * 0.9995) + ((((tmp1 + tmp2) - (2. * _ctx.rate)) / shift05) / 2.)),(- 1.),1.);
   _ctx.state_triang = this.clip(((_ctx.state_triang * 0.9995) + ((2. * _ctx.state_pulse) * _ctx.rate)),(- 1.),1.);
   if((wave < (1. / 3.))){
      _ctx.output = _ctx.state_pulse;
   }
   else
   {
      if((wave < (2. / 3.))){
         _ctx.output = (2. * _ctx.state_saw);
      }
      else
      {
         _ctx.output = ((2. * _ctx.state_triang) * (1. + pw));
      }
   }
   _ctx.output = this.dcblock(_ctx._inst1,_ctx.output);
   return this.clip((_ctx.output / 4.),(- 1.),1.);
}


this._ctx_type_11_init = function(){
   var _ctx = {};
   _ctx.dlow = 0.;
   _ctx.dband = 0.;
   return _ctx;
}
this. svf_step_init = function() { return this._ctx_type_11_init();}
this.svf_step = function(_ctx,input,g,q,sel){
   var low = (_ctx.dlow + (g * _ctx.dband));
   var high = ((input - low) - (q * _ctx.dband));
   var band = ((g * high) + _ctx.dband);
   var notch = (high + low);
   _ctx.dband = this.clip(band,(- 1.),1.);
   _ctx.dlow = this.clip(low,(- 1.),1.);
   var output = ((sel == (0|0))?low:((sel == (1|0))?high:((sel == (2|0))?band:notch)));
   return output;
}


this._ctx_type_12_init = function(){
   var _ctx = {};
   _ctx.step = this._ctx_type_11_init();
   _ctx.g = 0.;
   _ctx._inst0 = this._ctx_type_1_init();
   return _ctx;
}
this. svf_init = function() { return this._ctx_type_12_init();}
this.svf = function(_ctx,input,fc,q,sel){
   fc = this.clip(fc,0.,1.);
   q = this.clip(q,0.,1.);
   var fix_q = (2. * (1. - q));
   if(this.change(_ctx._inst0,fc)){
      _ctx.g = (fc / 2.);
   }
   var x1 = this.svf_step(_ctx.step,input,_ctx.g,fix_q,sel);
   var x2 = this.svf_step(_ctx.step,input,_ctx.g,fix_q,sel);
   return ((x1 + x2) / 2.);
}


this._ctx_type_13_init = function(){
   var _ctx = {};
   _ctx.value = 0.;
   _ctx.sustainLevel = 0.;
   _ctx.state = ((0|0)|0);
   _ctx.releaseRate = 0.;
   _ctx.gate = false;
   _ctx.decayRate = 0.;
   _ctx.attackRate = 0.;
   _ctx._inst2 = this._ctx_type_7_init();
   _ctx._inst1 = this._ctx_type_2_init();
   _ctx._inst0 = this._ctx_type_4_init();
   return _ctx;
}
this. adsr_init = function() { return this._ctx_type_13_init();}
this.adsr = function(_ctx,gate,attack,decay,sustain,release){
   var IDLE = ((0|0)|0);
   var ATTACK = ((1|0)|0);
   var DECAY = ((2|0)|0);
   var SUSTAIN = ((3|0)|0);
   var RELEASE = ((4|0)|0);
   if(this.each(_ctx._inst0,(32|0))){
      _ctx.attackRate = ((1. / 44100.) * (1. / (attack + 0.1)));
      var inv_sustain = (1. - sustain);
      _ctx.decayRate = ((inv_sustain / 44100.) * (inv_sustain / (decay + 0.1)));
      _ctx.releaseRate = ((0.5 / 44100.) * (0.5 / (release + 0.1)));
   }
   _ctx.sustainLevel = sustain;
   var trig = false;
   var rate = 0.;
   trig = this.bchange(_ctx._inst1,_ctx.gate);
   var up = (trig && _ctx.gate);
   var down = (trig && this.not(_ctx.gate));
   if((_ctx.state == IDLE)){
      if(up){
         _ctx.state = (ATTACK|0);
      }
   }
   else
   {
      if((_ctx.state == ATTACK)){
         if((_ctx.value >= 1.)){
            _ctx.state = (DECAY|0);
         }
         if(down){
            _ctx.state = (RELEASE|0);
         }
         rate = _ctx.attackRate;
      }
      else
      {
         if((_ctx.state == DECAY)){
            if((_ctx.value <= _ctx.sustainLevel)){
               _ctx.state = (SUSTAIN|0);
            }
            if(down){
               _ctx.state = (RELEASE|0);
            }
            rate = (- _ctx.decayRate);
         }
         else
         {
            if((_ctx.state == SUSTAIN)){
               if(down){
                  _ctx.state = (RELEASE|0);
               }
               rate = 0.;
               _ctx.value = _ctx.sustainLevel;
            }
            else
            {
               if((_ctx.state == RELEASE)){
                  if((_ctx.value <= 0.)){
                     _ctx.state = (IDLE|0);
                  }
                  if(up){
                     _ctx.state = (ATTACK|0);
                  }
                  rate = (- _ctx.releaseRate);
               }
            }
         }
      }
   }
   _ctx.value = this.clip((this.lpfilter(_ctx._inst2,rate) + _ctx.value),0.,1.);
   return _ctx.value;
}


this._ctx_type_14_init = function(){
   var _ctx = {};
   _ctx.phase = 0.;
   _ctx._inst1 = this._ctx_type_4_init();
   _ctx._inst0 = this._ctx_type_3_init();
   return _ctx;
}
this. lfo_init = function() { return this._ctx_type_14_init();}
this.lfo = function(_ctx,f,gate){
   var rate = (((f * 100.) * this.minFixed()) + this.minFixed());
   if(this.edge(_ctx._inst0,gate)){
      _ctx.phase = 0.;
   }
   if(this.each(_ctx._inst1,(4|0))){
      _ctx.phase = (_ctx.phase + rate);
   }
   if((_ctx.phase > 1.)){
      _ctx.phase = (_ctx.phase - 1.);
   }
   return (this.sin(((_ctx.phase * 2.) * 3.14159265359)) + 0.5);
}


this._ctx_type_15_init = function(){
   var _ctx = {};
   _ctx.pre = ((0|0)|0);
   _ctx.n4 = ((0|0)|0);
   _ctx.n3 = ((0|0)|0);
   _ctx.n2 = ((0|0)|0);
   _ctx.n1 = ((0|0)|0);
   _ctx.count = ((0|0)|0);
   return _ctx;
}
this. noteOn_init = function() { return this._ctx_type_15_init();}
this.noteOn = function(_ctx,n){
   if((_ctx.count == (0|0))){
      _ctx.n1 = (n|0);
      _ctx.pre = (n|0);
   }
   else
   {
      if((_ctx.count == (1|0))){
         _ctx.n2 = (n|0);
         _ctx.pre = (n|0);
      }
      else
      {
         if((_ctx.count == (2|0))){
            _ctx.n3 = (n|0);
            _ctx.pre = (n|0);
         }
         else
         {
            if((_ctx.count == (3|0))){
               _ctx.n4 = (n|0);
               _ctx.pre = (n|0);
            }
         }
      }
   }
   if((_ctx.count <= (4|0))){
      _ctx.count = ((_ctx.count + (1|0))|0);
   }
   return _ctx.pre;
}

this. noteOff_init = function() { return this._ctx_type_15_init();}
this.noteOff = function(_ctx,n){
   var found = false;
   if((n == _ctx.n1)){
      var _tmp_0 = (_ctx.n2|0);
      var _tmp_1 = (_ctx.n3|0);
      var _tmp_2 = (_ctx.n4|0);
      _ctx.n1 = (_tmp_0|0);
      _ctx.n2 = (_tmp_1|0);
      _ctx.n3 = (_tmp_2|0);
      found = true;
   }
   else
   {
      if((n == _ctx.n2)){
         var _tmp_0 = (_ctx.n3|0);
         var _tmp_1 = (_ctx.n4|0);
         _ctx.n2 = (_tmp_0|0);
         _ctx.n3 = (_tmp_1|0);
         found = true;
      }
      else
      {
         if((n == _ctx.n3)){
            _ctx.n3 = (_ctx.n4|0);
            found = true;
         }
         else
         {
            if((n == _ctx.n4)){
               found = true;
            }
         }
      }
   }
   if((found && (_ctx.count > (0|0)))){
      _ctx.count = ((_ctx.count - (1|0))|0);
   }
   if((_ctx.count == (1|0))){
      _ctx.pre = (_ctx.n1|0);
   }
   if((_ctx.count == (2|0))){
      _ctx.pre = (_ctx.n2|0);
   }
   if((_ctx.count == (3|0))){
      _ctx.pre = (_ctx.n3|0);
   }
   if((_ctx.count == (4|0))){
      _ctx.pre = (_ctx.n4|0);
   }
   return _ctx.pre;
}

this. isGateOn_init = function() { return this._ctx_type_15_init();}
this.isGateOn = function(_ctx){
   return (_ctx.count > (0|0));
}


this._ctx_type_16_init = function(){
   var _ctx = {};
   _ctx.param9 = 0.;
   _ctx.param8 = 0.;
   _ctx.param7 = 0.;
   _ctx.param6 = 0.;
   _ctx.param5 = 0.;
   _ctx.param4 = 0.;
   _ctx.param3 = 0.;
   _ctx.param2 = 0.;
   _ctx.param16 = 0.;
   _ctx.param15 = 0.;
   _ctx.param14 = 0.;
   _ctx.param13 = 0.;
   _ctx.param12 = 0.;
   _ctx.param11 = 0.;
   _ctx.param10 = 0.;
   _ctx.param1 = 0.;
   _ctx.monoin = this._ctx_type_15_init();
   _ctx._inst4 = this._ctx_type_12_init();
   _ctx._inst3 = this._ctx_type_13_init();
   _ctx._inst2 = this._ctx_type_13_init();
   _ctx._inst1 = this._ctx_type_10_init();
   _ctx._inst0 = this._ctx_type_14_init();
   return _ctx;
}
this. process_init = function() { return this._ctx_type_16_init();}
this.process = function(_ctx,i){
   var gate = this.isGateOn(_ctx.monoin);
   var lfo1 = (this.lfo(_ctx._inst0,_ctx.param5,gate) * _ctx.param4);
   var x = this.osc(_ctx._inst1,_ctx.param1,(_ctx.param2 + lfo1),_ctx.param3);
   var amp_env = this.adsr(_ctx._inst2,gate,_ctx.param9,_ctx.param10,_ctx.param11,_ctx.param12);
   var flt_env = this.adsr(_ctx._inst3,gate,_ctx.param13,_ctx.param14,_ctx.param15,_ctx.param16);
   var cut_mod = (_ctx.param6 + (_ctx.param8 * flt_env));
   var output = this.svf(_ctx._inst4,x,cut_mod,_ctx.param7,(0|0));
   return (output / 2.);
}

this. process_noteOn_init = function() { return this._ctx_type_16_init();}
this.process_noteOn = function(_ctx,n){
   this.noteOn(_ctx.monoin,n);
}

this. process_noteOff_init = function() { return this._ctx_type_16_init();}
this.process_noteOff = function(_ctx,n){
   this.noteOff(_ctx.monoin,n);
}

    if(this.process_init)  this.context =  this.process_init(); else this.context = {};
    if(this.default_)      this.default_(this.context);
    this.liveNoteOn        = function(note,velocity) { if(this.noteOn)        this.noteOn(this.context,note,velocity); };
    this.liveNoteOff       = function(note,velocity) { if(this.noteOff)       this.noteOff(this.context,note,velocity); };
    this.liveControlChange = function(note,velocity) { if(this.controlChange) this.controlChange(this.context,note,velocity); };
    this.liveProcess       = function(input)         { if(this.process)       return this.process(this.context,input); else return 0; };
    this.liveDefault       = function()              { if(this.default_)      return this.default_(this.context); };
}
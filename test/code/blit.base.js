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
this.near_zero = function(x){
   return (this.abs(x) < 0.02);
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
   _ctx.y1 = 0.;
   _ctx.x1 = 0.;
   return _ctx;
}
this. dcblock_init = function() { return this._ctx_type_2_init();}
this.dcblock = function(_ctx,x0){
   var y0 = ((x0 - _ctx.x1) + (_ctx.y1 * 0.995));
   _ctx.x1 = x0;
   _ctx.y1 = y0;
   return y0;
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


this._ctx_type_5_init = function(){
   var _ctx = {};
   _ctx.state_triang = 0.;
   _ctx.state_saw = 0.;
   _ctx.state_pulse = 0.;
   _ctx.rate = 0.;
   _ctx.phase = 0.;
   _ctx.output = 0.;
   _ctx.m = 0.;
   _ctx._inst1 = this._ctx_type_2_init();
   _ctx._inst0 = this._ctx_type_1_init();
   return _ctx;
}
this. osc_init = function() { return this._ctx_type_5_init();}
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

    if(this.process_init)  this.context =  this.process_init(); else this.context = {};
    if(this.default_)      this.default_(this.context);
    this.liveNoteOn        = function(note,velocity) { if(this.noteOn)        this.noteOn(this.context,note,velocity); };
    this.liveNoteOff       = function(note,velocity) { if(this.noteOff)       this.noteOff(this.context,note,velocity); };
    this.liveControlChange = function(note,velocity) { if(this.controlChange) this.controlChange(this.context,note,velocity); };
    this.liveProcess       = function(input)         { if(this.process)       return this.process(this.context,input); else return 0; };
    this.liveDefault       = function()              { if(this.default_)      return this.default_(this.context); };
}
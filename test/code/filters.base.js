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
this.samplerate = function(){
   return 44100.;
}
this.pi = function(){
   return 3.1416;
}
this.min = function(a,b){
   return ((a < b)?a:b);
}
this.max = function(a,b){
   return ((a > b)?a:b);
}
this.clip = function(low,high,value){
   return this.min(this.max(low,value),high);
}


this._ctx_type_5_init = function(){
   var _ctx = {};
   _ctx.pre_x = 0.;
   return _ctx;
}
this. change_init = function() { return this._ctx_type_5_init();}
this.change = function(_ctx,x){
   var v = (_ctx.pre_x != x);
   _ctx.pre_x = x;
   return v;
}


this._ctx_type_6_init = function(){
   var _ctx = {};
   _ctx.w2 = 0.;
   _ctx.w1 = 0.;
   return _ctx;
}
this. biquad_init = function() { return this._ctx_type_6_init();}
this.biquad = function(_ctx,x,b0,b1,b2,a1,a2){
   var w0 = ((x - (a1 * _ctx.w1)) - (a2 * _ctx.w2));
   var y0 = (((b0 * w0) + (b1 * _ctx.w1)) + (b2 * _ctx.w2));
   var _tmp_0 = _ctx.w1;
   var _tmp_1 = w0;
   _ctx.w2 = _tmp_0;
   _ctx.w1 = _tmp_1;
   return y0;
}


this._ctx_type_7_init = function(){
   var _ctx = {};
   _ctx.k = 0.;
   _ctx.fc = 0.;
   _ctx._inst1 = this._ctx_type_6_init();
   _ctx._inst0 = this._ctx_type_5_init();
   return _ctx;
}
this. lp6_init = function() { return this._ctx_type_7_init();}
this.lp6 = function(_ctx,x,fc){
   if(this.change(_ctx._inst0,_ctx.fc)){
      _ctx.fc = this.clip(_ctx.fc,0.,this.samplerate());
      _ctx.k = this.tan(((this.pi() * _ctx.fc) / this.samplerate()));
   }
   var b0 = (_ctx.k / (_ctx.k + 1.));
   var b1 = (_ctx.k / (_ctx.k + 1.));
   var a1 = ((_ctx.k - 1.) / (_ctx.k + 1.));
   return this.biquad(_ctx._inst1,x,b0,b1,0.,a1,0.);
}


this._ctx_type_8_init = function(){
   var _ctx = {};
   _ctx.b2 = 0.;
   _ctx.b1 = 0.;
   _ctx.b0 = 0.;
   _ctx.a2 = 0.;
   _ctx.a1 = 0.;
   _ctx._inst2 = this._ctx_type_6_init();
   _ctx._inst1 = this._ctx_type_5_init();
   _ctx._inst0 = this._ctx_type_5_init();
   return _ctx;
}
this. lp12_init = function() { return this._ctx_type_8_init();}
this.lp12 = function(_ctx,x,fc,q){
   if((this.change(_ctx._inst0,fc) || this.change(_ctx._inst1,q))){
      var qc = this.max(this.sqrt(2.),(q + this.sqrt(2.)));
      fc = this.clip(fc,0.,this.samplerate());
      var k = this.tan(((this.pi() * fc) / this.samplerate()));
      var den = ((((k * k) * qc) + k) + qc);
      _ctx.b0 = (((k * k) * qc) / den);
      _ctx.b1 = (2. * _ctx.b0);
      _ctx.b2 = _ctx.b0;
      _ctx.a1 = (((2. * qc) * ((k * k) - 1.)) / den);
      _ctx.a2 = (((((k * k) * qc) - k) + qc) / den);
   }
   return this.biquad(_ctx._inst2,x,_ctx.b0,_ctx.b1,_ctx.b2,_ctx.a1,_ctx.a2);
}


this._ctx_type_9_init = function(){
   var _ctx = {};
   _ctx._inst0 = this._ctx_type_6_init();
   return _ctx;
}
this. hp6_init = function() { return this._ctx_type_9_init();}
this.hp6 = function(_ctx,x,fc){
   fc = this.clip(fc,0.,this.samplerate());
   var k = this.tan(((this.pi() * fc) / this.samplerate()));
   var b0 = (1. / (k + 1.));
   var b1 = ((- 1.) / (k + 1.));
   var a1 = ((k - 1.) / (k + 1.));
   return this.biquad(_ctx._inst0,x,b0,b1,0.,a1,0.);
}


this._ctx_type_10_init = function(){
   var _ctx = {};
   _ctx._inst0 = this._ctx_type_6_init();
   return _ctx;
}
this. allp6_init = function() { return this._ctx_type_10_init();}
this.allp6 = function(_ctx,x,fc){
   fc = this.clip(fc,0.,this.samplerate());
   var k = this.tan(((this.pi() * fc) / this.samplerate()));
   var b0 = ((k - 1.) / (k + 1.));
   var b1 = 1.;
   var a1 = ((k - 1.) / (k + 1.));
   return this.biquad(_ctx._inst0,x,b0,b1,0.,a1,0.);
}


this._ctx_type_11_init = function(){
   var _ctx = {};
   _ctx.b2 = 0.;
   _ctx.b1 = 0.;
   _ctx.b0 = 0.;
   _ctx.a2 = 0.;
   _ctx.a1 = 0.;
   _ctx._inst2 = this._ctx_type_6_init();
   _ctx._inst1 = this._ctx_type_5_init();
   _ctx._inst0 = this._ctx_type_5_init();
   return _ctx;
}
this. hp12_init = function() { return this._ctx_type_11_init();}
this.hp12 = function(_ctx,x,fc,q){
   if((this.change(_ctx._inst0,fc) || this.change(_ctx._inst1,q))){
      var qc = this.max(this.sqrt(2.),(q + this.sqrt(2.)));
      fc = this.clip(fc,0.,this.samplerate());
      var k = this.tan(((this.pi() * fc) / this.samplerate()));
      var den = ((((k * k) * qc) + k) + qc);
      _ctx.b0 = (qc / den);
      _ctx.b1 = (((- 2.) * qc) / den);
      _ctx.b2 = (qc / den);
      _ctx.a1 = (((2. * qc) * ((k * k) - 1.)) / den);
      _ctx.a2 = (((((k * k) * qc) - k) + qc) / den);
   }
   return this.biquad(_ctx._inst2,x,_ctx.b0,_ctx.b1,_ctx.b2,_ctx.a1,_ctx.a2);
}


this._ctx_type_12_init = function(){
   var _ctx = {};
   _ctx.b2 = 0.;
   _ctx.b1 = 0.;
   _ctx.b0 = 0.;
   _ctx.a2 = 0.;
   _ctx.a1 = 0.;
   _ctx._inst2 = this._ctx_type_6_init();
   _ctx._inst1 = this._ctx_type_5_init();
   _ctx._inst0 = this._ctx_type_5_init();
   return _ctx;
}
this. bp12_init = function() { return this._ctx_type_12_init();}
this.bp12 = function(_ctx,x,fc,q){
   if((this.change(_ctx._inst0,fc) || this.change(_ctx._inst1,q))){
      var qc = this.max(this.sqrt(2.),(q + this.sqrt(2.)));
      fc = this.clip(fc,0.,this.samplerate());
      var k = this.tan(((this.pi() * fc) / this.samplerate()));
      var den = ((((k * k) * qc) + k) + qc);
      _ctx.b0 = (k / den);
      _ctx.b1 = 0.;
      _ctx.b2 = ((- k) / den);
      _ctx.a1 = (((2. * qc) * ((k * k) - 1.)) / den);
      _ctx.a2 = (((((k * k) * qc) - k) + qc) / den);
   }
   return this.biquad(_ctx._inst2,x,_ctx.b0,_ctx.b1,_ctx.b2,_ctx.a1,_ctx.a2);
}


this._ctx_type_13_init = function(){
   var _ctx = {};
   _ctx.b2 = 0.;
   _ctx.b1 = 0.;
   _ctx.b0 = 0.;
   _ctx.a2 = 0.;
   _ctx.a1 = 0.;
   _ctx._inst2 = this._ctx_type_6_init();
   _ctx._inst1 = this._ctx_type_5_init();
   _ctx._inst0 = this._ctx_type_5_init();
   return _ctx;
}
this. notch12_init = function() { return this._ctx_type_13_init();}
this.notch12 = function(_ctx,x,fc,q){
   if((this.change(_ctx._inst0,fc) || this.change(_ctx._inst1,q))){
      var qc = this.max(this.sqrt(2.),(q + this.sqrt(2.)));
      fc = this.clip(fc,0.,this.samplerate());
      var k = this.tan(((this.pi() * fc) / this.samplerate()));
      var den = ((((k * k) * qc) + k) + qc);
      _ctx.b0 = ((qc * (1. + (k * k))) / den);
      _ctx.b1 = (((2. * qc) * ((k * k) - 1.)) / den);
      _ctx.b2 = _ctx.b0;
      _ctx.a1 = (((2. * qc) * ((k * k) - 1.)) / den);
      _ctx.a2 = (((((k * k) * qc) - k) + qc) / den);
   }
   return this.biquad(_ctx._inst2,x,_ctx.b0,_ctx.b1,_ctx.b2,_ctx.a1,_ctx.a2);
}


this._ctx_type_14_init = function(){
   var _ctx = {};
   _ctx.b2 = 0.;
   _ctx.b1 = 0.;
   _ctx.b0 = 0.;
   _ctx.a2 = 0.;
   _ctx.a1 = 0.;
   _ctx._inst2 = this._ctx_type_6_init();
   _ctx._inst1 = this._ctx_type_5_init();
   _ctx._inst0 = this._ctx_type_5_init();
   return _ctx;
}
this. allp12_init = function() { return this._ctx_type_14_init();}
this.allp12 = function(_ctx,x,fc,q){
   if((this.change(_ctx._inst0,fc) || this.change(_ctx._inst1,q))){
      var qc = this.max(this.sqrt(2.),(q + this.sqrt(2.)));
      fc = this.clip(fc,0.,this.samplerate());
      var k = this.tan(((this.pi() * fc) / this.samplerate()));
      var den = ((((k * k) * qc) + k) + qc);
      _ctx.b0 = (((((k * k) * qc) - k) + qc) / den);
      _ctx.b1 = (((2. * qc) * ((k * k) - 1.)) / den);
      _ctx.b2 = 1.;
      _ctx.a1 = (((2. * qc) * ((k * k) - 1.)) / den);
      _ctx.a2 = (((((k * k) * qc) - k) + qc) / den);
   }
   return this.biquad(_ctx._inst2,x,_ctx.b0,_ctx.b1,_ctx.b2,_ctx.a1,_ctx.a2);
}
this.allp12(_ctx._inst3,0.,100.,0.5);

    if(this.process_init)  this.context =  this.process_init(); else this.context = {};
    if(this.default_)      this.default_(this.context);
    this.liveNoteOn        = function(note,velocity) { if(this.noteOn)        this.noteOn(this.context,note,velocity); };
    this.liveNoteOff       = function(note,velocity) { if(this.noteOff)       this.noteOff(this.context,note,velocity); };
    this.liveControlChange = function(note,velocity) { if(this.controlChange) this.controlChange(this.context,note,velocity); };
    this.liveProcess       = function(input)         { if(this.process)       return this.process(this.context,input); else return 0; };
    this.liveDefault       = function()              { if(this.default_)      return this.default_(this.context); };
}
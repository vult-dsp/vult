
local this = {}
local ffi = require("ffi")
function this.ternary(cond,then_,else_) if cond then return then_ else return else_ end end
function this.eps()             return 1e-18; end
function this.random()          return math.random(); end
function this.irandom()         return math.floor(math.random() * 4294967296); end
function this.clip(x,low,high)  return (this.ternary(x<low,low,this.ternary(x>high,high,x))); end
function this.real(x)           return x; end
function this.int(x)            local int_part,_ = math.modf(x) return int_part; end
function this.sin(x)            return math.sin(x); end
function this.cos(x)            return math.cos(x); end
function this.abs(x)            return math.abs(x); end
function this.exp(x)            return math.exp(x); end
function this.floor(x)          return math.floor(x); end
function this.tan(x)            return math.tan(x); end
function this.tanh(x)           return math.tanh(x); end
function this.sqrt(x)           return x; end
function this.set(a,i,v)        a[i]=v; end
function this.get(a,i)          return a[i]; end
function this.makeArray(size,v) local a = ffi.new("double[?]",size); for i=0,size-1 do a[i]=v end return a; end
function this.wrap_array(a)     return a; end
function this.Gates_velToCV(vel)
   return (this.real(vel) * 0.00787401574803);

end

function this.Gates__ctx_type_1_init()
   local _ctx = {};
   _ctx.vel = this.makeArray(8,0);
   return _ctx;

end

function this.Gates_gates_noteOn_init()
   return this.Gates__ctx_type_1_init();
end

function this.Gates_gates_noteOn(_ctx,note,velocity)
   if ((note >= 36) and (note <= 43)) then
      local n = 0;
      n = (note + -36);
      this.set(_ctx.vel,n,velocity);

   end

end

function this.Gates_gates_noteOff_init()
   return this.Gates__ctx_type_1_init();
end

function this.Gates_gates_noteOff(_ctx,note)
   if ((note >= 36) and (note <= 43)) then
      local n = 0;
      n = (note + -36);
      this.set(_ctx.vel,n,0);

   end

end

function this.Gates_gates_state_init()
   return this.Gates__ctx_type_1_init();
end

function this.Gates_gates_state(_ctx)
   local vel1 = this.Gates_velToCV(this.get(_ctx.vel,0));
   local vel2 = this.Gates_velToCV(this.get(_ctx.vel,1));
   local vel3 = this.Gates_velToCV(this.get(_ctx.vel,2));
   local vel4 = this.Gates_velToCV(this.get(_ctx.vel,3));
   local vel5 = this.Gates_velToCV(this.get(_ctx.vel,4));
   local vel6 = this.Gates_velToCV(this.get(_ctx.vel,5));
   local vel7 = this.Gates_velToCV(this.get(_ctx.vel,6));
   local vel8 = this.Gates_velToCV(this.get(_ctx.vel,7));
   local _tuple_15 = { field_0 : vel1, field_1 : vel2, field_2 : vel3, field_3 : vel4, field_4 : vel5, field_5 : vel6, field_6 : vel7, field_7 : vel8 };
   return _tuple_15;

end

function this.Gates__ctx_type_2_init()
   local _ctx = {};
   _ctx.gates = this.Gates__ctx_type_1_init();
   _ctx.channel_in = 0;
   return _ctx;

end

function this.Gates_process_init()
   return this.Gates__ctx_type_2_init();
end

function this.Gates_process(_ctx,c)
   _ctx.channel_in = c;
   local _call_17 = this.Gates_gates_state(_ctx.gates);
   return _call_17;

end

function this.Gates_noteOn_init()
   return this.Gates__ctx_type_2_init();
end

function this.Gates_noteOn(_ctx,note,velocity,channel)
   if (_ctx.channel_in == channel) then
      this.Gates_gates_noteOn(_ctx.gates,note,velocity);

   end

end

function this.Gates_noteOff_init()
   return this.Gates__ctx_type_2_init();
end

function this.Gates_noteOff(_ctx,note,channel)
   if (_ctx.channel_in == channel) then
      this.Gates_gates_noteOff(_ctx.gates,note);

   end

end

function this.Gates_controlChange_init()
   return this.Gates__ctx_type_2_init();
end

function this.Gates_controlChange(_ctx,control,value,channel)

end

function this.Gates_default_init()
   return this.Gates__ctx_type_2_init();
end

function this.Gates_default(_ctx)

end


function this.process(ctx,in0) return this.Gates_process(ctx,in0) end
function this.noteOn(ctx,in0,in1,in2) return this.Gates_noteOn(ctx,in0,in1,in2) end
function this.noteOff(ctx,in0,in1) return this.Gates_noteOff(ctx,in0,in1) end
function this.controlChange(ctx,in0,in1,in2) return this.Gates_controlChange(ctx,in0,in1,in2) end
function this.init() return this.Gates_process_init() end
function this.default(ctx) return this.Gates_default(ctx) end
this.config = { inputs = 2, outputs = 8, noteon_inputs = 4, noteoff_inputs = 3, controlchange_inputs = 4, is_active = true }
return this


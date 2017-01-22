(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

open CLike
open GenerateParams

let dot = Pla.map_sep (Pla.string ".") Pla.string

module Templates = struct

   type t =
      | None
      | Browser

   let get template =
      match template with
      | "none"   -> None
      | "default" -> Browser
      | "browser" -> Browser
      | t -> failwith (Printf.sprintf "The template '%s' is not available for this generator" t)

   let none code = code

   let browser module_name code =
      {pla|
function vultProcess(){
    this.eps  = function()           { return 1e-18 };
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
    this.set  = function(a,i,v)      { a[i]=v; };
    this.get  = function(a,i)        { return a[i]; };
    this.int_to_float = function(i){ return i; };
    this.float_to_int = function(i){ return Math.floor(i); };
    this.makeArray = function(size,v){ var a = new Array(size); for(var i=0;i<size;i++) a[i]=v; return a; };
    this.<#module_name#s>_process_init = null;
    this.<#module_name#s>_default = null;
<#code#>
    if(this.<#module_name#s>_process_init)  this.context =  this.<#module_name#s>_process_init(); else this.context = {};
    if(this.<#module_name#s>_default)      this.<#module_name#s>_default(this.context);
    this.liveNoteOn        = function(note,velocity,channel) { if(this.<#module_name#s>_noteOn)        this.<#module_name#s>_noteOn(this.context,note,velocity,channel); };
    this.liveNoteOff       = function(note,velocity,channel) { if(this.<#module_name#s>_noteOff)       this.<#module_name#s>_noteOff(this.context,note,velocity,channel); };
    this.liveControlChange = function(note,velocity,channel) { if(this.<#module_name#s>_controlChange) this.<#module_name#s>_controlChange(this.context,note,velocity,channel); };
    this.liveProcess       = function(input)         { if(this.<#module_name#s>_process)       return this.<#module_name#s>_process(this.context,input); else return 0; };
    this.liveDefault       = function()              { if(this.<#module_name#s>_default)      return this.<#module_name#s>_default(this.context); };
}
|pla}

   let apply (module_name:string) (template:string) (code:Pla.t) : Pla.t =
      match template with
      | "browser" -> browser module_name code
      | _ -> none code

end

let isSpecial (params:params) (name:string) : bool =
   match name with
   | _ when name = params.module_name^"_process" -> true
   | _ when name = params.module_name^"_noteOn" -> true
   | _ when name = params.module_name^"_noteOff" -> true
   | _ when name = params.module_name^"_controlChange" -> true
   | _ when name = params.module_name^"_default" -> true
   | _ -> false

let fixContext (is_special:bool) args =
   if is_special then
      match args with
      | [] -> [(Ref(CTSimple("any")),"_ctx")]
      | (_,"_ctx")::_ -> args
      | t -> (Ref(CTSimple("any")),"_ctx")::t
   else args

let rec printExp (params:params) (e:cexp) : Pla.t =
   match e with
   | CEEmpty -> Pla.unit
   | CEInt(n) ->
      {pla|(<#n#i>|0)|pla}
   | CEFloat(_,n) ->
      if n < 0.0 then {pla|(<#n#f>)|pla} else Pla.float n
   | CEBool(v) ->
      Pla.string (if v then "true" else "false")
   | CEString(s) ->
      Pla.wrap (Pla.string "\"") (Pla.string "\"") (Pla.string s)
   | CEArray(elems) ->
      let elems_t = Pla.map_sep Pla.comma (printExp params) elems in
      {pla|[<#elems_t#>]|pla}
   | CECall(name,args) ->
      let args_t = Pla.map_sep Pla.comma (printExp params) args in
      {pla|this.<#name#s>(<#args_t#>)|pla}
   | CEUnOp(op,e) ->
      let e_t = printExp params e in
      {pla|(<#op#s> <#e_t#>)|pla}
   | CEOp(op,elems) ->
      let op_t = {pla| <#op#s> |pla} in
      let elems_t = Pla.map_sep op_t (printExp params) elems in
      {pla|(<#elems_t#>)|pla}
   | CEVar(name) ->
      dot name
   | CEIf(cond,then_,else_) ->
      let cond_t = printExp params cond in
      let then_t = printExp params then_ in
      let else_t = printExp params else_ in
      {pla|(<#cond_t#>?<#then_t#>:<#else_t#>)|pla}
   | CENewObj -> Pla.string "{}"
   | CETuple(elems) ->
      let elems_t = Pla.map_sep Pla.commaspace (printJsField params) elems in
      {pla|{ <#elems_t#> }|pla}

and printJsField (params:params) (name,value) : Pla.t =
   let value_t = printExp params value in
   {pla|<#name#s> : <#value_t#>|pla}


let printLhsExpTuple (var:string list) (is_var:bool) (i:int) (e:clhsexp) : Pla.t =
   let var = dot var in
   match e with
   | CLId(_,name) ->
      let name = dot name in
      if is_var then
         {pla|var <#name#> = <#var#>.field_<#i#i>; |pla}
      else
         {pla|<#name#> = <#var#>.field_<#i#i>; |pla}

   | CLWild -> Pla.unit

   | _ -> failwith "printLhsExp: All other cases should be already covered"

let wrapInt (params:params) (is_int:bool) (e:cexp) : Pla.t =
   let e_t = printExp params e in
   if is_int then
      {pla|(<#e_t#>|0)|pla}
   else e_t

let getInitValue (descr:type_descr) : string =
   match descr with
   | CTSimple("int") -> "(0|0)"
   | CTSimple("float") -> "0.0"
   | CTSimple("real") -> "0.0"
   | CTSimple("bool") -> "false"
   | CTSimple("unit") -> "0"
   | _ -> "{}"

let rec printStmt (params:params) (stmt:cstmt) : Pla.t option =
   match stmt with
   | CSVar(CLWild) -> None

   | CSVar(CLId(tdescr,name)) ->
      let init = getInitValue tdescr in
      let name = dot name in
      Some({pla|var <#name#> = <#init#s>;|pla})

   | CSVar(CLTuple(_)) -> failwith "printStmt: invalid tuple assign"

   | CSConst(CLId(tdecr,name),value) ->
      let is_int = tdecr = CTSimple("int") in
      let value_t = wrapInt params is_int value in
      let name = dot name in
      Some({pla|var <#name#> = <#value_t#>;|pla})

   | CSConst(CLWild,value) ->
      let value_t = printExp params value in
      Some({pla|<#value_t#>;|pla})

   | CSConst(CLTuple(elems),CEVar(name)) ->
      List.mapi (printLhsExpTuple name true) elems
      |> Pla.join
      |> fun a -> Some(a)

   | CSConst _ -> failwith "printStmt: invalid constant assign"

   | CSBind(CLWild,value) ->
      Some(Pla.(printExp params value ++ semi))

   | CSBind(CLTuple(elems),CEVar(name)) ->
      List.mapi (printLhsExpTuple name false) elems
      |> Pla.join
      |> fun a -> Some(a)

   | CSBind(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"

   | CSBind(CLId(tdecr,name),value) ->
      let is_int = tdecr = CTSimple("int") in
      let value_t = wrapInt params is_int value in
      let name = dot name in
      Some({pla|<#name#> = <#value_t#>;|pla})

   | CSFunction(_,name,args,(CSBlock(_) as body)) ->
      (* if the function has any of the special names add the ctx argument *)
      let args = fixContext (isSpecial params name) args in
      let args_t = Pla.map_sep Pla.comma (fun (_,a) -> Pla.string a) args in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some({pla|this.<#name#s> = function(<#args_t#>)<#body_t#>|pla})

   | CSFunction(_,name,args,body) ->
      let args_t = Pla.map_sep Pla.comma (fun (_,a) -> Pla.string a) args in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some({pla|this.<#name#s> = function(<#args_t#>) { <#body_t#>}|pla})

   | CSReturn(e1) ->
      let e_t = printExp params e1 in
      Some({pla|return <#e_t#>;|pla})

   | CSWhile(cond,body) ->
      let cond_t = printExp params cond in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some({pla|while(<#cond_t#>)<#body_t#>|pla})

   | CSBlock(elems) ->
      let elems_t = printStmtList params elems in
      Some({pla|{<#elems_t#+>}|pla})

   | CSIf(cond,then_,None) ->
      let cond_t = printExp params cond in
      let then_t = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
      Some({pla|if(<#cond_t#>)<#then_t#>|pla})

   | CSIf(cond,then_,Some(else_)) ->
      let cond_t = printExp params cond in
      let then_t = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
      let else_t = CCOpt.get_or ~default:Pla.semi (printStmt params else_) in
      Some({pla|if(<#cond_t#>)<#then_t#><#>else<#><#else_t#>|pla})

   | CSEmpty -> None

   | CSType _ -> None

   | CSAlias _ -> None

   | CSExtFunc _ -> None

and printStmtList (params:params) (stmts:cstmt list) : Pla.t =
   let tstmts = CCList.filter_map (printStmt params) stmts in
   Pla.map_sep_all Pla.newline (fun a -> a) tstmts

let printJsCode (params:params) (stmts:cstmt list) : Pla.t =
   let code = printStmtList params stmts in
   Templates.apply params.module_name params.template code

(** Generates the .c and .h file contents for the given parsed files *)
let print (params:params) (stmts:CLike.cstmt list) : (Pla.t * string) list =
   let js_text = printJsCode params stmts in
   [js_text, "js"]


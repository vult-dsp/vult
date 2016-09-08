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

module Templates = struct

   type t =
      | None
      | Default

   let get template =
      match template with
      | "none"   -> None
      | "default" -> Default
      | t -> failwith (Printf.sprintf "The template '%s' is not available for this generator" t)

   let none code = code

   let default _ code =
{pla|local ffi = require("ffi")
function ternary(cond,then_,else_) if cond then return then_ else return else_ end end
function clip(x,low,high)  return (ternary(x<low,low,ternary(x>high,high,x))); end
function real(x)           return x; end
function int(x)            return x; end
function sin(x)            return math.sin(x); end
function cos(x)            return math.cos(x); end
function abs(x)            return math.abs(x); end
function exp(x)            return math.exp(x); end
function floor(x)          return math.floor(x); end
function tan(x)            return math.tan(x); end
function tanh(x)           return math.tanh(x); end
function sqrt(x)           return x; end
function set(a,i,v)        a[i]=v; end
function get(a,i)          return a[i]; end
function int_to_float(i)   return i; end
function float_to_int(i)   return math.floor(i); end
function makeArray(size,v) local a = ffi.new("double[?]",size); for i=0,size-1 do a[i]=v end return a; end
<#code#>
|pla}

   let apply (module_name:string) (template:string) (code:Pla.t) : Pla.t =
      match template with
      | "default" -> default module_name code
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
      {pla|<#n#i>|pla}
   | CEFloat(_,n) ->
      if n < 0.0 then {pla|(<#n#f>)|pla} else Pla.float n
   | CEBool(v) ->
      Pla.string (if v then "true" else "false")
   | CEString(s) ->
      Pla.wrap (Pla.string "\"") (Pla.string "\"") (Pla.string s)
   | CEArray(elems) ->
      let elems_t = Pla.map_sep Pla.comma (printExp params) elems in
      {pla|{<#elems_t#>}|pla}
   | CECall(name,args) ->
      let args_t = Pla.map_sep Pla.comma (printExp params) args in
      {pla|<#name#s>(<#args_t#>)|pla}
   | CEUnOp(op,e) ->
      let e_t = printExp params e in
      {pla|(<#op#s> <#e_t#>)|pla}
   | CEOp(op,elems) ->
      let op_t = {pla| <#op#s> |pla} in
      let elems_t = Pla.map_sep op_t (printExp params) elems in
      {pla|(<#elems_t#>)|pla}
   | CEVar(name) ->
      Pla.string name
   | CEIf(cond,then_,else_) ->
      let cond_t = printExp params cond in
      let then_t = printExp params then_ in
      let else_t = printExp params else_ in
      {pla|(ternary(<#cond_t#>, <#then_t#>, <#else_t#>)|pla}
   | CENewObj -> Pla.string "{}"
   | CETuple(elems) ->
      let elems_t = Pla.map_sep Pla.commaspace (printJsField params) elems in
      {pla|{ <#elems_t#> }|pla}

and printJsField (params:params) (name,value) : Pla.t =
   let value_t = printExp params value in
   {pla|<#name#s> : <#value_t#>|pla}


let printLhsExpTuple (var:string) (is_var:bool) (i:int) (e:clhsexp) : Pla.t =
   let i1 = i + 1 in
   match e with
   | CLId(_,name) ->
      if is_var then
         {pla|local <#name#s> = <#var#s>[<#i1#i>]; |pla}
      else
         {pla|<#name#s> = <#var#s>[<#i1#i>]; |pla}

   | CLWild -> Pla.unit

   | _ -> failwith "printLhsExp: All other cases should be already covered"

let getInitValue (descr:type_descr) : string =
   match descr with
   | CTSimple("int") -> "(0|0)"
   | CTSimple("real") -> "0.0"
   | CTSimple("float") -> "0.0"
   | CTSimple("bool") -> "false"
   | CTSimple("unit") -> "0"
   | _ -> "{}"

let rec printStmt (params:params) (stmt:cstmt) : Pla.t option =
   match stmt with
   | CSVarDecl(CLWild,Some(value)) ->
      let value_t = printExp params value in
      Some({pla|<#value_t#>;|pla})

   | CSVarDecl(CLWild,None) -> None

   | CSVarDecl(CLId(_,name),Some(value)) ->
      let value_t = printExp params value in
      Some({pla|local <#name#s> = <#value_t#>;|pla})

   | CSVarDecl(CLId(typ,name),None) ->
      let init = getInitValue typ in
      Some({pla|local <#name#s> = <#init#s>;|pla})

   | CSVarDecl(CLTuple(elems),Some(CEVar(name))) ->
      List.mapi (printLhsExpTuple name true) elems
      |> Pla.join
      |> fun a -> Some(a)

   | CSVarDecl(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"

   | CSBind(CLWild,value) ->
      Some(Pla.(printExp params value ++ semi))

   | CSBind(CLTuple(elems),CEVar(name)) ->
      List.mapi (printLhsExpTuple name false) elems
      |> Pla.join
      |> fun a -> Some(a)

   | CSBind(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"

   | CSBind(CLId(_,name),value) ->
      let value_t = printExp params value in
      Some({pla|<#name#s> = <#value_t#>;|pla})

   | CSFunction(_,name,args,(CSBlock(_) as body)) ->
      (* if the function has any of the special names add the ctx argument *)
      let args = fixContext (isSpecial params name) args in
      let args_t = Pla.map_sep Pla.comma (fun (_,a) -> Pla.string a) args in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some({pla|function <#name#s>(<#args_t#>)<#body_t#+><#>end<#>|pla})

   | CSFunction(_,name,args,body) ->
      let args_t = Pla.map_sep Pla.comma (fun (_,a) -> Pla.string a) args in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some({pla|function <#name#s>(<#args_t#>)<#body_t#+><#>end<#>|pla})

   | CSReturn(e1) ->
      let e_t = printExp params e1 in
      Some({pla|return <#e_t#>;|pla})

   | CSWhile(cond,body) ->
      let cond_t = printExp params cond in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some({pla|while <#cond_t#> do<#body_t#+>end|pla})

   | CSBlock(elems) ->
      let elems_t = printStmtList params elems in
      Some({pla|<#elems_t#>|pla})

   | CSIf(cond,then_,None) ->
      let cond_t = printExp params cond in
      let then_t = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
      Some({pla|if <#cond_t#> then<#then_t#+><#>end|pla})

   | CSIf(cond,then_,Some(else_)) ->
      let cond_t = printExp params cond in
      let then_t = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
      let else_t = CCOpt.get_or ~default:Pla.semi (printStmt params else_) in
      Some({pla|if <#cond_t#> then<#then_t#+><#>else<#><#else_t#+><#>end|pla})

   | CSEmpty -> None

   | CSType _ -> None

   | CSAlias _ -> None

   | CSExtFunc _ -> None

and printStmtList (params:params) (stmts:cstmt list) : Pla.t =
   let tstmts = CCList.filter_map (printStmt params) stmts in
   Pla.map_sep_all Pla.newline (fun a -> a) tstmts

let printLuaCode (params:params) (stmts:cstmt list) : Pla.t =
   let code = printStmtList params stmts in
   Templates.apply params.module_name params.template code

(** Generates the .c and .h file contents for the given parsed files *)
let print (params:params) (stmts:CLike.cstmt list) : (Pla.t * string) list =
   let js_text = printLuaCode params stmts in
   [js_text, "lua"]


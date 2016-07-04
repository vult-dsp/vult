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

open TypesVult
open Common

type jsexp =
   | JEInteger of int
   | JEFloat  of float
   | JEBool   of bool
   | JEString of string
   | JEArray  of jsexp list
   | JECall   of string * jsexp list
   | JEUnOp   of string * jsexp
   | JEOp     of string * jsexp list
   | JEVar    of string
   | JEIf     of jsexp * jsexp * jsexp
   | JETuple  of (string * jsexp) list
   | JENewObj

type jslhsexp =
   | JLWild
   | JLId    of string
   | JLTuple of jslhsexp list

type jsstmt =
   | JSVarDecl  of jslhsexp * jsexp * bool
   | JSBind     of jslhsexp * jsexp * bool
   | JSFunction of string * string list * jsstmt
   | JSReturn   of jsexp
   | JSWhile    of jsexp * jsstmt
   | JSBlock    of jsstmt list
   | JSIf       of jsexp * jsstmt * jsstmt option
   | JSEmpty

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

   let browser code =
      "function vultProcess(){
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
    this.makeArray = function(size,v){ var a = new Array(size); for(var i=0;i<size;i++) a[i]=v; return a; };
    this.Live_process_init = null;
    this.Live_default_ = null;
"^code^"
    if(this.Live_process_init)  this.context =  this.Live_process_init(); else this.context = {};
    if(this.Live_default_)      this.Live_default_(this.context);
    this.liveNoteOn        = function(note,velocity) { if(this.Live_noteOn)        this.Live_noteOn(this.context,note,velocity); };
    this.liveNoteOff       = function(note,velocity) { if(this.Live_noteOff)       this.Live_noteOff(this.context,note,velocity); };
    this.liveControlChange = function(note,velocity) { if(this.Live_controlChange) this.Live_controlChange(this.context,note,velocity); };
    this.liveProcess       = function(input)         { if(this.Live_process)       return this.Live_process(this.context,input); else return 0; };
    this.liveDefault       = function()              { if(this.Live_default_)      return this.Live_default_(this.context); };
}
"

   let apply template code =
      match template with
      | None -> none code
      | Browser -> browser code

end

type parameters =
   {
      template : Templates.t;
   }

let fixOp op =
   match op with
   | "<>" -> "!="
   | _ -> op

let fixKeyword key =
   match key with
   | "default" -> "default_"
   | _ -> key

let isSpecial name =
   match name with
   | [_;"process"] -> true
   | [_;"noteOn"] -> true
   | [_;"noteOff"] -> true
   | [_;"controlChange"] -> true
   | [_;"default"] -> true
   | _ -> false

let fixContext is_special args =
   if is_special then
      match args with
      | [] -> ["_ctx"]
      | "_ctx"::_ -> args
      | t -> "_ctx"::t
   else args

let isIntType (opt_typ:VType.t option) : bool =
   match opt_typ with
   | None -> false
   | Some(typ) ->
      match !(VType.unlink typ) with
      | VType.TId(name,_) when name=["int"] -> true
      | _ -> false

let rec join (sep:string) (id:string list) : string =
   match id with
   | [] -> ""
   | [ name ] -> fixKeyword name
   | h :: t -> (fixKeyword h) ^ sep ^ (join sep t)

let rec convertId (id:id) : string =
   join "." id

let rec convertFunctionId (id:id) : string =
   join "_" id

let rec convertExp (e:exp) : jsexp =
   match e with
   | PUnit(_)        -> JEInteger(0)
   | PBool(v,_)      -> JEBool(v)
   | PInt(n,_)       -> JEInteger(n)
   | PReal(v,_)      -> JEFloat(v)
   | PId(id,_)       -> JEVar(convertId id)
   | PArray(elems,_) -> JEArray(convertExpList elems)
   | PUnOp("|-|",e1,_)-> JEUnOp("-", convertExp e1)
   | PUnOp(op,e1,_)  -> JEUnOp(op, convertExp e1)
   | POp(op,elems,_) -> JEOp(fixOp op, convertExpList elems)
   | PCall(_,name,args,_)    -> JECall(convertFunctionId name, convertExpList args)
   | PIf(cond,then_,else_,_) -> JEIf(convertExp cond, convertExp then_, convertExp else_)
   | PGroup(e1,_)    -> convertExp e1
   | PTuple([e1],_)  -> convertExp e1
   | PTuple(elems,_)   ->
      let jselems = List.mapi (fun i a -> "field_"^(string_of_int i), convertExp a ) elems in
      JETuple(jselems)
   | PSeq _          -> failwith "VultJs.convertExp: Sequences are not yet supported for js"
   | PEmpty          -> failwith "VultJs.convertExp: Empty expressions are not allowed"

and convertExpList (e:exp list) : jsexp list =
   List.map convertExp e

let rec convertLhsExp (e:lhs_exp) : jslhsexp =
   match e with
   | LId(id,_,_)     -> JLId(convertId id)
   | LTyped(e1,_,_)  -> convertLhsExp e1
   | LTuple(elems,_) -> JLTuple(List.map convertLhsExp elems)
   | LWild _         -> JLWild
   | LGroup(e,_)    -> convertLhsExp e

let convertTypedId (e:typed_id) : string =
   match e with
   | SimpleId(id,_)  -> convertId id
   | TypedId(id,_,_) -> convertId id

let rec convertStmt (s:stmt) : jsstmt =
   match s with
   | StmtVal(lhs,None,_)    -> JSVarDecl(convertLhsExp lhs,JENewObj,false)
   | StmtVal(lhs,Some(rhs),_) ->
      let is_int = isIntType ((GetAttr.fromExp rhs).typ) in
      JSVarDecl(convertLhsExp lhs,convertExp rhs,is_int)
   | StmtMem _              -> JSEmpty
   | StmtWhile(cond,stmt,_) -> JSWhile(convertExp cond, convertStmt stmt)
   | StmtReturn(e1,_)       -> JSReturn(convertExp e1)
   | StmtIf(cond,then_,None,_) -> JSIf(convertExp cond, convertStmt then_, None)
   | StmtIf(cond,then_,Some(else_),_) -> JSIf(convertExp cond, convertStmt then_, Some(convertStmt else_))
   | StmtFun(name,args,body,_,_) ->
      let arg_names = List.map convertTypedId args in
      let args' = fixContext (isSpecial name) arg_names in
      JSFunction(convertFunctionId name,args',convertStmt body)
   | StmtBind(lhs,rhs,_)    ->
      let is_int = isIntType ((GetAttr.fromExp rhs).typ) in
      JSBind(convertLhsExp lhs, convertExp rhs,is_int)
   | StmtBlock(_,stmts,_)   -> JSBlock(convertStmtList stmts)
   | StmtType _             -> JSEmpty
   | StmtAliasType _        -> JSEmpty
   | StmtEmpty              -> JSEmpty
   | StmtExternal _         -> JSEmpty

and convertStmtList (stmts:stmt list) : jsstmt list =
   List.map convertStmt stmts

let rec printExp (e:jsexp) : Pla.t =
   match e with
   | JEInteger(n) ->
      {pla|(<#n#i>|0)|pla}
   | JEFloat(n) ->
      if n < 0.0 then {pla|(<#n#f>)|pla} else Pla.float n
   | JEBool(v) ->
      Pla.string (if v then "true" else "false")
   | JEString(s) ->
      Pla.wrap (Pla.string "\"") (Pla.string "\"") (Pla.string s)
   | JEArray(elems) ->
      let elems_t = Pla.map_sep Pla.comma printExp elems in
      {pla|[<#elems_t#>]|pla}
   | JECall(name,args) ->
      let args_t = Pla.map_sep Pla.comma printExp args in
      {pla|this.<#name#s>(<#args_t#>)|pla}
   | JEUnOp(op,e) ->
      let e_t = printExp e in
      {pla|(<#op#s> <#e_t#>)|pla}
   | JEOp(op,elems) ->
      let op_t = {pla| <#op#s> |pla} in
      let elems_t = Pla.map_sep op_t printExp elems in
      {pla|(<#elems_t#>)|pla}
   | JEVar(name) ->
      Pla.string name
   | JEIf(cond,then_,else_) ->
      let cond_t = printExp cond in
      let then_t = printExp then_ in
      let else_t = printExp else_ in
      {pla|(<#cond_t#>?<#then_t#>:<#else_t#>)|pla}
   | JENewObj -> Pla.string "{}"
   | JETuple(elems) ->
      let elems_t = Pla.map_sep Pla.commaspace printJsField elems in
      {pla|{ <#elems_t#> }|pla}

and printJsField (name,value) : Pla.t =
   let value_t = printExp value in
   {pla|<#name#s> : <#value_t#>|pla}


let printLhsExpTuple (var:string) (is_var:bool) (i:int) (e:jslhsexp) : Pla.t =
   match e with
   | JLId(name) ->
      if is_var then
         {pla|var <#name#s> = <#var#s>.field_<#i#i>; |pla}
      else
         {pla|<#name#s> = <#var#s>.field_<#i#i>; |pla}

   | JLWild -> Pla.unit

   | _ -> failwith "printLhsExp: All other cases should be already covered"

let wrapInt (is_int:bool) (e:jsexp) : Pla.t =
   let e_t = printExp e in
   if is_int then
      {pla|(<#e_t#>|0)|pla}
   else e_t

let rec printStmt (stmt:jsstmt) : Pla.t =
   match stmt with
   | JSVarDecl(JLWild,value,_) ->
      let value_t = printExp value in
      {pla|<#value_t#>;|pla}

   | JSVarDecl(JLId(name),value,is_int) ->
      let value_t = wrapInt is_int value in
      {pla|var <#name#s> = <#value_t#>;|pla}

   | JSVarDecl(JLTuple(elems),JEVar(name),_) ->
      List.mapi (printLhsExpTuple name true) elems
      |> Pla.join

   | JSVarDecl(JLTuple(_),_,_) -> failwith "printStmt: invalid tuple assign"

   | JSBind(JLWild,value,_) ->
      Pla.(printExp value ++ semi)

   | JSBind(JLTuple(elems),JEVar(name),_) ->
      List.mapi (printLhsExpTuple name false) elems
      |> Pla.join

   | JSBind(JLTuple(_),_,_) -> failwith "printStmt: invalid tuple assign"

   | JSBind(JLId(name),value,is_int) ->
      let value_t = wrapInt is_int value in
      {pla|<#name#s> = <#value_t#>;|pla}

   | JSFunction(name,args,(JSBlock(_) as body)) ->
      let args_t = Pla.map_sep Pla.comma Pla.string args in
      let body_t = printStmt body in
      {pla|this.<#name#s> = function(<#args_t#>)<#body_t#>|pla}

   | JSFunction(name,args,body) ->
      let args_t = Pla.map_sep Pla.comma Pla.string args in
      let body_t = printStmt body in
      {pla|this.<#name#s> = function(<#args_t#>) { <#body_t#>}|pla}

   | JSReturn(e1) ->
      let e_t = printExp e1 in
      {pla|return <#e_t#>;|pla}

   | JSWhile(cond,body) ->
      let cond_t = printExp cond in
      let body_t = printStmt body in
      {pla|while(<#cond_t#>)<#body_t#>|pla}

   | JSBlock(elems) ->
      let elems_t = printStmtList elems in
      {pla|{<#elems_t#+>}|pla}

   | JSIf(cond,then_,None) ->
      let cond_t = printExp cond in
      let then_t = printStmt then_ in
      {pla|if(<#cond_t#>)<#then_t#>|pla}

   | JSIf(cond,then_,Some(else_)) ->
      let cond_t = printExp cond in
      let then_t = printStmt then_ in
      let else_t = printStmt else_ in
      {pla|if(<#cond_t#>)<#then_t#><#>else<#><#else_t#>|pla}
   | JSEmpty -> Pla.unit

and printStmtList (stmts:jsstmt list) : Pla.t =
   Pla.map_sep_all Pla.newline printStmt stmts

let printJsCode args (stmts:stmt list) : string =
   let js = convertStmtList stmts in
   let t = printStmtList js in
   let code = Pla.print t in
   Templates.apply args.template code

let createParameters (args:arguments) : parameters =
   let template = Templates.get args.template in
   { template = template }

(** Generates the .c and .h file contents for the given parsed files *)
let generateJSCode (args:arguments) (parser_results:parser_results list) : (Pla.t * string) list =
   let params = createParameters args in
   let stmts =
      parser_results
      |> List.map (
         fun a -> match a.presult with
            | `Ok(b) -> b
            | _ -> [] )
      |> List.flatten
   in
   let js_text = printJsCode params stmts in
   [Pla.string js_text, "js"]


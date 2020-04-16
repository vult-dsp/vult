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

open Code
open Config

let dot = Pla.map_sep (Pla.string ".") Pla.string

module Templates = struct
   let none code = [ code, FileKind.ExtOnly "js" ]

   let runtime : Pla.t =
      Pla.string
         {| // Vult runtime functions
            this.random = function()         { return Math.random(); };
            this.irandom = function()        { return Math.floor(Math.random() * 4294967296); };
            this.eps  = function()           { return 1e-18 };
            this.pi   = function()           { return 3.1415926535897932384; }
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
            this.pow = function(a,b)         { return Math.pow(a,b); };
            this.sqrt = function(x)          { return x; };
            this.set  = function(a, i, v)    { a[i]=v; };
            this.get  = function(a, i)       { return a[i]; };
            this.int_to_float = function(i)  { return i; };
            this.float_to_int = function(i)  { return Math.floor(i); };
            this.makeArray = function(size, v){ var a = new Array(size); for(var i=0;i<size;i++) a[i]=v; return a; };
            this.wrap_array = function(a) { return a; }
            this.log = function(x) { console.log(x); }
         |}


   let common function_decl module_name code =
      {pla|<#function_decl#> {
           <#runtime#>
           this.<#module_name#s>_process_init = null;
           this.<#module_name#s>_default = null;
           <#code#>
           if(this.<#module_name#s>_process_init)  this.context =  this.<#module_name#s>_process_init(); else this.context = {};
           if(this.<#module_name#s>_default)      this.<#module_name#s>_default(this.context);
           this.liveNoteOn        = function(note,velocity,channel) { if(this.<#module_name#s>_noteOn)        this.<#module_name#s>_noteOn(this.context,note,velocity,channel); };
           this.liveNoteOff       = function(note,velocity,channel) { if(this.<#module_name#s>_noteOff)       this.<#module_name#s>_noteOff(this.context,note,velocity,channel); };
           this.liveControlChange = function(note,velocity,channel) { if(this.<#module_name#s>_controlChange) this.<#module_name#s>_controlChange(this.context,note,velocity,channel); };
           this.liveProcess       = function(input)         { if(this.<#module_name#s>_process)       return this.<#module_name#s>_process(this.context,input); else return 0; };
           this.liveDefault       = function() { if(this.<#module_name#s>_default)      return this.<#module_name#s>_default(this.context); };
           }|pla}


   let browser _config module_name code : (Pla.t * FileKind.t) list =
      let exports = Pla.string "function vultProcess()" in
      [ common exports module_name code, FileKind.ExtOnly "js" ]


   let node _config module_name code : (Pla.t * FileKind.t) list =
      let exports = Pla.string "exports.vultProcess = function ()" in
      [ common exports module_name code, FileKind.ExtOnly "js" ]


   let apply (params : params) (module_name : string) (template : string) (code : Pla.t) : (Pla.t * FileKind.t) list =
      match template with
      | "browser" -> browser params.config module_name code
      | "node" -> node params.config module_name code
      | "webaudio" -> Webaudio.get params runtime code
      | "performance" -> Performance.getJs params runtime code
      | _ -> none code
end

let isSpecial (params : params) (name : string) : bool =
   match name with
   | _ when name = params.module_name ^ "_process" -> true
   | _ when name = params.module_name ^ "_noteOn" -> true
   | _ when name = params.module_name ^ "_noteOff" -> true
   | _ when name = params.module_name ^ "_controlChange" -> true
   | _ when name = params.module_name ^ "_default" -> true
   | _ -> false


let fixContext (is_special : bool) args =
   if is_special then
      match args with
      | [] -> [ Ref (CTSimple "any"), "_ctx" ]
      | (_, "_ctx") :: _ -> args
      | t -> (Ref (CTSimple "any"), "_ctx") :: t
   else
      args


let rec printExp (params : params) (e : cexp) : Pla.t =
   match e with
   | CEEmpty -> Pla.unit
   | CEInt n -> {pla|(<#n#i>|0)|pla}
   | CEFloat (_, n) ->
      let sf = Float.to_string n in
      if n < 0.0 then {pla|(<#sf#s>)|pla} else Pla.string sf
   | CEBool v -> Pla.string (if v then "true" else "false")
   | CEString s -> Pla.wrap (Pla.string "\"") (Pla.string "\"") (Pla.string s)
   | CEArray (elems, _) ->
      let elems_t = Pla.map_sep Pla.comma (printExp params) elems in
      {pla|[<#elems_t#>]|pla}
   | CECall (name, args, _) ->
      let args_t = Pla.map_sep Pla.comma (printExp params) args in
      {pla|this.<#name#s>(<#args_t#>)|pla}
   | CEUnOp (op, e, _) ->
      let e_t = printExp params e in
      {pla|(<#op#s> <#e_t#>)|pla}
   | CEOp (op, elems, _) ->
      let op_t = {pla| <#op#s> |pla} in
      let elems_t = Pla.map_sep op_t (printExp params) elems in
      {pla|(<#elems_t#>)|pla}
   | CEVar (name, _) -> Pla.string name
   | CEIndex (e, index, _) ->
      let e = printExp params e in
      let index = printExp params index in
      {pla|<#e#>[<#index#>]|pla}
   | CEIf (cond, then_, else_, _) ->
      let cond_t = printExp params cond in
      let then_t = printExp params then_ in
      let else_t = printExp params else_ in
      {pla|(<#cond_t#>?<#then_t#>:<#else_t#>)|pla}
   | CETuple (elems, _) ->
      let elems_t = Pla.map_sep Pla.commaspace (printJsField params) elems in
      {pla|{ <#elems_t#> }|pla}
   | CEAccess (((CEVar _ | CEAccess _) as e), n) ->
      let e = printExp params e in
      {pla|<#e#>.<#n#s>|pla}
   | CEAccess (e, n) ->
      let e = printExp params e in
      {pla|(<#e#>).<#n#s>|pla}


and printJsField (params : params) (name, value) : Pla.t =
   let value_t = printExp params value in
   {pla|<#name#s> : <#value_t#>|pla}


let printLhsExpTuple (var : Pla.t) (is_var : bool) (i : int) (e : clhsexp) : Pla.t =
   match e with
   | CLId (_, name) ->
      let name = dot name in
      if is_var then
         {pla|var <#name#> = <#var#>.field_<#i#i>; |pla}
      else
         {pla|<#name#> = <#var#>.field_<#i#i>; |pla}
   | CLWild -> Pla.unit
   | _ -> failwith "printLhsExp: All other cases should be already covered"


let wrapInt (params : params) (is_int : bool) (e : cexp) : Pla.t =
   let e_t = printExp params e in
   if is_int then
      {pla|(<#e_t#>|0)|pla}
   else
      e_t


let rec getInitValue (descr : type_descr) : Pla.t =
   match descr with
   | CTSimple "int" -> Pla.string "(0|0)"
   | CTSimple "abstract" -> Pla.string "0"
   | CTSimple "float" -> Pla.string "0.0"
   | CTSimple "real" -> Pla.string "0.0"
   | CTSimple "bool" -> Pla.string "false"
   | CTSimple "unit" -> Pla.string "0"
   | CTArray (typ, size) ->
      let init = getInitValue typ in
      if size < 32 then
         let elems = CCList.init size (fun _ -> init) |> Pla.join_sep Pla.comma in
         {pla|[<#elems#>]|pla}
      else
         {pla|this.makeArray(<#size#i>,<#init#>)|pla}
   | _ -> Pla.string "{}"


let rec printSwitchStmt params e cases def =
   let e_t = printExp params e in
   let cases_t =
      Pla.map_sep_all
         Pla.newline
         (fun (v, stmt) ->
             let v_t = printExp params v in
             let stmt_t = CCOpt.get_or ~default:Pla.unit (printStmt params stmt) in
             {pla|case <#v_t#>:<#stmt_t#+><#>break;|pla})
         cases
   in
   let def_t =
      match def with
      | None -> Pla.unit
      | Some s ->
         match printStmt params s with
         | None -> Pla.unit
         | Some s -> {pla|default: <#s#+>|pla}
   in
   Some {pla|switch(<#e_t#>) {<#cases_t#+> <#def_t#><#>}|pla}


and printStmt (params : params) (stmt : cstmt) : Pla.t option =
   match stmt with
   | CSVar (CLWild, None) -> None
   | CSVar (CLId (tdescr, name), None) ->
      let init = getInitValue tdescr in
      let name = dot name in
      Some {pla|var <#name#> = <#init#>; |pla}
   | CSVar (CLTuple _, _) -> failwith "CodeJs.printStmt: invalid tuple assign"
   | CSVar (CLId (tdecr, name), Some value) ->
      let is_int = tdecr = CTSimple "int" in
      let value_t = wrapInt params is_int value in
      let name = dot name in
      Some {pla|var <#name#> = <#value_t#>; |pla}
   | CSVar (CLIndex (typ, name, _), None) ->
      let init = getInitValue typ in
      let name = dot name in
      Some {pla|var <#name#> = <#init#>; |pla}
   | CSVar (_, _) -> failwith "printStmt: invalid variable declaration"
   | CSConst (CLId (tdecr, name), value) ->
      let is_int = tdecr = CTSimple "int" in
      let value_t = wrapInt params is_int value in
      let name = dot name in
      Some {pla|var <#name#> = <#value_t#>; |pla}
   | CSConst (CLWild, value) ->
      let value_t = printExp params value in
      Some {pla|<#value_t#>; |pla}
   | CSConst (CLTuple elems, ((CEVar _ | CEAccess _) as rhs)) ->
      let rhs = printExp params rhs in
      List.mapi (printLhsExpTuple rhs true) elems |> Pla.join |> fun a -> Some a
   | CSConst _ -> failwith "printStmt: invalid constant assign"
   | CSBind (CLWild, value) -> Some Pla.(printExp params value ++ semi)
   | CSBind (CLTuple elems, ((CEVar _ | CEAccess _) as rhs)) ->
      let rhs = printExp params rhs in
      List.mapi (printLhsExpTuple rhs false) elems |> Pla.join |> fun a -> Some a
   | CSBind (CLTuple _, _) -> failwith "CodeJs.printStmt: invalid tuple assign"
   | CSBind (CLId (tdecr, name), value) ->
      let is_int = tdecr = CTSimple "int" in
      let value_t = wrapInt params is_int value in
      let name = dot name in
      Some {pla|<#name#> = <#value_t#>; |pla}
   | CSBind (CLIndex (tdecr, name, index), value) ->
      let is_int = tdecr = CTSimple "int" in
      let value_t = wrapInt params is_int value in
      let name = dot name in
      let index = printExp params index in
      Some {pla|<#name#>[<#index#>] = <#value_t#>; |pla}
   | CSFunction (_, name, args, (CSBlock _ as body), _) ->
      (* if the function has any of the special names add the ctx argument *)
      let args = fixContext (isSpecial params name) args in
      let args_t = Pla.map_sep Pla.comma (fun (_, a) -> Pla.string a) args in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some {pla|this.<#name#s> = function(<#args_t#>)<#body_t#>|pla}
   | CSFunction (_, name, args, body, _) ->
      let args_t = Pla.map_sep Pla.comma (fun (_, a) -> Pla.string a) args in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some {pla|this.<#name#s> = function(<#args_t#>) { <#body_t#>}|pla}
   | CSReturn e1 ->
      let e_t = printExp params e1 in
      Some {pla|return <#e_t#>; |pla}
   | CSWhile (cond, body) ->
      let cond_t = printExp params cond in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some {pla|while(<#cond_t#>)<#body_t#>|pla}
   | CSBlock elems ->
      let elems_t = printStmtList params elems in
      Some {pla|{<#elems_t#+>}|pla}
   | CSIf (cond, then_, None) ->
      let cond_t = printExp params cond in
      let then_t = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
      Some {pla|if(<#cond_t#>)<#then_t#>|pla}
   | CSIf (cond, then_, Some else_) ->
      let cond_t = printExp params cond in
      let then_t = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
      let else_t = CCOpt.get_or ~default:Pla.semi (printStmt params else_) in
      Some {pla|if(<#cond_t#>)<#then_t#><#>else<#><#else_t#>|pla}
   | CSSwitch (e, cases, def) -> printSwitchStmt params e cases def
   | CSEmpty -> None
   | CSType _ -> None
   | CSAlias _ -> None
   | CSExtFunc _ -> None


and printStmtList (params : params) (stmts : cstmt list) : Pla.t =
   let tstmts = CCList.filter_map (printStmt params) stmts in
   Pla.map_sep_all Pla.newline (fun a -> a) tstmts


let printJsCode (params : params) (stmts : cstmt list) : (Pla.t * FileKind.t) list =
   let code = printStmtList params stmts in
   Templates.apply params params.module_name params.template code


(** Generates the .c and .h file contents for the given parsed files *)
let print (params : params) (stmts : Code.cstmt list) : (Pla.t * FileKind.t) list = printJsCode params stmts

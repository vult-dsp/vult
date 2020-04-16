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
   type t =
      | None
      | Default

   let get template =
      match template with
      | "none" -> None
      | "default" -> Default
      | t -> failwith (Printf.sprintf "The template '%s' is not available for this generator" t)


   let none code = code

   let get_args inputs =
      inputs
      |> List.map (fun s ->
                  match s with
                  | IContext -> "ctx"
                  | IReal name
                  |IInt name
                  |IBool name ->
                     name)
      |> Pla.map_sep Pla.comma Pla.string


   let env =
      {pla|
function ternary(cond,then_,else_) if cond then return then_() else return else_() end end
function ternary_value(cond,then_,else_) if cond then return then_ else return else_ end end
function eps()              return 1e-18; end
function pi()               return 3.1415926535897932384; end
function random()           return math.random(); end
function irandom()          return math.floor(math.random() * 4294967296); end
function clip(x,low,high)   if x < low then return low else if x > high then return high else return x end end end
function real(x)            return x; end
function int(x)             local int_part,_ = math.modf(x) return int_part; end
function sin(x)             return math.sin(x); end
function cos(x)             return math.cos(x); end
function abs(x)             return math.abs(x); end
function exp(x)             return math.exp(x); end
function floor(x)           return math.floor(x); end
function tan(x)             return math.tan(x); end
function tanh(x)            return math.tanh(x); end
function pow(a,b)           return math.pow(a,b); end
function sqrt(x)            return x; end
function set(a, i, v)       a[i+1]=v; end
function get(a, i)          return a[i+1]; end
function makeArray(size, v) local a = {}; for i=1,size do a[i]=v end return a; end
function wrap_array(a)      return a; end
|pla}


   let default (config : config) module_name code =
      let process_inputs = get_args config.process_inputs in
      let noteon_inputs = get_args config.noteon_inputs in
      let noteoff_inputs = get_args config.noteoff_inputs in
      let controlchange_inputs = get_args config.controlchange_inputs in
      let nprocess_inputs = List.length config.process_inputs in
      let nprocess_outputs = List.length config.process_outputs in
      let nnoteon_inputs = List.length config.noteon_inputs in
      let nnoteoff_inputs = List.length config.noteoff_inputs in
      let ncontrolchange_inputs = List.length config.controlchange_inputs in
      let pass_data =
         if List.exists (fun a -> a = IContext) config.process_inputs then Pla.string "true" else Pla.string "false"
      in
      {pla|
<#env#>
<#code#>
local this = {}
function this.process(<#process_inputs#>) return <#module_name#s>_process(<#process_inputs#>) end
function this.noteOn(<#noteon_inputs#>) return <#module_name#s>_noteOn(<#noteon_inputs#>) end
function this.noteOff(<#noteoff_inputs#>) return <#module_name#s>_noteOff(<#noteoff_inputs#>) end
function this.controlChange(<#controlchange_inputs#>) return <#module_name#s>_controlChange(<#controlchange_inputs#>) end
function this.init() return <#module_name#s>_process_init() end
function this.default(ctx) return <#module_name#s>_default(ctx) end
this.config = { inputs = <#nprocess_inputs#i>, outputs = <#nprocess_outputs#i>, noteon_inputs = <#nnoteon_inputs#i>, noteoff_inputs = <#nnoteoff_inputs#i>, controlchange_inputs = <#ncontrolchange_inputs#i>, is_active = <#pass_data#> }
return this
|pla}


   let vcv (config : config) module_name code =
      let init_processor =
         match config.process_inputs with
         | IContext :: _ -> {pla|local processor = <#module_name#s>_process_init()|pla}
         | _ -> Pla.unit
      in
      let readInputs inputs =
         match inputs with
         | [] -> Pla.unit
         | IContext :: args ->
            let args =
               Pla.string "processor"
               :: List.mapi
                  (fun i _ ->
                      let i = i + 1 in
                      {pla|(block.inputs[<#i#i>][i] / 10.0)|pla})
                  args
            in
            Pla.join_sep Pla.comma args
         | _ ->
            let args =
               List.mapi
                  (fun i _ ->
                      let i = i + 1 in
                      {pla|(block.inputs[<#i#i>][i] / 10.0)|pla})
                  inputs
            in
            Pla.join_sep Pla.comma args
      in

      let bindOutputs outputs =
         match outputs with
         | [] -> Pla.unit, Pla.unit
         | [ _ ] -> {pla|block.outputs[1][i] = 10.0 * |pla}, Pla.unit
         | _ ->
            let bindings =
               List.mapi
                  (fun i _ ->
                      let li = i + 1 in
                      {pla|      block.outputs[<#li#i>][i] = 10.0 * processor.process_ret_<#i#i>|pla})
                  outputs
            in
            Pla.unit, Pla.join_sep_all Pla.newline bindings
      in
      let process_inputs = readInputs config.process_inputs in
      let process_lhs, bindings = bindOutputs config.process_outputs in
      let update_inputs = readInputs config.update_inputs in

      {pla|
local global_block = {}

function stringAppend(s1, s2)
  return s1 .. s2
end

function string(n)
   return tostring(n)
end

function getKnob(i)
  if i > 6 or i < 1 then
    return 0.0
  else
    return global_block.knobs[i]
  end
end

function getSwitch(i)
  if i > 6 or i < 1 then
    return false
  else
    return global_block.switch[i]
  end
end

function setLight(i, r, g ,b)
  if not(i > 6 or i < 1) then
    global_block.lights[i][1] = r
    global_block.lights[i][2] = g
    global_block.lights[i][3] = b
  end
end

function setSwitchLight(i, r, g ,b)
  if not(i > 6 or i < 1) then
    global_block.switchLights[i][1] = r
    global_block.switchLights[i][2] = g
    global_block.switchLights[i][3] = b
  end
end

function samplerate()
  return global_block.sampleRate
end

function sampletime()
  return global_block.sampleTime
end

<#env#>
<#code#>

config.frameDivider = 1
config.bufferSize = 32

<#init_processor#>

function process(block)
   global_block = block

   <#module_name#s>_update(<#update_inputs#>)

   for i=1,block.bufferSize do
      <#process_lhs#><#module_name#s>_process(<#process_inputs#>)
<#bindings#>
   end
end

|pla}


   let apply params (module_name : string) (template : string) (code : Pla.t) : (Pla.t * FileKind.t) list =
      match template with
      | "default" -> [ default params.config module_name code, FileKind.ExtOnly "lua" ]
      | "performance" -> Performance.getLua params (default params.config module_name code)
      | "vcv-prototype" -> [ vcv params.config module_name code, FileKind.ExtOnly "lua" ]
      | _ -> [ none code, FileKind.ExtOnly "lua" ]
end

let isSpecial (params : params) (name : string) : bool =
   if params.template <> "vcv-prototype" then
      match name with
      | _ when name = params.module_name ^ "_process" -> true
      | _ when name = params.module_name ^ "_noteOn" -> true
      | _ when name = params.module_name ^ "_noteOff" -> true
      | _ when name = params.module_name ^ "_controlChange" -> true
      | _ when name = params.module_name ^ "_default" -> true
      | _ -> false
   else
      false


let fixContext (is_special : bool) args =
   if is_special then
      match args with
      | [] -> [ Ref (CTSimple "any"), "_ctx" ]
      | (_, "_ctx") :: _ -> args
      | t -> (Ref (CTSimple "any"), "_ctx") :: t
   else
      args


let isSimpleExp e =
   match e with
   | CEInt _
   |CEFloat _
   |CEBool _
   |CEString _ ->
      true
   | _ -> false


let rec printExp (params : params) (e : cexp) : Pla.t =
   match e with
   | CEEmpty -> Pla.unit
   | CEInt n -> {pla|<#n#i>|pla}
   | CEFloat (_, n) ->
      let sf = Float.to_string n in
      if n < 0.0 then {pla|(<#sf#s>)|pla} else Pla.string sf
   | CEBool v -> Pla.string (if v then "true" else "false")
   | CEString s -> Pla.wrap (Pla.string "\"") (Pla.string "\"") (Pla.string s)
   | CEArray (elems, _) ->
      let elems_t = Pla.map_sep Pla.comma (printExp params) elems in
      {pla|{<#elems_t#>}|pla}
   | CECall ("not", [ arg ], _) ->
      let arg_t = printExp params arg in
      {pla|(not <#arg_t#>)|pla}
   | CECall (name, args, _) ->
      let args_t = Pla.map_sep Pla.comma (printExp params) args in
      {pla|<#name#s>(<#args_t#>)|pla}
   | CEUnOp (op, e, _) ->
      let e_t = printExp params e in
      {pla|(<#op#s> <#e_t#>)|pla}
   | CEOp (op, elems, _) ->
      let op_t = {pla| <#op#s> |pla} in
      let elems_t = Pla.map_sep op_t (printExp params) elems in
      {pla|(<#elems_t#>)|pla}
   | CEVar (name, _) -> Pla.string name
   | CEIndex (e, index, _) ->
      let index = printExp params index in
      let e = printExp params e in
      {pla|<#e#>[<#index#>+1]|pla}
   | CEIf (cond, then_, else_, _) when isSimpleExp then_ && isSimpleExp else_ ->
      let cond_t = printExp params cond in
      let then_t = printExp params then_ in
      let else_t = printExp params else_ in
      {pla|(ternary_value(<#cond_t#>, <#then_t#>, <#else_t#>)|pla}
   | CEIf (cond, then_, else_, _) ->
      let cond_t = printExp params cond in
      let then_t = printExp params then_ in
      let else_t = printExp params else_ in
      {pla|(ternary(<#cond_t#>, (function () <#then_t#> end), (function () <#else_t#> end))|pla}
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
   {pla|<#name#s> = <#value_t#>|pla}


let printLhsExpTuple (var : Pla.t) (is_var : bool) (i : int) (e : clhsexp) : Pla.t =
   match e with
   | CLId (_, name) ->
      let name = dot name in
      if is_var then
         {pla|local <#name#> = <#var#>.field_<#i#i>;|pla}
      else
         {pla|<#name#> = <#var#>.field_<#i#i>;|pla}
   | CLWild -> Pla.unit
   | _ -> failwith "printLhsExp: All other cases should be already covered"


let rec getInitValue (descr : type_descr) : Pla.t =
   match descr with
   | CTSimple "int" -> Pla.string "0"
   | CTSimple "float" -> Pla.string "0.0"
   | CTSimple "real" -> Pla.string "0.0"
   | CTSimple "bool" -> Pla.string "false"
   | CTSimple "unit" -> Pla.string "0"
   | CTArray (typ, size) ->
      let init = getInitValue typ in
      if size < 256 then
         let elems = CCList.init size (fun _ -> init) |> Pla.join_sep Pla.comma in
         {pla|{<#elems#>}|pla}
      else
         {pla|makeArray(<#size#i>,<#init#>)|pla}
   | _ -> Pla.string "{}"


let rec printSwitchStmt params e cases def =
   let e_t = printExp params e in
   let cases_t =
      Pla.map_sep_all
         Pla.newline
         (fun (v, stmt) ->
             let v_t = printExp params v in
             let stmt_t = CCOpt.get_or ~default:Pla.unit (printStmt params stmt) in
             {pla|case <#v_t#>:<#stmt_t#+>break;|pla})
         cases
   in
   let def_t =
      match def with
      | None -> Pla.unit
      | Some s ->
         match printStmt params s with
         | None -> Pla.unit
         | Some s -> {pla|default <#s#+>|pla}
   in
   Some {pla|switch(<#e_t#> {<#cases_t#> <#def_t#>})|pla}


and printStmt (params : params) (stmt : cstmt) : Pla.t option =
   match stmt with
   | CSVar (CLWild, None) -> None
   | CSVar (CLId (typ, name), None) ->
      let init = getInitValue typ in
      let name = dot name in
      Some {pla|local <#name#> = <#init#>;|pla}
   | CSVar (CLIndex (typ, name, _), None) ->
      let init = getInitValue typ in
      let name = dot name in
      Some {pla|local <#name#> = <#init#>;|pla}
   | CSVar (CLTuple _, _) -> failwith "CodeLua.printStmt: invalid tuple assign"
   | CSVar (CLId (_, name), Some value) ->
      let value_t = printExp params value in
      let name = dot name in
      Some {pla|local <#name#> = <#value_t#>;|pla}
   | CSVar (_, _) -> failwith "printStmt: invalid variable declaration"
   | CSConst (CLId (_, name), value) ->
      let value_t = printExp params value in
      let name = dot name in
      Some {pla|local <#name#> = <#value_t#>;|pla}
   | CSConst (CLWild, value) ->
      let value_t = printExp params value in
      Some {pla|<#value_t#>;|pla}
   | CSConst (CLTuple elems, ((CEVar _ | CEAccess _) as rhs)) ->
      let rhs = printExp params rhs in
      List.mapi (printLhsExpTuple rhs true) elems |> Pla.join |> fun a -> Some a
   | CSConst _ -> failwith "printStmt: invalid constant assign"
   | CSBind (CLWild, value) -> Some Pla.(printExp params value ++ semi)
   | CSBind (CLTuple elems, ((CEVar _ | CEAccess _) as rhs)) ->
      let rhs = printExp params rhs in
      List.mapi (printLhsExpTuple rhs false) elems |> Pla.join |> fun a -> Some a
   | CSBind (CLTuple _, _) -> failwith "CodeLua.printStmt: invalid tuple assign"
   | CSBind (CLId (_, name), value) ->
      let value_t = printExp params value in
      let name = dot name in
      Some {pla|<#name#> = <#value_t#>;|pla}
   | CSBind (CLIndex (_, name, index), value) ->
      let value_t = printExp params value in
      let name = dot name in
      let index = printExp params index in
      Some {pla|<#name#>[<#index#>+1] = <#value_t#>;|pla}
   | CSFunction (_, name, args, (CSBlock _ as body), _) ->
      (* if the function has any of the special names add the ctx argument *)
      let args = fixContext (isSpecial params name) args in
      let args_t = Pla.map_sep Pla.comma (fun (_, a) -> Pla.string a) args in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some {pla|function <#name#s>(<#args_t#>)<#body_t#+><#>end<#>|pla}
   | CSFunction (_, name, args, body, _) ->
      let args_t = Pla.map_sep Pla.comma (fun (_, a) -> Pla.string a) args in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some {pla|function <#name#s>(<#args_t#>)<#body_t#+><#>end<#>|pla}
   | CSReturn e1 ->
      let e_t = printExp params e1 in
      Some {pla|return <#e_t#>;|pla}
   | CSWhile (cond, body) ->
      let cond_t = printExp params cond in
      let body_t = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some {pla|while <#cond_t#> do<#body_t#+>end|pla}
   | CSBlock elems ->
      let elems_t = printStmtList params elems in
      Some {pla|<#elems_t#>|pla}
   | CSIf (cond, then_, None) ->
      let cond_t = printExp params cond in
      let then_t = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
      Some {pla|if <#cond_t#> then<#then_t#+><#>end|pla}
   | CSIf (cond, then_, Some else_) ->
      let cond_t = printExp params cond in
      let then_t = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
      let else_t = CCOpt.get_or ~default:Pla.semi (printStmt params else_) in
      Some {pla|if <#cond_t#> then<#then_t#+><#>else<#><#else_t#+><#>end|pla}
   | CSSwitch (e, cases, def) -> printSwitchStmt params e cases def
   | CSEmpty -> None
   | CSType _ -> None
   | CSAlias _ -> None
   | CSExtFunc _ -> None


and printStmtList (params : params) (stmts : cstmt list) : Pla.t =
   let tstmts = CCList.filter_map (printStmt params) stmts in
   Pla.map_sep_all Pla.newline (fun a -> a) tstmts


let printLuaCode (params : params) (stmts : cstmt list) : (Pla.t * FileKind.t) list =
   let code = printStmtList params stmts in
   Templates.apply params params.module_name params.template code


(** Generates the .c and .h file contents for the given parsed files *)
let print (params : params) (stmts : Code.cstmt list) : (Pla.t * FileKind.t) list = printLuaCode params stmts

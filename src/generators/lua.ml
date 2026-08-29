(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz

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

open Core.Prog

(* TODO:
   - String support: conversions and concatenation
*)

let rec isValueOrIf (e : exp) =
  match e.e with
  | EUnit | EBool _ | EInt _ | EReal _ | EString _ | EId _ | EMember _ | EFixed _ ->
      true
  | EUnOp (_, e) ->
      isValueOrIf e
  | EOp (_, e1, e2) ->
      isValueOrIf e1 && isValueOrIf e2
  | EIndex {e; index} ->
      isValueOrIf e && isValueOrIf index
  | EIf {then_; else_; _} ->
      isValueOrIf then_ && isValueOrIf else_
  | _ ->
      false

(* Only the runtime pieces used by the generated code are emitted. Simple
   builtins like eps, pi, clip, real, int, bool are inlined by the printer. *)

let luajit_detection = {%pla|-- LuaJIT detection
local isLuaJIT = type(jit) == "table" and jit.version
<#>|}

let initialize_array_runtime =
  {%pla|
-- Numeric arrays use FFI storage under LuaJIT, tables everywhere else.
local initializeArray
if isLuaJIT then
    local ffi = require("ffi")
    function initializeArray(v, n)
        if type(v) == "number" then
            -- One-based, so element zero is unused. This is cdata, not a
            -- table: indexing works, '#' and pairs() do not.
            local a = ffi.new("double[?]", n + 1)
            for i = 1, n do a[i] = v end
            return a
        else
            local a = {}
            for i = 1, n do a[i] = v end
            return a
        end
    end
else
    function initializeArray(v, n)
        local a = {}
        for i = 1, n do a[i] = v end
        return a
    end
end
<#>|}

(* Builtins Lua exposes as 'math.<name>' under the same name. atan2 and log10
   are absent: later Lua versions dropped them, so they need a fallback. *)
let math_builtins : Core.Builtin.t list =
  [Sin; Cos; Abs; Exp; Floor; Ceil; Tan; Tanh; Sinh; Cosh; Sqrt; Log; Asin; Acos; Atan; Min; Max]

(* 'modf' is not a Vult builtin; it is how the 'int' conversion is emitted. *)
let math_local_functions = CCList.map Core.Builtin.name math_builtins @ ["modf"]

let local_math_name path =
  let prefix = "math." in
  let prefix_length = String.length prefix in
  let name =
    if String.length path > prefix_length && String.sub path 0 prefix_length = prefix then
      String.sub path prefix_length (String.length path - prefix_length)
    else path
  in
  if CCList.exists (String.equal name) math_local_functions then Some name else None

let rec is_repeatable_exp (e : exp) =
  match e.e with
  | EUnit | EBool _ | EInt _ | EReal _ | EString _ | EId _ | EFixed _ ->
      true
  | EUnOp (_, e) | EMember (e, _) | ETMember (e, _) ->
      is_repeatable_exp e
  | EOp (_, lhs, rhs) | EIndex {e= lhs; index= rhs} ->
      is_repeatable_exp lhs && is_repeatable_exp rhs
  | EEmptyValue | EArray _ | ECall _ | EIf _ | ETuple _ | ERecord _ ->
      false

let bit_ops_runtime =
  {%pla|
-- Bit operations, using the LuaJIT bit library when available
local lshift, rshift
if isLuaJIT then
    local bit = require("bit")
    lshift = bit.lshift
    rshift = bit.rshift
else
    -- Arithmetic fallback for standard Lua
    function lshift(a, b) return a * (2 ^ b) end
    function rshift(a, b) return math.floor(a / (2 ^ b)) end
end
<#>|}

let core_runtime_functions =
  [ ( "ifExpressionValue"
    , {%pla|function ifExpressionValue(cond,then_,else_) if cond then return then_ else return else_ end end<#>|} )
  ; ( "ifExpression"
    , {%pla|function ifExpression(cond,then_,else_) if cond then return then_() else return else_() end end<#>|} )
  ; ("random", {%pla|function random()           return math.random() end<#>|})
  ; ("irandom", {%pla|function irandom()          return math.floor(math.random() * 4294967296) end<#>|})
  ; ( "int16"
    , {%pla|function int16(x)           local int_part,_ = math.modf(x) return math.max(-32768, math.min(32767, int_part)) end<#>|}
    )
  ; ("intDiv", {%pla|function intDiv(a, b)       return math.floor(a / b) end<#>|})
  ; ( "clipValue"
    , {%pla|local function clipValue(x, low, high) if x > high then return high elseif x < low then return low else return x end end<#>|}
    )
  ; ("list_clear", {%pla|function list_clear(t)      for k in pairs(t) do t[k] = nil end end<#>|}) ]

let runtime (args : Util.Args.args) (stmts : prog) =
  let calls = Usage.calledFunctions stmts in
  let uses name = Util.Maps.Set.mem name calls in
  let uses_shifts =
    uses "lshift" || uses "rshift"
    || Usage.existsExp (fun e -> match e.e with EOp ((OpLsh | OpRsh), _, _) -> true | _ -> false) stmts
  in
  let uses_if_value =
    Usage.existsExp
      (fun e -> match e.e with EIf {then_; else_; _} -> isValueOrIf then_ && isValueOrIf else_ | _ -> false)
      stmts
  in
  let uses_if_closure =
    Usage.existsExp
      (fun e -> match e.e with EIf {then_; else_; _} -> not (isValueOrIf then_ && isValueOrIf else_) | _ -> false)
      stmts
  in
  let uses_core name =
    match name with
    | "ifExpressionValue" ->
        uses_if_value
    | "ifExpression" ->
        uses_if_closure
    | "clipValue" ->
        Usage.existsExp
          (fun e ->
            match e.e with
            | ECall {path= "clip"; args= [x; low; high]} ->
                not (is_repeatable_exp x && is_repeatable_exp low && is_repeatable_exp high)
            | _ ->
                false )
          stmts
    | _ ->
        uses name
  in
  let initialize_array = if uses "initializeArray" then initialize_array_runtime else Pla.unit in
  let math_locals =
    let uses_math name = uses name || uses ("math." ^ name) || (String.equal name "modf" && uses "int") in
    let used = CCList.filter uses_math math_local_functions in
    (* Functions dropped by later Lua versions need a fallback, not an alias. *)
    let uses_atan2 = uses_math "atan2" in
    let atan2 =
      if uses_atan2 then
        (* math.atan2 was removed in Lua 5.4; the two-argument math.atan replaces it *)
        {%pla|local atan2 = math.atan2 or function(y, x) return math.atan(y, x) end<#>|}
      else Pla.unit
    in
    let uses_log10 = uses_math "log10" in
    let log10 =
      if uses_log10 then
        (* math.log10 was removed in Lua 5.3; the two-argument math.log replaces it *)
        {%pla|local log10 = math.log10 or function(x) return math.log(x, 10) end<#>|}
      else Pla.unit
    in
    if CCList.is_empty used && (not uses_atan2) && not uses_log10 then Pla.unit
    else
      let aliases = Pla.map_join (fun name -> {%pla|local <#name#s> = math.<#name#s><#>|}) used in
      {%pla|<#>-- Math functions<#><#aliases#><#atan2#><#log10#>|}
  in
  let bit_ops = if uses_shifts then bit_ops_runtime else Pla.unit in
  let core =
    let fragments =
      CCList.filter_map (fun (name, code) -> if uses_core name then Some code else None) core_runtime_functions
    in
    if CCList.is_empty fragments then Pla.unit else Pla.join ({%pla|<#>-- Core runtime functions<#>|} :: fragments)
  in
  let performance_template = match args.template with Some "performance" -> true | _ -> false in
  let needs_luajit_detection = uses "initializeArray" || uses_shifts || performance_template in
  let detection = if needs_luajit_detection then luajit_detection else Pla.unit in
  {%pla|<#><#detection#><#initialize_array#><#math_locals#><#bit_ops#><#core#><#>|}

let operator (op : operator) =
  match op with
  | OpAdd ->
      Pla.string "+"
  | OpSub ->
      Pla.string "-"
  | OpMul ->
      Pla.string "*"
  | OpDiv ->
      Pla.string "/"
  | OpMod ->
      Pla.string "%"
  | OpLand ->
      Pla.string "and"
  | OpLor ->
      Pla.string "or"
  | OpBor ->
      Pla.string "|"
  | OpBand ->
      Pla.string "&"
  | OpBxor ->
      Pla.string "^"
  | OpLsh ->
      Pla.string "<<"
  | OpRsh ->
      Pla.string ">>"
  | OpEq ->
      Pla.string "=="
  | OpNe ->
      Pla.string "~="
  | OpLt ->
      Pla.string "<"
  | OpLe ->
      Pla.string "<="
  | OpGt ->
      Pla.string ">"
  | OpGe ->
      Pla.string ">="

let uoperator (op : uoperator) = match op with UOpNeg -> Pla.string "-" | UOpNot -> Pla.string "not"

let rec print_exp e =
  match e.e with
  | EEmptyValue ->
      Pla.string "{}"
  | EUnit ->
      Pla.string ""
  | EBool v ->
      Pla.string (if v then "true" else "false")
  | EInt n ->
      Pla.int n
  | EReal n ->
      Pla.string (Util.Vfloat.to_string n)
  | EFixed n ->
      Pla.string (Util.Vfloat.to_string n)
  | EString s ->
      Pla.string_quoted s
  | EId id ->
      Pla.string id
  | EIndex {e; index= {e= EInt i; _}} ->
      let e = print_exp e in
      let index = i + 1 in
      {%pla|<#e#>[<#index#i>]|}
  | EIndex {e; index} ->
      (* Inline index+1 to avoid function call overhead in standard Lua *)
      let e = print_exp e in
      let index = print_exp index in
      {%pla|<#e#>[(<#index#>) + 1]|}
  | EArray l ->
      Pla.wrap (Pla.string "{") (Pla.string "}") (Pla.map_sep Pla.commaspace print_exp l)
  | ECall {path; args} -> (
    (* Use optimized functions when available *)
    match (local_math_name path, path, args) with
    | Some name, _, _ ->
        let args = Pla.map_sep Pla.commaspace print_exp args in
        {%pla|<#name#s>(<#args#>)|}
    | None, ("atan2" | "math.atan2"), _ ->
        let args = Pla.map_sep Pla.commaspace print_exp args in
        {%pla|atan2(<#args#>)|}
    | None, ("log10" | "math.log10"), _ ->
        let args = Pla.map_sep Pla.commaspace print_exp args in
        {%pla|log10(<#args#>)|}
    | None, "lshift", [a; b] ->
        let a = print_exp a in
        let b = print_exp b in
        {%pla|lshift(<#a#>, <#b#>)|}
    | None, "rshift", [a; b] ->
        let a = print_exp a in
        let b = print_exp b in
        {%pla|rshift(<#a#>, <#b#>)|}
    (* List operations *)
    | None, "list_size", [e1] ->
        let e1 = print_exp e1 in
        {%pla|#<#e1#>|}
    | None, "list_capacity", [_] ->
        {%pla|2147483647|}
    | None, "list_append", [l; v] ->
        let l = print_exp l in
        let v = print_exp v in
        {%pla|table.insert(<#l#>, <#v#>)|}
    | None, "list_insert", [l; i; v] ->
        let l = print_exp l in
        let i = print_exp i in
        let v = print_exp v in
        {%pla|table.insert(<#l#>, <#i#> + 1, <#v#>)|}
    | None, "list_remove", [l; i] ->
        let l = print_exp l in
        let i = print_exp i in
        {%pla|table.remove(<#l#>, <#i#> + 1)|}
    | None, "list_clear", [e1] ->
        let e1 = print_exp e1 in
        {%pla|list_clear(<#e1#>)|}
    | None, "list_reserve", [_; _] ->
        (* No-op for Lua *)
        {%pla|nil|}
    | None, "list_get", [l; i] ->
        let l = print_exp l in
        let i = print_exp i in
        (* Inline index+1 to avoid function call overhead *)
        {%pla|<#l#>[(<#i#>) + 1]|}
    | None, "list_set", [l; i; v] ->
        let l = print_exp l in
        let i = print_exp i in
        let v = print_exp v in
        (* Lua is 1-based *)
        {%pla|<#l#>[<#i#> + 1] = <#v#>|}
    (* Inline simple builtins to avoid function call overhead *)
    | None, "eps", [] ->
        {%pla|1e-18|}
    | None, "pi", [] ->
        {%pla|3.1415926535897932384|}
    | None, "real", [x] ->
        let x = print_exp x in
        {%pla|(<#x#>)|}
    | None, "int", [x] ->
        (* Inline int conversion using math.modf (truncates towards zero) *)
        let x = print_exp x in
        {%pla|(modf(<#x#>))|}
    | None, "bool", [x] ->
        (* Inline bool conversion *)
        let x = print_exp x in
        {%pla|((<#x#>) ~= 0 and (<#x#>) ~= false)|}
    | None, "not_", [x] ->
        (* Inline logical not *)
        let x = print_exp x in
        {%pla|(not (<#x#>))|}
    | None, "clip", [x; low; high] when is_repeatable_exp x && is_repeatable_exp low && is_repeatable_exp high ->
        (* Inline clip as: (x > high) and high or ((x < low) and low or x) *)
        let x = print_exp x in
        let low = print_exp low in
        let high = print_exp high in
        {%pla|((<#x#>) > (<#high#>) and (<#high#>) or ((<#x#>) < (<#low#>) and (<#low#>) or (<#x#>)))|}
    | None, "clip", [x; low; high] ->
        let x = print_exp x in
        let low = print_exp low in
        let high = print_exp high in
        {%pla|clipValue(<#x#>, <#low#>, <#high#>)|}
    | None, _, _ ->
        let args = Pla.map_sep Pla.commaspace print_exp args in
        {%pla|<#path#s>(<#args#>)|} )
  | EUnOp (op, e) ->
      let e = print_exp e in
      let op = uoperator op in
      {%pla|(<#op#><#e#>)|}
  | EOp (op, e1, e2) -> (
      let se1 = print_exp e1 in
      let se2 = print_exp e2 in
      match op with
      (* Use optimized bit shift functions (LuaJIT uses bit library, standard Lua uses arithmetic) *)
      | OpLsh ->
          {%pla|lshift(<#se1#>, <#se2#>)|}
      | OpRsh ->
          {%pla|rshift(<#se1#>, <#se2#>)|}
      | _ ->
          let op = operator op in
          {%pla|(<#se1#> <#op#> <#se2#>)|} )
  | EIf {cond; then_; else_} when isValueOrIf then_ && isValueOrIf else_ ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      {%pla|ifExpressionValue(<#cond#>, <#then_#>, <#else_#>)|}
  | EIf {cond; then_; else_} ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      {%pla|ifExpression(<#cond#>, (function () return <#then_#> end), (function () return <#else_#> end))|}
  | ETuple l ->
      let l = Pla.map_sep Pla.commaspace print_exp l in
      {%pla|{ <#l#> }|}
  | EMember (e, m) ->
      let e = print_exp e in
      {%pla|<#e#>.<#m#s>|}
  | ETMember (e, i) ->
      let e = print_exp e in
      let m = i + 1 in
      {%pla|<#e#>[<#m#i>]|}
  | ERecord {elems; _} ->
      let printElem (n, v) =
        let v = print_exp v in
        {%pla|<#n#s> = <#v#>|}
      in
      let elems = Pla.map_sep Pla.commaspace printElem elems in
      {%pla|{ <#elems#> }|}

let rec print_lexp e =
  match e.l with
  | LWild ->
      Pla.string "_wild"
  | LId s ->
      Pla.string s
  | LMember (e, m) ->
      let e = print_lexp e in
      {%pla|<#e#>.<#m#s>|}
  | LIndex {e; index= {e= EInt i; _}} ->
      let e = print_lexp e in
      let index = i + 1 in
      {%pla|<#e#>[<#index#i>]|}
  | LIndex {e; index} ->
      let e = print_lexp e in
      let index = print_exp index in
      {%pla|<#e#>[<#index#> + 1]|}
  | _ ->
      failwith "Lua:print_lexp LTuple"

let print_dexp (e : dexp) =
  match e.d with DId (id, None) -> {%pla|<#id#s>|} | DId (id, Some dim) -> {%pla|<#id#s>[<#dim#i>]|}

let rec print_stmt (s : stmt) =
  match s.s with
  (* if the name is _ctx, do not call the allocator*)
  | StmtDecl (({d= DId ("_ctx", _); t= {t= TStruct _; _}; _} as lhs), None) ->
      let lhs = print_dexp lhs in
      {%pla|local <#lhs#> = {};|}
  (* needs allocation *)
  | StmtDecl (({t= {t= TStruct {path; _}; _}; _} as lhs), None) ->
      let lhs = print_dexp lhs in
      {%pla|local <#lhs#> = <#path#s>_alloc();|}
  | StmtDecl (lhs, None) ->
      let lhs = print_dexp lhs in
      {%pla|local <#lhs#>|}
  | StmtDecl (lhs, Some rhs) ->
      let lhs = print_dexp lhs in
      let rhs = print_exp rhs in
      {%pla|local <#lhs#> = <#rhs#>|}
  | StmtBind ({l= LWild; _}, rhs) ->
      let rhs = print_exp rhs in
      {%pla|<#rhs#>|}
  | StmtBind (lhs, rhs) ->
      let lhs = print_lexp lhs in
      let rhs = print_exp rhs in
      {%pla|<#lhs#> = <#rhs#>|}
  | StmtReturn e ->
      let e = print_exp e in
      {%pla|return <#e#>|}
  | StmtIf (cond, then_, None) ->
      let e = print_exp cond in
      let then_ = print_stmt then_ in
      {%pla|if <#e#> then<#then_#+><#>end|}
  | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_stmt then_ in
      let else_ = print_stmt else_ in
      {%pla|if <#cond#> then<#then_#+><#>else<#else_#+><#>end|}
  | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_stmt stmt in
      {%pla|while <#cond#> do<#stmt#+><#>end|}
  | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      {%pla|do<#stmt#+>end|}
  | StmtSwitch (e1, cases, default) -> (
      let if_ =
        CCList.fold_right
          (fun (e2, body) else_ ->
            let cond = C.eeq e1 e2 in
            Some (C.sif cond body else_) )
          cases default
      in
      match if_ with None -> Pla.unit | Some if_ -> print_stmt if_ )

let print_arg ({name; _} : param) = {%pla|<#name#s>|}

let print_function_def (def : function_def) =
  let name = def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  {%pla|function <#name#s>(<#args#>)|}

let print_body body =
  match body.s with
  | StmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
      {%pla|<#stmts#+>end|}
  | _ ->
      let stmt = print_stmt body in
      {%pla|<#stmt#+><#>end|}

let print_top_stmt (args : Util.Args.args) t =
  match t.top with
  | TopFunction (def, body) ->
      let def = print_function_def def in
      let body = print_body body in
      {%pla|<#def#><#body#><#><#>|}
  | TopExternal _ ->
      Pla.unit
  | TopType _ ->
      Pla.unit
  | TopAlias _ ->
      Pla.unit
  | TopConstant (name, _, _, _, _) when args.test_mode ->
      {%pla|<#name#s> = {};<#>|}
  | TopConstant (name, _, _, rhs, _) ->
      let rhs = print_exp rhs in
      {%pla|local <#name#s> = <#rhs#><#>|}

let print_prog args t = Pla.map_join (print_top_stmt args) t

let getTemplateCode (args : Util.Args.args) (stmts : top_stmt list) =
  match args.template with
  | None ->
      (Pla.unit, Pla.unit)
  | Some "performance" ->
      T_performance.generateLua args
  | Some "vcv-prototype" ->
      T_vcv_prototype.generate args stmts
  | Some name ->
      Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "'")

let generate (args : Util.Args.args) (stmts : top_stmt list) =
  let file = Common.setExt ".lua" args.output in
  let code = print_prog args stmts in
  let pre, post = getTemplateCode args stmts in
  let runtime = runtime args stmts in
  [({%pla|<#runtime#><#pre#><#code#><#post#>|}, file)]

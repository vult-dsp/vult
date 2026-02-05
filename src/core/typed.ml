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
open Util
open Pparser

type path = Syntax.path

let print_exp_types = false

let print_exp_locs = ref false

let print_path (p : path) = Syntax.print_path p

(** Print location in format [line:col-line:col] *)
let print_loc (loc : Loc.t) : Pla.t =
  let start_line = Loc.startLine loc in
  let end_line = Loc.endLine loc in
  let start_col = Loc.startColumn loc in
  let end_col = Loc.endColumn loc in
  {%pla|[<#start_line#i>:<#start_col#i>-<#end_line#i>:<#end_col#i>]|}


type constness_d =
  | TEConst of int
  | TEMut of int
  | TECLink of constness

and constness = { mutable c : constness_d }

type type_d_ =
  | TENoReturn
  | TEUnbound of int option (* None marks an explicit unbound type *)
  | TEId of path
  | TESize of int
  | TELink of type_
  | TEOption of type_ list
  | TEComposed of string * type_ list
  | TEFunction of type_ list * type_

and type_ =
  { mutable tx : type_d_
  ; const : constness
  ; mutable loc : Loc.t
  }

type fun_type = type_ list * type_

let rec compare_type_ (a : type_) (b : type_) =
  if a == b then
    0
  else
    match a.tx, b.tx with
    | TELink a, _ -> compare_type_ a b
    | _, TELink b -> compare_type_ a b
    | TEId p1, TEId p2 -> Syntax.compare_path p1 p2
    | TESize p1, TESize p2 -> compare p1 p2
    | TEComposed (n1, e1), TEComposed (n2, e2) -> CCOrd.(string n1 n2 <?> (compare_type_list_, e1, e2))
    | TEOption e1, TEOption e2 -> compare_type_list_ e1 e2
    | TEUnbound n1, TEUnbound n2 -> compare n1 n2
    | _ -> compare a.tx b.tx


and compare_type_list_ a b = CCOrd.list compare_type_ a b

type tag = Ptags.tag

type exp_d =
  | EUnit
  | EBool of bool
  | EInt of int
  | EReal of float
  | EFixed of float
  | EString of string
  | EId of string
  | EConst of path
  | EIndex of
      { e : exp
      ; index : exp
      }
  | EArray of exp list
  | ECall of
      { instance : string option
      ; path : path
      ; args : exp list
      }
  | EUnOp of string * exp
  | EOp of string * exp * exp
  | EIf of
      { cond : exp
      ; then_ : exp
      ; else_ : exp
      }
  | ETuple of exp list
  | EMember of exp * string
  | ERecord of
      { path : path
      ; elems : (string * exp) list
      }
  | EGenCall of
      { generic_path : path (* Full path to the generic function for lookup *)
      ; args : exp list (* Processed function arguments *)
      ; explicit_args : exp list (* Processed explicit generic arguments (functions, constants) *)
      }
  | ETypeIntrinsic of
      { intrinsic : type_intrinsic (* Which intrinsic: typedefault, typemax, typemin *)
      ; type_param : string (* The generic type parameter name, e.g., "t" from 't *)
      }

(** Type intrinsics that depend on the concrete type during generic instantiation *)
and type_intrinsic =
  | TypeDefault (* typedefault('t) - default value for the type *)
  | TypeMax (* typemax('t) - maximum value for the type *)
  | TypeMin (* typemin('t) - minimum value for the type *)

and exp =
  { e : exp_d
  ; loc : Loc.t
  ; t : type_
  }

and lexp_d =
  | LWild
  | LId of string
  | LMember of lexp * string
  | LIndex of
      { e : lexp
      ; index : exp
      }
  | LTuple of lexp list

and lexp =
  { l : lexp_d
  ; loc : Loc.t
  ; t : type_
  }

type dexp_d =
  | DWild
  | DId of string * int option
  | DTuple of dexp list

and dexp =
  { d : dexp_d
  ; loc : Loc.t
  ; t : type_
  }

and stmt_d =
  | StmtVal of dexp
  | StmtMem of dexp * tag list
  | StmtBind of lexp * exp
  | StmtReturn of exp
  | StmtBlock of stmt list
  | StmtIf of exp * stmt * stmt option
  | StmtWhile of exp * stmt

and stmt =
  { s : stmt_d
  ; loc : Loc.t
  }

and arg =
  { name : string
  ; t : type_
  ; loc : Loc.t
  }

and function_def =
  { name : path
  ; args : arg list
  ; t : type_ list * type_
  ; next : (function_def * stmt) option
  ; loc : Loc.t
  ; tags : tag list
  ; is_root : bool
  }

type top_stmt_d =
  | TopExternal of function_def * string option
  | TopFunction of function_def * stmt
  | TopGenericPlaceholder of string (* Marks where specializations of a generic function should be inserted *)
  | TopType of
      { path : path
      ; members : (string * type_ * Ptags.tags * Loc.t) list
      }
  | TopAlias of
      { path : path
      ; alias_of : path
      }
  | TopEnum of
      { path : path
      ; members : (string * Loc.t) list
      }
  | TopConstant of path * int option * type_ * exp * Ptags.tags option

and top_stmt =
  { top : top_stmt_d
  ; loc : Loc.t
  }

type program = top_stmt list

type generic_param =
  | GParamFunction of string * type_ option (* Function type using TEFunction *)
  | GParamType of string
  | GParamConstant of string * type_

type param_kind =
  | PKGeneric of int (* Index into generic_params list *)
  | PKArg of int (* Index into args list *)

type generic_function =
  { name : string
  ; generic_params : generic_param list
  ; args : arg list
  ; param_order : param_kind list (* Original order of parameters - maps call position to param *)
  ; t : fun_type (* Keep as fun_type for function definitions *)
  ; body : Syntax.stmt (* Store the unprocessed body *)
  ; loc : Loc.t
  ; tags : tag list
  ; type_index : int (* Index for type ordering - captured at definition time *)
  }

type generic_binding =
  | BindFunction of string * type_ (* Function type using TEFunction *)
  | BindType of type_
  | BindConstant of exp * type_
  | BindNonSpecializable (* Marker for non-specializable parameters *)

(* Concrete constant values for generic instantiation *)
type constant_value =
  | IntConstant of int
  | RealConstant of float
  | BoolConstant of bool
  | StringConstant of string

(* Enhanced generic binding with complete type information *)
type generic_binding_value =
  | FunctionBinding of
      { func_name : string
      ; arg_types : type_ list
      ; return_type : type_
      }
  | TypeBinding of type_
  | ConstantBinding of
      { value : constant_value
      ; value_type : type_
      }

(* Individual generic parameter binding *)
type generic_param_binding =
  { param_name : string
  ; binding : generic_binding_value
  }

(* Regular function argument signature *)
type function_arg_signature =
  { param_name : string
  ; arg_type : type_
  }

(* Complete instantiation signature including generic params, function args, and return type *)
type instantiation_signature =
  { generic_name : string
  ; generic_params : generic_param_binding list
  ; function_args : function_arg_signature list
  ; return_type : type_
  }

type generic_instantiation =
  { signature : instantiation_signature
  ; specialized_name : string
  ; bindings : (string * generic_binding) list (* Legacy - will be removed *)
  ; specialized_def : function_def
  }

(* Comparison functions for signature-based lookup *)
let compare_constant_value (c1 : constant_value) (c2 : constant_value) : int =
  match c1, c2 with
  | IntConstant i1, IntConstant i2 -> Int.compare i1 i2
  | RealConstant r1, RealConstant r2 -> Float.compare r1 r2
  | BoolConstant b1, BoolConstant b2 -> Bool.compare b1 b2
  | StringConstant s1, StringConstant s2 -> String.compare s1 s2
  | IntConstant _, _ -> -1
  | RealConstant _, IntConstant _ -> 1
  | RealConstant _, _ -> -1
  | BoolConstant _, (IntConstant _ | RealConstant _) -> 1
  | BoolConstant _, _ -> -1
  | StringConstant _, _ -> 1


let compare_type (t1 : type_) (t2 : type_) : int =
  (* Use existing type comparison function *)
  compare_type_ t1 t2


let compare_generic_binding_value (b1 : generic_binding_value) (b2 : generic_binding_value) : int =
  match b1, b2 with
  | FunctionBinding f1, FunctionBinding f2 ->
    let name_cmp = String.compare f1.func_name f2.func_name in
    if name_cmp <> 0 then
      name_cmp
    else
      let args_cmp = CCList.compare compare_type f1.arg_types f2.arg_types in
      if args_cmp <> 0 then
        args_cmp
      else
        compare_type f1.return_type f2.return_type
  | TypeBinding t1, TypeBinding t2 -> compare_type t1 t2
  | ConstantBinding c1, ConstantBinding c2 ->
    let value_cmp = compare_constant_value c1.value c2.value in
    if value_cmp <> 0 then
      value_cmp
    else
      compare_type c1.value_type c2.value_type
  | FunctionBinding _, _ -> -1
  | TypeBinding _, FunctionBinding _ -> 1
  | TypeBinding _, _ -> -1
  | ConstantBinding _, _ -> 1


let compare_generic_param_binding (p1 : generic_param_binding) (p2 : generic_param_binding) : int =
  let name_cmp = String.compare p1.param_name p2.param_name in
  if name_cmp <> 0 then
    name_cmp
  else
    compare_generic_binding_value p1.binding p2.binding


let compare_function_arg_signature (a1 : function_arg_signature) (a2 : function_arg_signature) : int =
  let name_cmp = String.compare a1.param_name a2.param_name in
  if name_cmp <> 0 then
    name_cmp
  else
    compare_type a1.arg_type a2.arg_type


let compare_instantiation_signature (s1 : instantiation_signature) (s2 : instantiation_signature) : int =
  let name_cmp = String.compare s1.generic_name s2.generic_name in
  if name_cmp <> 0 then
    name_cmp
  else
    let generic_params_cmp = CCList.compare compare_generic_param_binding s1.generic_params s2.generic_params in
    if generic_params_cmp <> 0 then
      generic_params_cmp
    else
      let function_args_cmp = CCList.compare compare_function_arg_signature s1.function_args s2.function_args in
      if function_args_cmp <> 0 then
        function_args_cmp
      else
        compare_type s1.return_type s2.return_type


let rec print_constness (c : constness) =
  match c.c with
  | TEConst i -> {%pla|const<#i#i> |}
  | TEMut i -> {%pla|mut<#i#i> |}
  | TECLink c -> print_constness c


let rec print_type_ ?(detailed = false) (t : type_) : Pla.t =
  let prefix pt =
    if detailed || print_exp_types then
      Pla.append (print_constness t.const) pt
    else
      pt
  in
  match t.tx with
  | TENoReturn -> Pla.string "noreturn"
  | TELink t -> print_type_ ~detailed t
  | TEUnbound (Some i) ->
    if detailed then
      {%pla|_<#i#i>|}
    else
      Pla.string "_"
  | TEUnbound None -> Pla.string "_"
  | TEId p -> prefix @@ print_path p
  | TESize n -> Pla.int n
  | TEOption alt -> prefix @@ Pla.parenthesize @@ Pla.map_sep (Pla.string "|") print_type_ alt
  | TEComposed (name, elems) ->
    let elems = Pla.map_sep Pla.commaspace (print_type_ ~detailed) elems in
    prefix {%pla|<#name#s>(<#elems#>)|}
  | TEFunction (args, ret) ->
    let args = Pla.map_sep Pla.commaspace (print_type_ ~detailed) args in
    let ret = print_type_ ~detailed ret in
    prefix {%pla|<#args#> -> <#ret#>|}


let rec print_exp (e : exp) =
  (fun es ->
    let with_type =
      if print_exp_types then
        let t = print_type_ e.t in
        {%pla|(<#es#> : <#t#>)|}
      else
        es
    in
    if !print_exp_locs then
      let loc = print_loc e.loc in
      {%pla|<#with_type#><#loc#>|}
    else
      with_type)
  @@
  match e.e with
  | EUnit -> Pla.string "()"
  | EBool v ->
    Pla.string
      (if v then
         "true"
       else
         "false")
  | EInt n -> Pla.int n
  | EReal n -> Pla.float n
  | EFixed n -> {%pla|<#n#f>x]|}
  | EString s -> Pla.string_quoted s
  | EId id -> Pla.string id
  | EConst p -> print_path p
  | EIndex { e; index } ->
    let e = print_exp e in
    let index = print_exp index in
    {%pla|<#e#>[<#index#>]|}
  | EArray l -> Pla.wrap (Pla.string "[ ") (Pla.string " ]") (Pla.map_sep Pla.commaspace print_exp l)
  | ECall { instance; path; args } ->
    let instance = Option.value (Option.map (fun s -> {%pla|<#s#s>:|}) instance) ~default:Pla.unit in
    let path = print_path path in
    let args = Pla.map_sep Pla.commaspace print_exp args in
    {%pla|<#instance#><#path#>(<#args#>)|}
  | EUnOp (op, e) ->
    let e = print_exp e in
    {%pla|(<#op#s><#e#>)|}
  | EOp (op, e1, e2) ->
    let e1 = print_exp e1 in
    let e2 = print_exp e2 in
    {%pla|(<#e1#> <#op#s> <#e2#>)|}
  | EIf { cond; then_; else_ } ->
    let cond = print_exp cond in
    let then_ = print_exp then_ in
    let else_ = print_exp else_ in
    {%pla|(if <#cond#> then <#then_#> else <#else_#>)|}
  | ETuple l ->
    let l = Pla.map_sep Pla.commaspace print_exp l in
    {%pla|(<#l#>)|}
  | EMember (e, m) ->
    let e = print_exp e in
    {%pla|<#e#>.<#m#s>|}
  | ERecord { path; elems } ->
    let printElem (id, v) =
      let v = print_exp v in
      {%pla|<#id#s> = <#v#>|}
    in
    let path = print_path path in
    let elems = Pla.map_sep Pla.commaspace printElem elems in
    {%pla|<#path#> { <#elems#> }|}
  | EGenCall { generic_path; args; explicit_args } ->
    let all_args = explicit_args @ args in
    let args = Pla.map_sep Pla.commaspace print_exp all_args in
    let path = print_path generic_path in
    {%pla|<#path#>@generic(<#args#>)|}
  | ETypeIntrinsic { intrinsic; type_param } ->
    let intrinsic_name =
      match intrinsic with
      | TypeDefault -> "typedefault"
      | TypeMax -> "typemax"
      | TypeMin -> "typemin"
    in
    {%pla|<#intrinsic_name#s>('<#type_param#s>)|}


let rec print_lexp (e : lexp) =
  (fun es ->
    let with_type =
      if print_exp_types then
        let t = print_type_ e.t in
        {%pla|(<#es#> : <#t#>)|}
      else
        es
    in
    if !print_exp_locs then
      let loc = print_loc e.loc in
      {%pla|<#with_type#><#loc#>|}
    else
      with_type)
  @@
  match e.l with
  | LWild -> Pla.string "_"
  | LId s -> Pla.string s
  | LMember (e, m) ->
    let e = print_lexp e in
    {%pla|<#e#>.<#m#s>|}
  | LIndex { e; index } ->
    let e = print_lexp e in
    let index = print_exp index in
    {%pla|<#e#>[<#index#>]|}
  | LTuple l ->
    let l = Pla.map_sep Pla.commaspace print_lexp l in
    {%pla|(<#l#>)|}


let rec print_dexp (e : dexp) =
  let t = print_type_ ~detailed:true e.t in
  let base =
    match e.d with
    | DWild -> {%pla|_ : <#t#>|}
    | DId (id, None) -> {%pla|<#id#s> : <#t#>|}
    | DId (id, Some dim) -> {%pla|<#id#s>[<#dim#i>] : <#t#>|}
    | DTuple l ->
      let l = Pla.map_sep Pla.commaspace print_dexp l in
      {%pla|(<#l#>) : <#t#>|}
  in
  if !print_exp_locs then
    let loc = print_loc e.loc in
    {%pla|<#base#><#loc#>|}
  else
    base


let rec print_stmt s =
  let base =
    match s.s with
    | StmtVal lhs ->
      let lhs = print_dexp lhs in
      {%pla|val <#lhs#>;|}
    | StmtMem (lhs, tags) ->
      let tags = Ptags.print_tags tags in
      let lhs = print_dexp lhs in
      {%pla|mem <#lhs#><#tags#>;|}
    | StmtBind (lhs, rhs) ->
      let lhs = print_lexp lhs in
      let rhs = print_exp rhs in
      {%pla|<#lhs#> = <#rhs#>;|}
    | StmtReturn e ->
      let e = print_exp e in
      {%pla|return <#e#>;|}
    | StmtIf (cond, then_, None) ->
      let e = print_exp cond in
      let then_ = print_stmt then_ in
      {%pla|if (<#e#>) <#then_#>|}
    | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_stmt then_ in
      let else_ = print_stmt else_ in
      {%pla|if (<#cond#>) <#then_#><#>else <#else_#>|}
    | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_stmt stmt in
      {%pla|while (<#cond#>)<#stmt#+>|}
    | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      {%pla|{<#stmt#+>}|}
  in
  if !print_exp_locs then
    let loc = print_loc s.loc in
    {%pla|<#base#><#loc#>|}
  else
    base


let print_arg (arg : arg) =
  let t = print_type_ ~detailed:true arg.t in
  let name = arg.name in
  let base = {%pla|<#name#s> : <#t#>|} in
  if !print_exp_locs then
    let loc = print_loc arg.loc in
    {%pla|<#base#><#loc#>|}
  else
    base


(* Variant type for function definition keywords *)
type fun_kind =
  | FunKindFun
  | FunKindAnd
  | FunKindExternal

let fun_kind_to_string (kind : fun_kind) : string =
  match kind with
  | FunKindFun -> "fun"
  | FunKindAnd -> "and"
  | FunKindExternal -> "external"


let next_kind (kind : fun_kind) : fun_kind =
  match kind with
  | FunKindFun -> FunKindAnd
  | FunKindAnd -> FunKindAnd
  | FunKindExternal -> FunKindExternal


let print_body_linkname body_linkname =
  match body_linkname with
  | `Body stmt -> print_stmt stmt
  | `LinkName name -> {%pla| "<#name#s>"|}
  | `NoLinkName -> Pla.unit


let rec print_function_def (kind : fun_kind) (def : function_def) body_linkname =
  let kind_str = fun_kind_to_string kind in
  let name = print_path def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  let tags = Ptags.print_tags def.tags in
  let t = print_type_ ~detailed:true (snd def.t) in
  let body = print_body_linkname body_linkname in
  let next = print_next_function_def kind def.next in
  let def_loc =
    if !print_exp_locs then
      let loc = print_loc def.loc in
      {%pla| #def<#loc#>|}
    else
      Pla.unit
  in
  {%pla|<#kind_str#s> <#name#>(<#args#>) : <#t#><#def_loc#> <#tags#><#body#><#><#next#>|}


and print_next_function_def kind next =
  match next with
  | None -> Pla.unit
  | Some (def, body) -> print_function_def (next_kind kind) def (`Body body)


let print_record_member (name, t, tags, _) =
  let tags = Ptags.print_tags tags in
  let t = print_type_ t in
  {%pla|<#name#s> : <#t#><#tags#>;|}


let print_enum_member (name, _) = {%pla|<#name#s>|}

let print_top_stmt t =
  let base =
    match t.top with
    | TopFunction (def, body) -> print_function_def FunKindFun def (`Body body)
    | TopExternal (def, Some linkname) -> print_function_def FunKindExternal def (`LinkName linkname)
    | TopExternal (def, None) -> print_function_def FunKindExternal def `NoLinkName
    | TopGenericPlaceholder name -> {%pla|(* generic placeholder: <#name#s> *)|}
    | TopAlias { path = p; alias_of } ->
      let p = print_path p in
      let alias_of = print_path alias_of in
      {%pla|type <#p#> = <#alias_of#><#>|}
    | TopType { path = p; members } ->
      let p = print_path p in
      let members = Pla.map_sep_all Pla.newline print_record_member members in
      {%pla|type <#p#> {<#members#+>}<#>|}
    | TopEnum { path = p; members } ->
      let p = print_path p in
      let members = Pla.map_sep {%pla|,<#>|} print_enum_member members in
      {%pla|enum <#p#> {<#members#+><#>}<#>|}
    | TopConstant (path, dim, _, e, _) ->
      let path = print_path path in
      let e = print_exp e in
      let dim =
        match dim with
        | None -> Pla.unit
        | Some dim -> {%pla|[<#dim#i>]|}
      in
      {%pla|constant <#path#><#dim#> = <#e#>|}
  in
  if !print_exp_locs then
    let loc = print_loc t.loc in
    {%pla|<#base#> @<#loc#>|}
  else
    base


let print_prog prog = Pla.map_sep_all Pla.newline print_top_stmt prog

module C = struct
  let tick = ref 0

  let ctick = ref 0

  let const () =
    incr ctick;
    { c = TEConst !ctick }


  let makeId loc id = { tx = TEId { id; n = None; loc }; loc; const = const () }

  let path_t loc path = { tx = TEId path; loc; const = const () }

  let unbound loc =
    incr tick;
    { tx = TEUnbound (Some !tick); loc; const = const () }


  let noreturn loc = { tx = TENoReturn; loc; const = const () }

  let unit ~loc = makeId loc "unit"

  let int ~loc = makeId loc "int"

  let bool ~loc = makeId loc "bool"

  let string ~loc = makeId loc "string"

  let real ~loc = makeId loc "real"

  let int16 ~loc = makeId loc "int16"

  let fix16 ~loc = makeId loc "fix16"

  let num loc = { tx = TEOption [ real ~loc; int ~loc; int16 ~loc; fix16 ~loc ]; loc; const = const () }

  let numstr loc = { tx = TEOption [ real ~loc; int ~loc; int16 ~loc; fix16 ~loc; string ~loc ]; loc; const = const () }

  let num_bool loc = { tx = TEOption [ real ~loc; int ~loc; int16 ~loc; fix16 ~loc; bool ~loc ]; loc; const = const () }

  let size ?(loc = Loc.default) n = { tx = TESize n; loc; const = const () }

  let array ?(fixed = true) ?(loc = Loc.default) ?(size = unbound loc) t =
    let a_dim = { tx = TEComposed ("array", [ t; size ]); loc; const = const () } in
    if fixed then
      a_dim
    else
      let a = { tx = TEComposed ("array", [ t ]); loc; const = const () } in
      { tx = TEOption [ a; a_dim ]; loc; const = const () }


  let tuple ?(loc = Loc.default) l = { tx = TEComposed ("tuple", l); loc; const = const () }

  let list ?(loc = Loc.default) t = { tx = TEComposed ("list", [ t ]); loc; const = const () }

  (* Type for indexable collections: arrays and lists *)
  let indexable ?(loc = Loc.default) t =
    let a = { tx = TEComposed ("array", [ t ]); loc; const = const () } in
    let a_dim = { tx = TEComposed ("array", [ t; unbound loc ]); loc; const = const () } in
    let l = { tx = TEComposed ("list", [ t ]); loc; const = const () } in
    { tx = TEOption [ a; a_dim; l ]; loc; const = const () }


  let freal_type ?(loc = Loc.default) () = { tx = TEOption [ real ~loc; fix16 ~loc ]; loc; const = const () }

  let array_size () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array ~fixed:false a in
    [ a_array ], int ~loc


  (* List functions *)
  let list_size () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_list = list a in
    [ a_list ], int ~loc


  let list_append () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_list = list a in
    [ a_list; a ], unit ~loc


  let list_insert () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_list = list a in
    [ a_list; int ~loc; a ], unit ~loc


  let list_remove () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_list = list a in
    [ a_list; int ~loc ], unit ~loc


  let list_clear () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_list = list a in
    [ a_list ], unit ~loc


  let list_reserve () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_list = list a in
    [ a_list; int ~loc ], unit ~loc


  let list_capacity () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_list = list a in
    [ a_list ], int ~loc


  let list_get () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_list = list a in
    [ a_list; int ~loc ], a


  let list_set () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_list = list a in
    [ a_list; int ~loc; a ], unit ~loc


  let str_length () : fun_type =
    let loc = Loc.default in
    [ string ~loc ], int ~loc


  let array_make () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array a in
    [ int ~loc; a ], a_array


  let wrap_array () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let array_type = array a in
    [ array_type ], array_type


  let freal_freal () : fun_type =
    let t = freal_type () in
    [ t ], t


  let real_real_real () : fun_type =
    let loc = Loc.default in
    let t = real ~loc in
    [ t; t ], t


  let clip () : fun_type =
    let loc = Loc.default in
    let t = unbound loc in
    [ t; t; t ], t


  let valid_int () : fun_type =
    let loc = Loc.default in
    [ { tx = TEOption [ real ~loc; int ~loc; int16 ~loc; fix16 ~loc; bool ~loc ]; loc; const = const () } ], int ~loc


  let valid_real () : fun_type =
    let loc = Loc.default in
    [ { tx = TEOption [ real ~loc; int ~loc; int16 ~loc; fix16 ~loc; bool ~loc ]; loc; const = const () } ], real ~loc


  let valid_fix16 () : fun_type =
    let loc = Loc.default in
    [ { tx = TEOption [ real ~loc; int ~loc; int16 ~loc; fix16 ~loc; bool ~loc ]; loc; const = const () } ], fix16 ~loc


  let valid_int16 () : fun_type =
    let loc = Loc.default in
    [ { tx = TEOption [ real ~loc; int ~loc; int16 ~loc; fix16 ~loc; bool ~loc ]; loc; const = const () } ], int16 ~loc


  let valid_bool () : fun_type =
    let loc = Loc.default in
    [ { tx = TEOption [ real ~loc; int ~loc; int16 ~loc; fix16 ~loc; bool ~loc ]; loc; const = const () } ], bool ~loc


  let valid_string () : fun_type =
    let loc = Loc.default in
    ( [ { tx = TEOption [ real ~loc; int ~loc; int16 ~loc; fix16 ~loc; bool ~loc; string ~loc ]; loc; const = const () }
      ]
    , string ~loc )


  let num_num () : fun_type =
    let loc = Loc.default in
    let t = num loc in
    [ t ], t


  let num_num_num () : fun_type =
    let loc = Loc.default in
    let t = num loc in
    [ t; t ], t


  let numstr_numstr_numstr () : fun_type =
    let loc = Loc.default in
    let t = numstr loc in
    [ t; t ], t


  let int_int_int () : fun_type =
    let loc = Loc.default in
    let t = int ~loc in
    [ t; t ], t


  let num_num_bool () : fun_type =
    let loc = Loc.default in
    let t = num loc in
    [ t; t ], bool ~loc


  let a_a_bool () : fun_type =
    let loc = Loc.default in
    let t = unbound loc in
    [ t; t ], bool ~loc


  let bool_bool () : fun_type =
    let loc = Loc.default in
    let t = bool ~loc in
    [ t ], t


  let bool_bool_bool () : fun_type =
    let loc = Loc.default in
    let t = bool ~loc in
    [ t; t ], t


  let unit_int () : fun_type =
    let loc = Loc.default in
    [], int ~loc


  let unit_real () : fun_type =
    let loc = Loc.default in
    [], real ~loc


  let unit_freal () : fun_type =
    let loc = Loc.default in
    [], freal_type ~loc ()


  let string_string () : fun_type =
    let loc = Loc.default in
    let t = string ~loc in
    [ t; t ], t
end

let rec setConstness (c : constness) (v : bool) =
  match c.c with
  | TEConst i | TEMut i ->
    c.c <-
      (if v then
         TEConst i
       else
         TEMut i)
  | TECLink c -> setConstness c v


let setTypeMut (t : type_) = setConstness t.const false

let setTypeConstness (t : type_) v = setConstness t.const v

let isTypeConst (t : type_) =
  let rec loop const =
    match const.c with
    | TEConst _ -> true
    | TEMut _ -> false
    | TECLink c -> loop c
  in
  loop t.const


let rec refreshConstness (t : type_) =
  let t =
    match t.tx with
    | TELink t -> refreshConstness t
    | TEOption options -> { t with tx = TEOption (CCList.map refreshConstness options) }
    | TEComposed (name, subs) -> { t with tx = TEComposed (name, CCList.map refreshConstness subs) }
    | _ -> t
  in
  { t with const = C.const () }


(* Create fresh copies of types with new mutable cells to avoid type constraint sharing *)
let rec copy_type (t : type_) : type_ =
  match t.tx with
  | TEUnbound _ ->
    (* Create a completely fresh unbound type *)
    { tx = TEUnbound None; const = C.const (); loc = t.loc }
  | TELink linked_t -> copy_type linked_t
  | TEId path -> { tx = TEId path; const = C.const (); loc = t.loc }
  | TESize size -> { tx = TESize size; const = C.const (); loc = t.loc }
  | TEComposed (name, type_list) ->
    let fresh_type_list = CCList.map copy_type type_list in
    { tx = TEComposed (name, fresh_type_list); const = C.const (); loc = t.loc }
  | TEOption type_list ->
    let fresh_type_list = CCList.map copy_type type_list in
    { tx = TEOption fresh_type_list; const = C.const (); loc = t.loc }
  | TEFunction (arg_types, ret_type) ->
    let fresh_arg_types = CCList.map copy_type arg_types in
    let fresh_ret_type = copy_type ret_type in
    { tx = TEFunction (fresh_arg_types, fresh_ret_type); const = C.const (); loc = t.loc }
  | TENoReturn -> { tx = TENoReturn; const = C.const (); loc = t.loc }


(* Copy types while preserving sharing: if the same original unbound type appears *)
(* in multiple places (e.g., 't in both array('t, 3) and return type 't), they *)
(* will map to the same fresh unbound type in the copy. This is essential for *)
(* generic function specialization to work correctly. *)
module TypeHashtbl = Hashtbl.Make (struct
  type t = type_

  let equal = ( == )

  let hash = Hashtbl.hash
end)

let copy_types_preserving_sharing (types : type_ list) : type_ list =
  let memo = TypeHashtbl.create 16 in
  let rec copy_with_memo (t : type_) : type_ =
    (* Check if we've already copied this exact type object *)
    match TypeHashtbl.find_opt memo t with
    | Some fresh_t -> fresh_t
    | None ->
      let fresh_t =
        match t.tx with
        | TEUnbound _ ->
          (* Create a fresh unbound type and remember it *)
          { tx = TEUnbound None; const = C.const (); loc = t.loc }
        | TELink linked_t ->
          (* For linked types, copy the linked type (but don't memo the link itself) *)
          copy_with_memo linked_t
        | TEId path -> { tx = TEId path; const = C.const (); loc = t.loc }
        | TESize size -> { tx = TESize size; const = C.const (); loc = t.loc }
        | TEComposed (name, type_list) ->
          let fresh_type_list = CCList.map copy_with_memo type_list in
          { tx = TEComposed (name, fresh_type_list); const = C.const (); loc = t.loc }
        | TEOption type_list ->
          let fresh_type_list = CCList.map copy_with_memo type_list in
          { tx = TEOption fresh_type_list; const = C.const (); loc = t.loc }
        | TEFunction (arg_types, ret_type) ->
          let fresh_arg_types = CCList.map copy_with_memo arg_types in
          let fresh_ret_type = copy_with_memo ret_type in
          { tx = TEFunction (fresh_arg_types, fresh_ret_type); const = C.const (); loc = t.loc }
        | TENoReturn -> { tx = TENoReturn; const = C.const (); loc = t.loc }
      in
      (* Only memoize unbound types - these are the ones we need to preserve sharing for *)
      (match t.tx with
      | TEUnbound _ -> TypeHashtbl.add memo t fresh_t
      | _ -> ());
      fresh_t
  in
  CCList.map copy_with_memo types


(* Find all unique unbound types in a list of types, in order of first appearance *)
(* Uses physical identity to track uniqueness *)
let find_unbounds_in_types (types : type_ list) : type_ list =
  let seen = TypeHashtbl.create 16 in
  let result = ref [] in
  let rec walk (t : type_) : unit =
    match t.tx with
    | TEUnbound _ ->
      if not (TypeHashtbl.mem seen t) then (
        TypeHashtbl.add seen t ();
        result := t :: !result)
    | TELink linked_t -> walk linked_t
    | TEComposed (_, type_list) -> CCList.iter walk type_list
    | TEOption type_list -> CCList.iter walk type_list
    | TEFunction (arg_types, ret_type) ->
      CCList.iter walk arg_types;
      walk ret_type
    | TEId _ | TESize _ | TENoReturn -> ()
  in
  CCList.iter walk types;
  CCList.rev !result


(* Copy types while preserving sharing AND return the mapping from original unbounds to fresh unbounds *)
(* This is needed to extract type bindings after unification *)
let copy_types_with_unbound_mapping (types : type_ list) : type_ list * (type_ * type_) list =
  let memo = TypeHashtbl.create 16 in
  let rec copy_with_memo (t : type_) : type_ =
    match TypeHashtbl.find_opt memo t with
    | Some fresh_t -> fresh_t
    | None ->
      let fresh_t =
        match t.tx with
        | TEUnbound _ -> { tx = TEUnbound None; const = C.const (); loc = t.loc }
        | TELink linked_t -> copy_with_memo linked_t
        | TEId path -> { tx = TEId path; const = C.const (); loc = t.loc }
        | TESize size -> { tx = TESize size; const = C.const (); loc = t.loc }
        | TEComposed (name, type_list) ->
          let fresh_type_list = CCList.map copy_with_memo type_list in
          { tx = TEComposed (name, fresh_type_list); const = C.const (); loc = t.loc }
        | TEOption type_list ->
          let fresh_type_list = CCList.map copy_with_memo type_list in
          { tx = TEOption fresh_type_list; const = C.const (); loc = t.loc }
        | TEFunction (arg_types, ret_type) ->
          let fresh_arg_types = CCList.map copy_with_memo arg_types in
          let fresh_ret_type = copy_with_memo ret_type in
          { tx = TEFunction (fresh_arg_types, fresh_ret_type); const = C.const (); loc = t.loc }
        | TENoReturn -> { tx = TENoReturn; const = C.const (); loc = t.loc }
      in
      (match t.tx with
      | TEUnbound _ -> TypeHashtbl.add memo t fresh_t
      | _ -> ());
      fresh_t
  in
  let fresh_types = CCList.map copy_with_memo types in
  (* Extract the mapping as a list of (original, fresh) pairs *)
  let mapping = TypeHashtbl.fold (fun orig fresh acc -> (orig, fresh) :: acc) memo [] in
  fresh_types, mapping

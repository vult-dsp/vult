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

let print_path (p : path) = Syntax.print_path p

(** Print location in format [line:col-line:col] *)
let print_loc (loc : Loc.t) : Pla.t =
  let start_line = Loc.startLine loc in
  let end_line = Loc.endLine loc in
  let start_col = Loc.startColumn loc in
  let end_col = Loc.endColumn loc in
  {%pla|[<#start_line#i>:<#start_col#i>-<#end_line#i>:<#end_col#i>]|}

(* Whether a value can be modified. During inference every type carries a variable that the
   substitution (see Subst) resolves; the final types carry the resolved value. *)
type constness = CVar of int | CConst | CMut

(* The variables (TENoReturn, TEUnbound and TEOption) are identified by their number and are bound
   by the substitution kept during inference (see Subst). *)
type type_d_ =
  | TENoReturn of int (* The type of an expression that does not produce a value yet *)
  | TEUnbound of int
  | TEId of path
  | TESize of int
  | TEOption of int * type_ list (* A variable that can only be bound to one of the alternatives *)
  | TEComposed of string * type_ list
  | TEFunction of type_ list * type_

and type_ = {tx: type_d_; const: constness; loc: Loc.t}

type fun_type = type_ list * type_

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
  | EIndex of {e: exp; index: exp}
  | EArray of exp list
  | ECall of {instance: string option; path: path; args: exp list}
  | EUnOp of string * exp
  | EOp of string * exp * exp
  | EIf of {cond: exp; then_: exp; else_: exp}
  | ETuple of exp list
  | EMember of exp * string
  | ERecord of {elems: (string * exp) list} (* The type of the expression is the record *)
  | EEnum of {name: string; values: (path * int) list}
  (* A value of an enumeration inferred from the type: the enumerations that have it and its index *)
  | EGenCall of
      { instance: string option (* Instance identifier if using inst:func() syntax *)
      ; generic_path: path (* Full path to the generic function for lookup *)
      ; params: call_param list (* The arguments paired with the parameters, in declaration order *) }
  | EGenCompanionCall of
      { instance: string option (* Instance identifier if using inst:func() syntax *)
      ; companion_name: string (* Name of the companion function *)
      ; parent_generic_path: path (* Path to the parent generic function *)
      ; args: exp list (* Processed arguments *) }
  | ETypeIntrinsic of
      { intrinsic: type_intrinsic (* Which intrinsic: typedefault, typemax, typemin *)
      ; type_param: string (* The generic type parameter name, e.g., "t" from 't *) }

(** Type intrinsics that depend on the concrete type during generic instantiation *)
and type_intrinsic =
  | TypeDefault (* typedefault('t) - default value for the type *)
  | TypeMax (* typemax('t) - maximum value for the type *)
  | TypeMin (* typemin('t) - minimum value for the type *)

and exp = {e: exp_d; loc: Loc.t; t: type_}

(* An argument of a call to a generic function paired with the parameter it is given to *)
and call_param =
  | CallArg of {arg: arg; value: exp} (* A regular argument *)
  | CallGeneric of {param: generic_param; value: exp}
(* An explicit generic parameter *)

and generic_param =
  | GParamFunction of string * type_ option (* Function type using TEFunction *)
  | GParamType of string
  | GParamConstant of string * type_ * Loc.t

and arg = {name: string; t: type_; loc: Loc.t}

and lexp_d = LWild | LId of string | LMember of lexp * string | LIndex of {e: lexp; index: exp} | LTuple of lexp list

and lexp = {l: lexp_d; loc: Loc.t; t: type_}

type dexp_d = DWild | DId of string * int option | DTuple of dexp list

and dexp = {d: dexp_d; loc: Loc.t; t: type_}

and stmt_d =
  | StmtVal of dexp
  | StmtMem of dexp * tag list
  | StmtBind of lexp * exp
  | StmtReturn of exp
  | StmtBlock of stmt list
  | StmtIf of exp * stmt * stmt option
  | StmtWhile of exp * stmt

and stmt = {s: stmt_d; loc: Loc.t}

and function_def =
  { name: path
  ; args: arg list
  ; t: type_ list * type_
  ; next: (function_def * stmt) option
  ; loc: Loc.t
  ; tags: tag list
  ; is_root: bool }

type top_stmt_d =
  | TopExternal of function_def * string option
  | TopFunction of function_def * stmt
  | TopGenericPlaceholder of string (* Marks where specializations of a generic function should be inserted *)
  | TopType of {path: path; members: (string * type_ * Ptags.tags * Loc.t) list}
  | TopAlias of {path: path; alias_of: path}
  | TopEnum of {path: path; members: (string * Loc.t) list}
  | TopConstant of path * int option * type_ * exp * Ptags.tags option

and top_stmt = {top: top_stmt_d; loc: Loc.t}

type program = top_stmt list

(* A parameter of a generic function. Regular arguments and explicit generic parameters can be
   interleaved; the implicit type parameters are not listed. *)
type param_slot = SlotArg of arg | SlotGeneric of generic_param

type generic_function =
  { name: string
  ; args: arg list (* The regular arguments, the same values listed in [params] *)
  ; params: param_slot list (* The parameters in declaration order *)
  ; t: fun_type (* Keep as fun_type for function definitions *)
  ; type_param_map: (string * type_) list (* Type parameter name -> shared unbound cell used in [t] *)
  ; body: Syntax.stmt (* Store the unprocessed body *)
  ; next: Syntax.function_def option (* Companion 'and' functions *)
  ; loc: Loc.t
  ; tags: tag list
  ; type_index: int (* Index for type ordering - captured at definition time *) }

let callValue (param : call_param) : exp = match param with CallArg {value; _} | CallGeneric {value; _} -> value

(* The values of the arguments of a generic call, in declaration order *)
let callValues (params : call_param list) : exp list = CCList.map callValue params

(* The values given to the regular arguments of a generic call *)
let callArgs (params : call_param list) : exp list =
  CCList.filter_map (function CallArg {value; _} -> Some value | CallGeneric _ -> None) params

(* The values given to the explicit generic parameters of a generic call *)
let callExplicitArgs (params : call_param list) : exp list =
  CCList.filter_map (function CallGeneric {value; _} -> Some value | CallArg _ -> None) params

let mapCallValues (f : exp -> exp) (params : call_param list) : call_param list =
  CCList.map
    (function
      | CallArg {arg; value} ->
          CallArg {arg; value= f value}
      | CallGeneric {param; value} ->
          CallGeneric {param; value= f value} )
    params

let print_constness (c : constness) =
  match c with CVar i -> {%pla|const<#i#i> |} | CConst -> Pla.string "const " | CMut -> Pla.string "mut "

let rec print_type_ ?(detailed = false) (t : type_) : Pla.t =
  let prefix pt = if detailed || print_exp_types then Pla.append (print_constness t.const) pt else pt in
  match t.tx with
  | TENoReturn _ ->
      Pla.string "noreturn"
  | TEUnbound i ->
      if detailed then {%pla|_<#i#i>|} else Pla.string "_"
  | TEId p ->
      prefix @@ print_path p
  | TESize n ->
      Pla.int n
  | TEOption (_, alt) ->
      prefix @@ Pla.parenthesize @@ Pla.map_sep (Pla.string "|") print_type_ alt
  | TEComposed (name, elems) ->
      let elems = Pla.map_sep Pla.commaspace (print_type_ ~detailed) elems in
      prefix {%pla|<#name#s>(<#elems#>)|}
  | TEFunction (args, ret) ->
      let args = Pla.map_sep Pla.commaspace (print_type_ ~detailed) args in
      let ret = print_type_ ~detailed ret in
      prefix {%pla|<#args#> -> <#ret#>|}

let rec print_exp ?(locs = false) (e : exp) =
  (fun es ->
    let with_type =
      if print_exp_types then
        let t = print_type_ e.t in
        {%pla|(<#es#> : <#t#>)|}
      else es
    in
    if locs then
      let loc = print_loc e.loc in
      {%pla|<#with_type#><#loc#>|}
    else with_type )
  @@
  match e.e with
  | EUnit ->
      Pla.string "()"
  | EBool v ->
      Pla.string (if v then "true" else "false")
  | EInt n ->
      Pla.int n
  | EReal n ->
      Pla.float n
  | EFixed n ->
      {%pla|<#n#f>x|}
  | EString s ->
      Pla.string_quoted s
  | EId id ->
      Pla.string id
  | EConst p ->
      print_path p
  | EIndex {e; index} ->
      let e = (print_exp ~locs) e in
      let index = (print_exp ~locs) index in
      {%pla|<#e#>[<#index#>]|}
  | EArray l ->
      Pla.wrap (Pla.string "[ ") (Pla.string " ]") (Pla.map_sep Pla.commaspace (print_exp ~locs) l)
  | ECall {instance; path; args} ->
      let instance = Option.value (Option.map (fun s -> {%pla|<#s#s>:|}) instance) ~default:Pla.unit in
      let path = print_path path in
      let args = Pla.map_sep Pla.commaspace (print_exp ~locs) args in
      {%pla|<#instance#><#path#>(<#args#>)|}
  | EUnOp (op, e) ->
      let e = (print_exp ~locs) e in
      {%pla|(<#op#s><#e#>)|}
  | EOp (op, e1, e2) ->
      let e1 = (print_exp ~locs) e1 in
      let e2 = (print_exp ~locs) e2 in
      {%pla|(<#e1#> <#op#s> <#e2#>)|}
  | EIf {cond; then_; else_} ->
      let cond = (print_exp ~locs) cond in
      let then_ = (print_exp ~locs) then_ in
      let else_ = (print_exp ~locs) else_ in
      {%pla|(if <#cond#> then <#then_#> else <#else_#>)|}
  | ETuple l ->
      let l = Pla.map_sep Pla.commaspace (print_exp ~locs) l in
      {%pla|(<#l#>)|}
  | EMember (e, m) ->
      let e = (print_exp ~locs) e in
      {%pla|<#e#>.<#m#s>|}
  | EEnum {name; _} ->
      Pla.string name
  | ERecord {elems} ->
      let printElem (id, v) =
        let v = (print_exp ~locs) v in
        {%pla|<#id#s> = <#v#>|}
      in
      let path = match e.t.tx with TEId path -> print_path path | _ -> Pla.string "_" in
      let elems = Pla.map_sep Pla.commaspace printElem elems in
      {%pla|<#path#> { <#elems#> }|}
  | EGenCall {instance; generic_path; params} ->
      let args = Pla.map_sep Pla.commaspace (print_exp ~locs) (callValues params) in
      let path = print_path generic_path in
      let inst = match instance with None -> Pla.unit | Some i -> Pla.string (i ^ ":") in
      {%pla|<#inst#><#path#>@generic(<#args#>)|}
  | EGenCompanionCall {instance; companion_name; parent_generic_path; args} ->
      let args = Pla.map_sep Pla.commaspace (print_exp ~locs) args in
      let path = print_path parent_generic_path in
      let inst = match instance with None -> Pla.unit | Some i -> Pla.string (i ^ ":") in
      {%pla|<#inst#><#path#>@companion:<#companion_name#s>(<#args#>)|}
  | ETypeIntrinsic {intrinsic; type_param} ->
      let intrinsic_name =
        match intrinsic with TypeDefault -> "typedefault" | TypeMax -> "typemax" | TypeMin -> "typemin"
      in
      {%pla|<#intrinsic_name#s>('<#type_param#s>)|}

let rec print_lexp ?(locs = false) (e : lexp) =
  (fun es ->
    let with_type =
      if print_exp_types then
        let t = print_type_ e.t in
        {%pla|(<#es#> : <#t#>)|}
      else es
    in
    if locs then
      let loc = print_loc e.loc in
      {%pla|<#with_type#><#loc#>|}
    else with_type )
  @@
  match e.l with
  | LWild ->
      Pla.string "_"
  | LId s ->
      Pla.string s
  | LMember (e, m) ->
      let e = (print_lexp ~locs) e in
      {%pla|<#e#>.<#m#s>|}
  | LIndex {e; index} ->
      let e = (print_lexp ~locs) e in
      let index = (print_exp ~locs) index in
      {%pla|<#e#>[<#index#>]|}
  | LTuple l ->
      let l = Pla.map_sep Pla.commaspace (print_lexp ~locs) l in
      {%pla|(<#l#>)|}

let rec print_dexp ?(locs = false) (e : dexp) =
  let t = print_type_ ~detailed:true e.t in
  let base =
    match e.d with
    | DWild ->
        {%pla|_ : <#t#>|}
    | DId (id, None) ->
        {%pla|<#id#s> : <#t#>|}
    | DId (id, Some dim) ->
        {%pla|<#id#s>[<#dim#i>] : <#t#>|}
    | DTuple l ->
        let l = Pla.map_sep Pla.commaspace (print_dexp ~locs) l in
        {%pla|(<#l#>) : <#t#>|}
  in
  if locs then
    let loc = print_loc e.loc in
    {%pla|<#base#><#loc#>|}
  else base

let rec print_stmt ?(locs = false) s =
  let base =
    match s.s with
    | StmtVal lhs ->
        let lhs = (print_dexp ~locs) lhs in
        {%pla|val <#lhs#>;|}
    | StmtMem (lhs, tags) ->
        let tags = Ptags.print_tags tags in
        let lhs = (print_dexp ~locs) lhs in
        {%pla|mem <#lhs#><#tags#>;|}
    | StmtBind (lhs, rhs) ->
        let lhs = (print_lexp ~locs) lhs in
        let rhs = (print_exp ~locs) rhs in
        {%pla|<#lhs#> = <#rhs#>;|}
    | StmtReturn e ->
        let e = (print_exp ~locs) e in
        {%pla|return <#e#>;|}
    | StmtIf (cond, then_, None) ->
        let e = (print_exp ~locs) cond in
        let then_ = (print_stmt ~locs) then_ in
        {%pla|if (<#e#>) <#then_#>|}
    | StmtIf (cond, then_, Some else_) ->
        let cond = (print_exp ~locs) cond in
        let then_ = (print_stmt ~locs) then_ in
        let else_ = (print_stmt ~locs) else_ in
        {%pla|if (<#cond#>) <#then_#><#>else <#else_#>|}
    | StmtWhile (cond, stmt) ->
        let cond = (print_exp ~locs) cond in
        let stmt = (print_stmt ~locs) stmt in
        {%pla|while (<#cond#>)<#stmt#+>|}
    | StmtBlock stmts ->
        let stmt = Pla.map_sep_all Pla.newline (print_stmt ~locs) stmts in
        {%pla|{<#stmt#+>}|}
  in
  if locs then
    let loc = print_loc s.loc in
    {%pla|<#base#><#loc#>|}
  else base

let print_arg ?(locs = false) (arg : arg) =
  let t = print_type_ ~detailed:true arg.t in
  let name = arg.name in
  let base = {%pla|<#name#s> : <#t#>|} in
  if locs then
    let loc = print_loc arg.loc in
    {%pla|<#base#><#loc#>|}
  else base

(* Variant type for function definition keywords *)
type fun_kind = FunKindFun | FunKindAnd | FunKindExternal

let fun_kind_to_string (kind : fun_kind) : string =
  match kind with FunKindFun -> "fun" | FunKindAnd -> "and" | FunKindExternal -> "external"

let next_kind (kind : fun_kind) : fun_kind =
  match kind with FunKindFun -> FunKindAnd | FunKindAnd -> FunKindAnd | FunKindExternal -> FunKindExternal

let print_body_linkname ?(locs = false) body_linkname =
  match body_linkname with
  | `Body stmt ->
      (print_stmt ~locs) stmt
  | `LinkName name ->
      {%pla| "<#name#s>"|}
  | `NoLinkName ->
      Pla.unit

let rec print_function_def ?(locs = false) (kind : fun_kind) (def : function_def) body_linkname =
  let kind_str = fun_kind_to_string kind in
  let name = print_path def.name in
  let args = Pla.map_sep Pla.commaspace (print_arg ~locs) def.args in
  let tags = Ptags.print_tags def.tags in
  let t = print_type_ ~detailed:true (snd def.t) in
  let body = (print_body_linkname ~locs) body_linkname in
  let next = (print_next_function_def ~locs) kind def.next in
  let def_loc =
    if locs then
      let loc = print_loc def.loc in
      {%pla| #def<#loc#>|}
    else Pla.unit
  in
  {%pla|<#kind_str#s> <#name#>(<#args#>) : <#t#><#def_loc#> <#tags#><#body#><#><#next#>|}

and print_next_function_def ?(locs = false) kind next =
  match next with None -> Pla.unit | Some (def, body) -> (print_function_def ~locs) (next_kind kind) def (`Body body)

let print_record_member (name, t, tags, _) =
  let tags = Ptags.print_tags tags in
  let t = print_type_ t in
  {%pla|<#name#s> : <#t#><#tags#>;|}

let print_enum_member (name, _) = {%pla|<#name#s>|}

let print_top_stmt ?(locs = false) t =
  let base =
    match t.top with
    | TopFunction (def, body) ->
        (print_function_def ~locs) FunKindFun def (`Body body)
    | TopExternal (def, Some linkname) ->
        (print_function_def ~locs) FunKindExternal def (`LinkName linkname)
    | TopExternal (def, None) ->
        (print_function_def ~locs) FunKindExternal def `NoLinkName
    | TopGenericPlaceholder name ->
        {%pla|(* generic placeholder: <#name#s> *)|}
    | TopAlias {path= p; alias_of} ->
        let p = print_path p in
        let alias_of = print_path alias_of in
        {%pla|type <#p#> = <#alias_of#><#>|}
    | TopType {path= p; members} ->
        let p = print_path p in
        let members = Pla.map_sep_all Pla.newline print_record_member members in
        {%pla|type <#p#> {<#members#+>}<#>|}
    | TopEnum {path= p; members} ->
        let p = print_path p in
        let members = Pla.map_sep {%pla|,<#>|} print_enum_member members in
        {%pla|enum <#p#> {<#members#+><#>}<#>|}
    | TopConstant (path, dim, _, e, _) ->
        let path = print_path path in
        let e = (print_exp ~locs) e in
        let dim = match dim with None -> Pla.unit | Some dim -> {%pla|[<#dim#i>]|} in
        {%pla|constant <#path#><#dim#> = <#e#>|}
  in
  if locs then
    let loc = print_loc t.loc in
    {%pla|<#base#> @<#loc#>|}
  else base

let print_prog ?(locs = false) prog = Pla.map_sep_all Pla.newline (print_top_stmt ~locs) prog

(* The numbers of the next type and constness variables. The substitution of the inference carries
   it (see Subst), so the variables of a compilation do not depend on the previous ones. *)
type supply = {next_var: int; next_const: int}

let initial_supply : supply = {next_var= 1; next_const= 1}

(* A value that needs fresh variables. Running it returns the supply that follows them. *)
type 'a fresh = supply -> supply * 'a

module C = struct
  let fresh (s : supply) : supply * int = ({s with next_var= s.next_var + 1}, s.next_var)

  let const (s : supply) : supply * constness = ({s with next_const= s.next_const + 1}, CVar s.next_const)

  (* Runs the makers in order and collects their results *)
  let all (makers : 'a fresh list) (s : supply) : supply * 'a list =
    CCList.fold_left_map (fun s make -> make s) s makers

  let withConst (loc : Loc.t) (tx : type_d_) (s : supply) : supply * type_ =
    let s, const = const s in
    (s, {tx; loc; const})

  let withVar (loc : Loc.t) (tx : int -> type_d_) (s : supply) : supply * type_ =
    let s, n = fresh s in
    withConst loc (tx n) s

  let makeId loc id = withConst loc (TEId {id; n= None; loc})

  let path_t loc path = withConst loc (TEId path)

  let unbound loc = withVar loc (fun n -> TEUnbound n)

  let noreturn loc = withVar loc (fun n -> TENoReturn n)

  let option loc (alternatives : type_ list) = withVar loc (fun n -> TEOption (n, alternatives))

  let optionOf loc (makers : type_ fresh list) (s : supply) : supply * type_ =
    let s, alternatives = all makers s in
    option loc alternatives s

  let unit ~loc = makeId loc "unit"

  let int ~loc = makeId loc "int"

  let bool ~loc = makeId loc "bool"

  let string ~loc = makeId loc "string"

  let real ~loc = makeId loc "real"

  let int16 ~loc = makeId loc "int16"

  let fix16 ~loc = makeId loc "fix16"

  let num loc = optionOf loc [real ~loc; int ~loc; int16 ~loc; fix16 ~loc]

  let numstr loc = optionOf loc [real ~loc; int ~loc; int16 ~loc; fix16 ~loc; string ~loc]

  let num_bool loc = optionOf loc [real ~loc; int ~loc; int16 ~loc; fix16 ~loc; bool ~loc]

  let size ?(loc = Loc.default) n = withConst loc (TESize n)

  (* An array whose size is unknown when [size] is not given. When it is not [fixed], it can also
     be an array without a size. *)
  let array ?(fixed = true) ?(loc = Loc.default) ?size (t : type_) (s : supply) : supply * type_ =
    let s, size = match size with Some size -> (s, size) | None -> unbound loc s in
    let s, a_dim = withConst loc (TEComposed ("array", [t; size])) s in
    if fixed then (s, a_dim)
    else
      let s, a = withConst loc (TEComposed ("array", [t])) s in
      option loc [a; a_dim] s

  let tuple ?(loc = Loc.default) l = withConst loc (TEComposed ("tuple", l))

  let list ?(loc = Loc.default) t = withConst loc (TEComposed ("list", [t]))

  (* Type for indexable collections: arrays and lists *)
  let indexable ?(loc = Loc.default) t (s : supply) : supply * type_ =
    let s, size = unbound loc s in
    optionOf loc
      [ withConst loc (TEComposed ("array", [t]))
      ; withConst loc (TEComposed ("array", [t; size]))
      ; withConst loc (TEComposed ("list", [t])) ]
      s

  let freal_type ?(loc = Loc.default) () = optionOf loc [real ~loc; fix16 ~loc]

  (* The type of a builtin function with arguments and a return of the same type *)
  let same (n_args : int) (make : type_ fresh) (s : supply) : supply * fun_type =
    let s, t = make s in
    (s, (CCList.init n_args (fun _ -> t), t))

  (* The type of a builtin function given the makers of its arguments and its return *)
  let signature (args : type_ fresh list) (ret : type_ fresh) (s : supply) : supply * fun_type =
    let s, args = all args s in
    let s, ret = ret s in
    (s, (args, ret))

  (* A type that is already created *)
  let known (t : type_) : type_ fresh = fun s -> (s, t)

  (* The location of the types of the builtin functions *)
  let loc = Loc.default

  (* A builtin function on a list whose elements have any type, given its other arguments and return *)
  let listFunction (args : type_ -> type_ fresh list) (ret : type_ -> type_ fresh) (s : supply) : supply * fun_type =
    let s, a = unbound loc s in
    let s, a_list = list a s in
    signature (known a_list :: args a) (ret a) s

  let array_size (s : supply) : supply * fun_type =
    let s, a = unbound loc s in
    let s, a_array = array ~fixed:false a s in
    signature [known a_array] (int ~loc) s

  (* List functions *)
  let list_size = listFunction (fun _ -> []) (fun _ -> int ~loc)

  let list_append = listFunction (fun a -> [known a]) (fun _ -> unit ~loc)

  let list_insert = listFunction (fun a -> [int ~loc; known a]) (fun _ -> unit ~loc)

  let list_remove = listFunction (fun _ -> [int ~loc]) (fun _ -> unit ~loc)

  let list_clear = listFunction (fun _ -> []) (fun _ -> unit ~loc)

  let list_reserve = listFunction (fun _ -> [int ~loc]) (fun _ -> unit ~loc)

  let list_capacity = listFunction (fun _ -> []) (fun _ -> int ~loc)

  let list_get = listFunction (fun _ -> [int ~loc]) known

  let list_set = listFunction (fun a -> [int ~loc; known a]) (fun _ -> unit ~loc)

  let str_length = signature [string ~loc] (int ~loc)

  let array_make (s : supply) : supply * fun_type =
    let s, a = unbound loc s in
    let s, a_array = array a s in
    let s, n = int ~loc s in
    (s, ([n; a], a_array))

  let wrap_array (s : supply) : supply * fun_type =
    let s, a = unbound loc s in
    let s, array_type = array a s in
    (s, ([array_type], array_type))

  let freal_freal = same 1 (freal_type ())

  let real_real_real = same 2 (real ~loc)

  let clip = same 3 (unbound loc)

  let valid ret = signature [num_bool loc] ret

  let valid_int = valid (int ~loc)

  let valid_real = valid (real ~loc)

  let valid_fix16 = valid (fix16 ~loc)

  let valid_int16 = valid (int16 ~loc)

  let valid_bool = valid (bool ~loc)

  let valid_string =
    signature [optionOf loc [real ~loc; int ~loc; int16 ~loc; fix16 ~loc; bool ~loc; string ~loc]] (string ~loc)

  let num_num = same 1 (num loc)

  let num_num_num = same 2 (num loc)

  let numstr_numstr_numstr = same 2 (numstr loc)

  let int_int_int = same 2 (int ~loc)

  let a_a_bool (s : supply) : supply * fun_type =
    let s, t = unbound loc s in
    let s, b = bool ~loc s in
    (s, ([t; t], b))

  let bool_bool = same 1 (bool ~loc)

  let bool_bool_bool = same 2 (bool ~loc)

  let unit_int = signature [] (int ~loc)

  let unit_real = signature [] (real ~loc)

  (* VCV Prototype builtins *)
  let int_real = signature [int ~loc] (real ~loc)

  let int_bool = signature [int ~loc] (bool ~loc)

  let string_unit = signature [string ~loc] (unit ~loc)

  let string_string_string = same 2 (string ~loc)

  let int_real_real_real_unit = signature [int ~loc; real ~loc; real ~loc; real ~loc] (unit ~loc)
end

(* Whether a resolved type is constant. A constness that was never resolved is constant. *)
let isTypeConst (t : type_) : bool = match t.const with CConst | CVar _ -> true | CMut -> false

(* ========== Shared Utility Functions ========== *)

(** Converts a Syntax.path to a string. *)
let pathString (p : path) : string = match p with {id; n= None; _} -> id | {id; n= Some n; _} -> n ^ "_" ^ id

(** Converts a resolved type to a mangled name for specialized function names. *)
let rec typeToMangledName (t : type_) : string =
  match t.tx with
  | TEId {id; n= None; _} ->
      id
  | TEId {id; n= Some module_name; _} ->
      module_name ^ "_" ^ id
  | TEFunction (arg_types, ret_type) ->
      let args_str = CCList.map typeToMangledName arg_types |> String.concat "_" in
      let ret_str = typeToMangledName ret_type in
      "fn_" ^ args_str ^ "_to_" ^ ret_str
  | TEComposed (name, type_args) ->
      let args_str = CCList.map typeToMangledName type_args |> String.concat "_" in
      if args_str = "" then name else name ^ "_of_" ^ args_str
  | TEUnbound id ->
      "unbound" ^ string_of_int id
  | TEOption (_, type_list) -> (
    match type_list with
    | [single_type] ->
        typeToMangledName single_type
    | multiple_types -> (
        let non_option_types =
          CCList.filter (fun t -> match t.tx with TEOption _ -> false | _ -> true) multiple_types
        in
        match non_option_types with
        | concrete_type :: _ ->
            typeToMangledName concrete_type
        | [] ->
            "opt_" ^ (CCList.map typeToMangledName type_list |> String.concat "_") ) )
  | TENoReturn _ ->
      "noreturn"
  | TESize i ->
      "size_" ^ string_of_int i

(** Resolves a type intrinsic to a concrete expression for a given resolved type. *)
let resolveTypeIntrinsicInline (intrinsic : type_intrinsic) (concrete_type : type_) (loc : Loc.t) : exp =
  let t = concrete_type in
  match (intrinsic, concrete_type.tx) with
  | TypeDefault, TEId {id= "int"; _} ->
      {e= EInt 0; t; loc}
  | TypeDefault, TEId {id= "int16"; _} ->
      {e= EInt 0; t; loc}
  | TypeDefault, TEId {id= "real"; _} ->
      {e= EReal 0.0; t; loc}
  | TypeDefault, TEId {id= "fix16"; _} ->
      {e= EFixed 0.0; t; loc}
  | TypeDefault, TEId {id= "bool"; _} ->
      {e= EBool false; t; loc}
  | TypeDefault, TEId {id= "string"; _} ->
      {e= EString ""; t; loc}
  | TypeMax, TEId {id= "int"; _} ->
      (* int is 32 bits in the generated code *)
      {e= EInt 2147483647; t; loc}
  | TypeMax, TEId {id= "int16"; _} ->
      {e= EInt 32767; t; loc}
  | TypeMax, TEId {id= "real"; _} ->
      (* real is a 32-bit float in the generated code *)
      {e= EReal 3.40282346638528859812e+38; t; loc}
  | TypeMax, TEId {id= "fix16"; _} ->
      {e= EFixed 32767.99998; t; loc}
  | TypeMax, TEId {id= "bool"; _} ->
      {e= EBool true; t; loc}
  | TypeMin, TEId {id= "int"; _} ->
      (* int is 32 bits in the generated code *)
      {e= EInt (-2147483648); t; loc}
  | TypeMin, TEId {id= "int16"; _} ->
      {e= EInt (-32768); t; loc}
  | TypeMin, TEId {id= "real"; _} ->
      (* Most negative finite 32-bit float (Float.min_float is the smallest positive value) *)
      {e= EReal (-3.40282346638528859812e+38); t; loc}
  | TypeMin, TEId {id= "fix16"; _} ->
      {e= EFixed (-32768.0); t; loc}
  | TypeMin, TEId {id= "bool"; _} ->
      {e= EBool false; t; loc}
  | TypeMax, _ ->
      let type_str = Pla.print (print_type_ concrete_type) in
      Error.raiseError (Printf.sprintf "typemax() is not supported for type '%s'" type_str) loc
  | TypeMin, _ ->
      let type_str = Pla.print (print_type_ concrete_type) in
      Error.raiseError (Printf.sprintf "typemin() is not supported for type '%s'" type_str) loc
  | TypeDefault, _ ->
      let type_str = Pla.print (print_type_ concrete_type) in
      Error.raiseError (Printf.sprintf "typedefault() is not supported for type '%s'" type_str) loc

(** Resolves type intrinsics in an expression tree using the type substitution map. *)
let rec resolveTypeIntrinsicsInExp (type_substitution_map : (string * type_) list) (e : exp) : exp =
  match e.e with
  | ETypeIntrinsic {intrinsic; type_param} -> (
    match CCList.assoc_opt ~eq:String.equal type_param type_substitution_map with
    | Some concrete_type ->
        resolveTypeIntrinsicInline intrinsic concrete_type e.loc
    | None ->
        Error.raiseError (Printf.sprintf "Type parameter '%s' not found in generic bindings" type_param) e.loc )
  | ECall {instance; path; args} ->
      let args = CCList.map (resolveTypeIntrinsicsInExp type_substitution_map) args in
      {e with e= ECall {instance; path; args}}
  | EOp (op, e1, e2) ->
      let e1 = resolveTypeIntrinsicsInExp type_substitution_map e1 in
      let e2 = resolveTypeIntrinsicsInExp type_substitution_map e2 in
      {e with e= EOp (op, e1, e2)}
  | EUnOp (op, e1) ->
      let e1 = resolveTypeIntrinsicsInExp type_substitution_map e1 in
      {e with e= EUnOp (op, e1)}
  | EIf {cond; then_; else_} ->
      let cond = resolveTypeIntrinsicsInExp type_substitution_map cond in
      let then_ = resolveTypeIntrinsicsInExp type_substitution_map then_ in
      let else_ = resolveTypeIntrinsicsInExp type_substitution_map else_ in
      {e with e= EIf {cond; then_; else_}}
  | EIndex {e= arr; index} ->
      let arr = resolveTypeIntrinsicsInExp type_substitution_map arr in
      let index = resolveTypeIntrinsicsInExp type_substitution_map index in
      {e with e= EIndex {e= arr; index}}
  | EArray elems ->
      let elems = CCList.map (resolveTypeIntrinsicsInExp type_substitution_map) elems in
      {e with e= EArray elems}
  | ETuple elems ->
      let elems = CCList.map (resolveTypeIntrinsicsInExp type_substitution_map) elems in
      {e with e= ETuple elems}
  | EMember (e1, m) ->
      let e1 = resolveTypeIntrinsicsInExp type_substitution_map e1 in
      {e with e= EMember (e1, m)}
  | ERecord {elems} ->
      let elems = CCList.map (fun (n, v) -> (n, resolveTypeIntrinsicsInExp type_substitution_map v)) elems in
      {e with e= ERecord {elems}}
  | EGenCall {instance; generic_path; params} ->
      let params = mapCallValues (resolveTypeIntrinsicsInExp type_substitution_map) params in
      {e with e= EGenCall {instance; generic_path; params}}
  | EGenCompanionCall {instance; companion_name; parent_generic_path; args} ->
      let args = CCList.map (resolveTypeIntrinsicsInExp type_substitution_map) args in
      {e with e= EGenCompanionCall {instance; companion_name; parent_generic_path; args}}
  | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ | EConst _ | EEnum _ ->
      e

(** Resolves type intrinsics in a statement tree. *)
and resolveTypeIntrinsicsInStmt (type_substitution_map : (string * type_) list) (s : stmt) : stmt =
  match s.s with
  | StmtVal _ ->
      s
  | StmtMem (_, _) ->
      s
  | StmtBind (lhs, rhs) ->
      let rhs = resolveTypeIntrinsicsInExp type_substitution_map rhs in
      {s with s= StmtBind (lhs, rhs)}
  | StmtReturn e ->
      let e = resolveTypeIntrinsicsInExp type_substitution_map e in
      {s with s= StmtReturn e}
  | StmtIf (cond, then_, else_opt) ->
      let cond = resolveTypeIntrinsicsInExp type_substitution_map cond in
      let then_ = resolveTypeIntrinsicsInStmt type_substitution_map then_ in
      let else_opt = Option.map (resolveTypeIntrinsicsInStmt type_substitution_map) else_opt in
      {s with s= StmtIf (cond, then_, else_opt)}
  | StmtWhile (cond, body) ->
      let cond = resolveTypeIntrinsicsInExp type_substitution_map cond in
      let body = resolveTypeIntrinsicsInStmt type_substitution_map body in
      {s with s= StmtWhile (cond, body)}
  | StmtBlock stmts ->
      let stmts = CCList.map (resolveTypeIntrinsicsInStmt type_substitution_map) stmts in
      {s with s= StmtBlock stmts}

(** Substitutes constant parameter references with their literal values in expressions. *)
let rec substituteConstantsInExp (constant_map : (string * exp) list) (e : exp) : exp =
  match e.e with
  | EId name -> (
    match CCList.assoc_opt ~eq:String.equal name constant_map with
    | Some const_exp ->
        {const_exp with loc= e.loc; t= e.t}
    | None ->
        e )
  | ECall {instance; path; args} ->
      let args = CCList.map (substituteConstantsInExp constant_map) args in
      {e with e= ECall {instance; path; args}}
  | EOp (op, e1, e2) ->
      let e1 = substituteConstantsInExp constant_map e1 in
      let e2 = substituteConstantsInExp constant_map e2 in
      {e with e= EOp (op, e1, e2)}
  | EUnOp (op, e1) ->
      let e1 = substituteConstantsInExp constant_map e1 in
      {e with e= EUnOp (op, e1)}
  | EIf {cond; then_; else_} ->
      let cond = substituteConstantsInExp constant_map cond in
      let then_ = substituteConstantsInExp constant_map then_ in
      let else_ = substituteConstantsInExp constant_map else_ in
      {e with e= EIf {cond; then_; else_}}
  | EIndex {e= arr; index} ->
      let arr = substituteConstantsInExp constant_map arr in
      let index = substituteConstantsInExp constant_map index in
      {e with e= EIndex {e= arr; index}}
  | EArray elems ->
      let elems = CCList.map (substituteConstantsInExp constant_map) elems in
      {e with e= EArray elems}
  | ETuple elems ->
      let elems = CCList.map (substituteConstantsInExp constant_map) elems in
      {e with e= ETuple elems}
  | EMember (e1, m) ->
      let e1 = substituteConstantsInExp constant_map e1 in
      {e with e= EMember (e1, m)}
  | ERecord {elems} ->
      let elems = CCList.map (fun (n, v) -> (n, substituteConstantsInExp constant_map v)) elems in
      {e with e= ERecord {elems}}
  | EGenCall {instance; generic_path; params} ->
      let params = mapCallValues (substituteConstantsInExp constant_map) params in
      {e with e= EGenCall {instance; generic_path; params}}
  | EGenCompanionCall {instance; companion_name; parent_generic_path; args} ->
      let args = CCList.map (substituteConstantsInExp constant_map) args in
      {e with e= EGenCompanionCall {instance; companion_name; parent_generic_path; args}}
  | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EConst _ | ETypeIntrinsic _ | EEnum _ ->
      e

(** Substitutes constant parameter references in a left-hand side expression. *)
and substituteConstantsInLexp (constant_map : (string * exp) list) (l : lexp) : lexp =
  match l.l with
  | LWild ->
      l
  | LId _ ->
      l
  | LMember (e, member_name) ->
      let e = substituteConstantsInLexp constant_map e in
      {l with l= LMember (e, member_name)}
  | LIndex {e; index} ->
      let e = substituteConstantsInLexp constant_map e in
      let index = substituteConstantsInExp constant_map index in
      {l with l= LIndex {e; index}}
  | LTuple lexps ->
      let lexps = CCList.map (substituteConstantsInLexp constant_map) lexps in
      {l with l= LTuple lexps}

(** Substitutes constant parameter references in a statement tree. *)
and substituteConstantsInStmt (constant_map : (string * exp) list) (s : stmt) : stmt =
  match s.s with
  | StmtVal _ ->
      s
  | StmtMem (_, _) ->
      s
  | StmtBind (lhs, rhs) ->
      let lhs = substituteConstantsInLexp constant_map lhs in
      let rhs = substituteConstantsInExp constant_map rhs in
      {s with s= StmtBind (lhs, rhs)}
  | StmtReturn e ->
      let e = substituteConstantsInExp constant_map e in
      {s with s= StmtReturn e}
  | StmtIf (cond, then_, else_opt) ->
      let cond = substituteConstantsInExp constant_map cond in
      let then_ = substituteConstantsInStmt constant_map then_ in
      let else_opt = Option.map (substituteConstantsInStmt constant_map) else_opt in
      {s with s= StmtIf (cond, then_, else_opt)}
  | StmtWhile (cond, body) ->
      let cond = substituteConstantsInExp constant_map cond in
      let body = substituteConstantsInStmt constant_map body in
      {s with s= StmtWhile (cond, body)}
  | StmtBlock stmts ->
      let stmts = CCList.map (substituteConstantsInStmt constant_map) stmts in
      {s with s= StmtBlock stmts}

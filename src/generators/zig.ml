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
module Set = Util.Maps.Set

(* Vult uses the "RefObject" initializer model for Zig (see [Initializer.initializerType]): context
   structures are values that are passed to functions by pointer ([*T]) and initialized in place by
   the generated [<T>_init] functions. This mirrors the C++ backend, except that Zig has no reference
   types, so struct arguments must be turned into pointers explicitly with [&] at the call site and
   struct parameters are declared as [*T]. Field access ([_ctx.field]) and indexing auto-dereference
   the single-level pointer, exactly like C++ references. *)

type state =
  { args: Util.Args.args
  ; mutable ref_params: Set.t (* parameters of the current function passed by pointer *)
  ; mutable mutated: Set.t (* names reassigned in the current function (need [var]) *)
  ; void_funcs: Set.t (* names of functions returning void; their calls need no [_ =] discard *) }

(* The small runtime prelude. The names defined here must match the ones produced by
   [Replacements.Zig.fun_to_fun] and [Replacements.Zig.op_to_fun]. Unused helpers are dropped by
   Zig's lazy analysis, so we can define the full set unconditionally. *)
let runtime =
  {%pla|
const std = @import("std");

inline fn intDiv(a: i32, b: i32) i32 {
    return @divTrunc(a, b);
}
inline fn intMod(a: i32, b: i32) i32 {
    return @rem(a, b);
}
inline fn fmodf(a: f32, b: f32) f32 {
    return @rem(a, b);
}
inline fn int_abs(a: i32) i32 {
    return @intCast(@abs(a));
}

inline fn sinf(a: f32) f32 {
    return @sin(a);
}
inline fn cosf(a: f32) f32 {
    return @cos(a);
}
inline fn tanf(a: f32) f32 {
    return std.math.tan(a);
}
inline fn sinhf(a: f32) f32 {
    return std.math.sinh(a);
}
inline fn coshf(a: f32) f32 {
    return std.math.cosh(a);
}
inline fn tanhf(a: f32) f32 {
    return std.math.tanh(a);
}
inline fn expf(a: f32) f32 {
    return @exp(a);
}
inline fn logf(a: f32) f32 {
    return @log(a);
}
inline fn log10f(a: f32) f32 {
    return @log10(a);
}
inline fn floorf(a: f32) f32 {
    return @floor(a);
}
inline fn fabsf(a: f32) f32 {
    return @abs(a);
}
inline fn sqrtf(a: f32) f32 {
    return @sqrt(a);
}
inline fn powf(a: f32, b: f32) f32 {
    return std.math.pow(f32, a, b);
}

inline fn float_pi() f32 {
    return 3.1415926535897932384;
}
inline fn float_eps() f32 {
    return 1e-18;
}
inline fn float_samplerate() f32 {
    return 44100.0;
}
inline fn float_clip(v: f32, lo: f32, hi: f32) f32 {
    return if (v < lo) lo else if (v > hi) hi else v;
}
inline fn int_clip(v: i32, lo: i32, hi: i32) i32 {
    return if (v < lo) lo else if (v > hi) hi else v;
}

var rand_state: u64 = 88172645463325252;
inline fn next_rand() u64 {
    rand_state ^= rand_state << 13;
    rand_state ^= rand_state >> 7;
    rand_state ^= rand_state << 17;
    return rand_state;
}
inline fn int_random() i32 {
    return @intCast(next_rand() & 0x7fffffff);
}
inline fn float_random() f32 {
    return @as(f32, @floatFromInt(int_random())) / 2147483647.0;
}

inline fn int_to_float(a: i32) f32 {
    return @floatFromInt(a);
}
inline fn float_to_int(a: f32) i32 {
    return @intFromFloat(a);
}
inline fn int_to_bool(a: i32) bool {
    return a != 0;
}
inline fn float_to_bool(a: f32) bool {
    return a != 0.0;
}
inline fn bool_to_float(a: bool) f32 {
    return if (a) 1.0 else 0.0;
}
inline fn int16_to_int(a: i16) i32 {
    return @intCast(a);
}
inline fn int16_to_float(a: i16) f32 {
    return @floatFromInt(a);
}
inline fn int16_to_bool(a: i16) bool {
    return a != 0;
}
inline fn int16_to_int16(a: i16) i16 {
    return a;
}
inline fn int_to_int16(a: i32) i16 {
    return @intCast(if (a < -32768) -32768 else if (a > 32767) 32767 else a);
}
inline fn float_to_int16(a: f32) i16 {
    return int_to_int16(@intFromFloat(a));
}
inline fn bool_to_int16(a: bool) i16 {
    return if (a) 1 else 0;
}

fn now_ns() i128 {
    var ts: std.os.linux.timespec = undefined;
    _ = std.os.linux.clock_gettime(.MONOTONIC, &ts);
    return @as(i128, ts.sec) * 1_000_000_000 + @as(i128, ts.nsec);
}

fn printResult(name: []const u8, ms: f64) void {
    var buf: [256]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{s}\tZig\t{d:.2} ms/s\n", .{ name, ms }) catch return;
    _ = std.os.linux.write(1, s.ptr, s.len);
}

|}

let rec print_type (state : state) (t : type_) =
  match t.t with
  | TVoid _ ->
      Pla.string "void"
  | TInt ->
      Pla.string "i32"
  | TInt16 ->
      Pla.string "i16"
  | TReal ->
      Pla.string "f32"
  | TBool ->
      Pla.string "bool"
  | TFix16 ->
      Pla.string "i32"
  | TString ->
      Pla.string "[]const u8"
  | TEmptyType ->
      Pla.string "*anyopaque"
  | TArray (Some dim, t) ->
      let t = print_type state t in
      {%pla|[<#dim#i>]<#t#>|}
  | TArray (None, t) ->
      let t = print_type state t in
      {%pla|[]<#t#>|}
  | TStruct {path; _} ->
      Pla.string path
  | TTuple l ->
      let l = Pla.map_sep Pla.commaspace (print_type state) l in
      {%pla|struct { <#l#> }|}
  | TList t ->
      let t = print_type state t in
      {%pla|[]<#t#>|}

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
      Pla.string "!="
  | OpLt ->
      Pla.string "<"
  | OpLe ->
      Pla.string "<="
  | OpGt ->
      Pla.string ">"
  | OpGe ->
      Pla.string ">="

let uoperator (op : uoperator) = match op with UOpNeg -> Pla.string "-" | UOpNot -> Pla.string "!"

let rec print_exp (state : state) (e : exp) =
  match e.e with
  | EEmptyValue ->
      Pla.string "undefined"
  | EUnit ->
      Pla.string ""
  | EBool v ->
      Pla.string (if v then "true" else "false")
  | EInt n ->
      {%pla|<#n#i>|}
  | EReal n ->
      let s = Util.Vfloat.to_string n in
      Pla.string s
  | EFixed n ->
      let s = Util.Vfloat.to_string n in
      Pla.string s
  | EString s ->
      Pla.string_quoted s
  | EId id ->
      Pla.string id
  | EIndex {e; index} ->
      let e = print_exp state e in
      let index = print_exp state index in
      {%pla|<#e#>[@intCast(<#index#>)]|}
  | EArray l ->
      let l = Pla.map_sep Pla.commaspace (print_exp state) l in
      {%pla|.{ <#l#> }|}
  | ECall {path= "not"; args= [e1]} ->
      let e1 = print_exp state e1 in
      {%pla|!(<#e1#>)|}
  | ECall {path= "size" | "length"; args= [e1]} ->
      let e1 = print_exp state e1 in
      {%pla|@as(i32, @intCast(<#e1#>.len))|}
  | ECall {path; args} ->
      let args = Pla.map_sep Pla.commaspace (print_arg_exp state) args in
      {%pla|<#path#s>(<#args#>)|}
  | EUnOp (op, e) ->
      let e = print_exp state e in
      let op = uoperator op in
      {%pla|(<#op#><#e#>)|}
  | EOp (op, e1, e2) ->
      let se1 = print_exp state e1 in
      let se2 = print_exp state e2 in
      let op = operator op in
      {%pla|(<#se1#> <#op#> <#se2#>)|}
  | EIf {cond; then_; else_} ->
      let cond = print_exp state cond in
      let then_ = print_exp state then_ in
      let else_ = print_exp state else_ in
      {%pla|(if (<#cond#>) <#then_#> else <#else_#>)|}
  | ETuple l ->
      let l = Pla.map_sep Pla.commaspace (print_exp state) l in
      {%pla|.{ <#l#> }|}
  | EMember (e, m) ->
      let e = print_exp state e in
      {%pla|<#e#>.<#m#s>|}
  | ETMember (e, i) ->
      let e = print_exp state e in
      {%pla|<#e#>[<#i#i>]|}
  | ERecord {elems; _} ->
      let printElem (n, v) =
        let v = print_exp state v in
        {%pla|.<#n#s> = <#v#>|}
      in
      let elems = Pla.map_sep Pla.commaspace printElem elems in
      {%pla|.{ <#elems#> }|}

(* A struct or array argument must be passed as a pointer. If the argument is already a pointer (a
   by-pointer parameter of the current function), pass it as-is; otherwise take its address. *)
and print_arg_exp (state : state) (a : exp) =
  match a.t.t with
  | TStruct _ | TArray _ -> (
    match a.e with
    | EId n when Set.mem n state.ref_params ->
        print_exp state a
    | _ ->
        let inner = print_exp state a in
        {%pla|&<#inner#>|} )
  | _ ->
      print_exp state a

let rec print_lexp (state : state) (e : lexp) =
  match e.l with
  | LWild ->
      Pla.string "_"
  | LId s ->
      Pla.string s
  | LMember (e, m) ->
      let e = print_lexp state e in
      {%pla|<#e#>.<#m#s>|}
  | LIndex {e; index} ->
      let e = print_lexp state e in
      let index = print_exp state index in
      {%pla|<#e#>[@intCast(<#index#>)]|}
  | LTuple _ ->
      failwith "Zig.print_lexp: LTuple not implemented"

let print_dexp (e : dexp) = match e.d with DId (id, _) -> Pla.string id

(* Root variable name of an assignment target *)
let rec lexp_root (l : lexp) =
  match l.l with
  | LId n ->
      Some n
  | LMember (l, _) ->
      lexp_root l
  | LIndex {e; _} ->
      lexp_root e
  | LWild | LTuple _ ->
      None

(* Collect the set of names that are reassigned (roots of bind targets) in a statement *)
let rec collect_mutated acc (s : stmt) =
  match s.s with
  | StmtBind (l, _) -> (
    match lexp_root l with Some n -> Set.add n acc | None -> acc )
  | StmtIf (_, then_, Some else_) ->
      collect_mutated (collect_mutated acc then_) else_
  | StmtIf (_, then_, None) ->
      collect_mutated acc then_
  | StmtWhile (_, body) ->
      collect_mutated acc body
  | StmtBlock l ->
      CCList.fold_left collect_mutated acc l
  | StmtSwitch (_, cases, default) -> (
      let acc = CCList.fold_left (fun a (_, b) -> collect_mutated a b) acc cases in
      match default with Some d -> collect_mutated acc d | None -> acc )
  | StmtDecl _ | StmtReturn _ ->
      acc

(* Collect names whose whole binding is directly reassigned (target is exactly [LId n]). Vult allows
   reassigning value parameters, but Zig parameters are immutable, so such parameters must be shadowed
   by a mutable local. *)
let rec collect_direct_assigned acc (s : stmt) =
  match s.s with
  | StmtBind ({l= LId n; _}, _) ->
      Set.add n acc
  | StmtBind _ ->
      acc
  | StmtIf (_, then_, Some else_) ->
      collect_direct_assigned (collect_direct_assigned acc then_) else_
  | StmtIf (_, then_, None) ->
      collect_direct_assigned acc then_
  | StmtWhile (_, body) ->
      collect_direct_assigned acc body
  | StmtBlock l ->
      CCList.fold_left collect_direct_assigned acc l
  | StmtSwitch (_, cases, default) -> (
      let acc = CCList.fold_left (fun a (_, b) -> collect_direct_assigned a b) acc cases in
      match default with Some d -> collect_direct_assigned acc d | None -> acc )
  | StmtDecl _ | StmtReturn _ ->
      acc

(* Collect the set of identifiers referenced in an expression *)
let rec exp_ids acc (e : exp) =
  match e.e with
  | EId n ->
      Set.add n acc
  | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ ->
      acc
  | EUnOp (_, e) ->
      exp_ids acc e
  | EOp (_, a, b) ->
      exp_ids (exp_ids acc a) b
  | EIndex {e; index} ->
      exp_ids (exp_ids acc e) index
  | EArray l ->
      CCList.fold_left exp_ids acc l
  | ECall {args; _} ->
      CCList.fold_left exp_ids acc args
  | EIf {cond; then_; else_} ->
      exp_ids (exp_ids (exp_ids acc cond) then_) else_
  | ETuple l ->
      CCList.fold_left exp_ids acc l
  | EMember (e, _) ->
      exp_ids acc e
  | ETMember (e, _) ->
      exp_ids acc e
  | ERecord {elems; _} ->
      CCList.fold_left (fun a (_, v) -> exp_ids a v) acc elems

let rec lexp_ids acc (l : lexp) =
  match l.l with
  | LId n ->
      Set.add n acc
  | LWild ->
      acc
  | LMember (l, _) ->
      lexp_ids acc l
  | LIndex {e; index} ->
      exp_ids (lexp_ids acc e) index
  | LTuple l ->
      CCList.fold_left lexp_ids acc l

(* Collect all identifiers used (read or as an assignment target base) in a statement *)
let rec collect_used acc (s : stmt) =
  match s.s with
  | StmtDecl (_, Some e) ->
      exp_ids acc e
  | StmtDecl (_, None) ->
      acc
  | StmtBind (l, e) ->
      exp_ids (lexp_ids acc l) e
  | StmtReturn e ->
      exp_ids acc e
  | StmtIf (c, then_, Some else_) ->
      collect_used (collect_used (exp_ids acc c) then_) else_
  | StmtIf (c, then_, None) ->
      collect_used (exp_ids acc c) then_
  | StmtWhile (c, body) ->
      collect_used (exp_ids acc c) body
  | StmtBlock l ->
      CCList.fold_left collect_used acc l
  | StmtSwitch (e, cases, default) -> (
      let acc = exp_ids acc e in
      let acc = CCList.fold_left (fun a (ce, b) -> collect_used (exp_ids a ce) b) acc cases in
      match default with Some d -> collect_used acc d | None -> acc )

let rec print_stmt (state : state) (s : stmt) =
  match s.s with
  (* Declares and initializes a context structure in place *)
  | StmtDecl (({t= {t= TStruct {path; _}; _}; _} as lhs), None) ->
      let n = print_dexp lhs in
      {%pla|var <#n#>: <#path#s> = undefined;<#><#path#s>_init(&<#n#>);|}
  (* Fills a fixed-size array with a default value *)
  | StmtDecl (({d= DId (n, _); t; _} as _lhs), Some {e= ECall {path= "initializeArray"; args= [def; _]}; _}) ->
      let ty = print_type state t in
      let def = print_exp state def in
      {%pla|var <#n#s>: <#ty#> = undefined;<#>for (&<#n#s>) |elem| {<#>    elem.* = <#def#>;<#>}|}
  (* Declaration without initializer: assigned later, so it must be a [var] *)
  | StmtDecl (({d= DId (n, _); t; _} as _lhs), None) ->
      let ty = print_type state t in
      {%pla|var <#n#s>: <#ty#> = undefined;|}
  | StmtDecl (({d= DId (n, _); t; _} as _lhs), Some rhs) ->
      let kw = if Set.mem n state.mutated then "var" else "const" in
      let ty = print_type state t in
      let rhs = print_exp state rhs in
      {%pla|<#kw#s> <#n#s>: <#ty#> = <#rhs#>;|}
  | StmtBind ({l= LWild; _}, ({e= ECall {path; _}; _} as rhs)) ->
      (* The type of a discarded call expression is [void] regardless of the callee, so we look up the
         callee's real return type to decide whether Zig needs a [_ =] discard. *)
      let r = print_exp state rhs in
      if Set.mem path state.void_funcs then {%pla|<#r#>;|} else {%pla|_ = <#r#>;|}
  | StmtBind ({l= LWild; _}, rhs) -> (
      let r = print_exp state rhs in
      match rhs.t.t with TVoid _ -> {%pla|<#r#>;|} | _ -> {%pla|_ = <#r#>;|} )
  | StmtBind (lhs, rhs) ->
      let lhs = print_lexp state lhs in
      let rhs = print_exp state rhs in
      {%pla|<#lhs#> = <#rhs#>;|}
  | StmtReturn e -> (
    match e.e with
    | EUnit ->
        Pla.string "return;"
    | _ ->
        let e = print_exp state e in
        {%pla|return <#e#>;|} )
  | StmtIf (cond, then_, None) ->
      let cond = print_exp state cond in
      let then_ = print_block state then_ in
      {%pla|if (<#cond#>) <#then_#>|}
  | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp state cond in
      let then_ = print_block state then_ in
      let else_ = print_block state else_ in
      {%pla|if (<#cond#>) <#then_#> else <#else_#>|}
  | StmtWhile (cond, body) ->
      let cond = print_exp state cond in
      let body = print_block state body in
      {%pla|while (<#cond#>) <#body#>|}
  | StmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline (print_stmt state) stmts in
      {%pla|{<#stmts#+>}|}
  | StmtSwitch (e1, cases, default) ->
      let rec build cases =
        match cases with
        | [] -> (
          match default with Some d -> print_block state d | None -> Pla.string "{}" )
        | (ce, body) :: rest ->
            let c = print_exp state e1 in
            let ce = print_exp state ce in
            let b = print_block state body in
            let rest = build rest in
            {%pla|if (<#c#> == <#ce#>) <#b#> else <#rest#>|}
      in
      build cases

and print_block (state : state) (body : stmt) =
  match body.s with
  | StmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline (print_stmt state) stmts in
      {%pla|{<#stmts#+>}|}
  | _ ->
      let stmt = print_stmt state body in
      {%pla|{<#stmt#+><#>}|}

let print_param_named (state : state) (name : string) (p : param) =
  match p.t.t with
  | TStruct {path; _} ->
      {%pla|<#name#s>: *<#path#s>|}
  | TArray (Some dim, elem) ->
      let elem = print_type state elem in
      {%pla|<#name#s>: *[<#dim#i>]<#elem#>|}
  | TArray (None, elem) ->
      let elem = print_type state elem in
      {%pla|<#name#s>: []<#elem#>|}
  | _ ->
      let t = print_type state p.t in
      {%pla|<#name#s>: <#t#>|}

let print_top_stmt (state : state) (t : top_stmt) =
  match t.top with
  | TopFunction (def, body) ->
      (* Set up the per-function analysis context used by [print_arg_exp] and the var/const choice. *)
      let ref_params =
        CCList.fold_left
          (fun acc (p : param) -> match p.t.t with TStruct _ | TArray _ -> Set.add p.name acc | _ -> acc)
          Set.empty def.args
      in
      state.ref_params <- ref_params ;
      state.mutated <- collect_mutated Set.empty body ;
      let direct = collect_direct_assigned Set.empty body in
      let used = collect_used Set.empty body in
      let name = def.name in
      let ret = print_type state (snd def.t) in
      let body_stmts = match body.s with StmtBlock l -> l | _ -> [body] in
      (* A value parameter that is reassigned in the body is shadowed by a mutable local, since Zig
         parameters are immutable. The incoming parameter is renamed with an [_arg] suffix. *)
      let is_shadowed (p : param) =
        Set.mem p.name direct && match p.t.t with TStruct _ | TArray _ -> false | _ -> true
      in
      let args =
        CCList.map
          (fun (p : param) ->
            let arg_name = if is_shadowed p then p.name ^ "_arg" else p.name in
            print_param_named state arg_name p )
          def.args
        |> Pla.join_sep Pla.commaspace
      in
      let prelude =
        CCList.filter_map
          (fun (p : param) ->
            if is_shadowed p then
              let name = p.name in
              let arg_name = p.name ^ "_arg" in
              let ty = print_type state p.t in
              Some {%pla|var <#name#s>: <#ty#> = <#arg_name#s>;|}
            else if Set.mem p.name used then None
            else
              let name = p.name in
              Some {%pla|_ = <#name#s>;|} )
          def.args
      in
      let prelude = Pla.map_sep_all Pla.newline (fun d -> d) prelude in
      let stmts = Pla.map_sep_all Pla.newline (print_stmt state) body_stmts in
      {%pla|fn <#name#s>(<#args#>) <#ret#> {<#prelude#+><#stmts#+>}<#><#>|}
  | TopType {path; members} ->
      let printMember (n, (t : type_), _, _) =
        let ty = print_type state t in
        {%pla|<#n#s>: <#ty#>,|}
      in
      let members = Pla.map_sep_all Pla.newline printMember members in
      {%pla|const <#path#s> = struct {<#members#+>};<#><#>|}
  | TopAlias {path; alias_of} ->
      {%pla|const <#path#s> = <#alias_of#s>;<#><#>|}
  | TopExternal _ ->
      Pla.unit
  | TopConstant (name, _, t, rhs, _) -> (
    match t.t with
    | TArray (_, elem) -> (
        let elem = print_type state elem in
        match rhs.e with
        | EArray l ->
            let l = Pla.map_sep Pla.commaspace (print_exp state) l in
            {%pla|const <#name#s> = [_]<#elem#>{ <#l#> };<#><#>|}
        | _ ->
            let rhs = print_exp state rhs in
            {%pla|const <#name#s> = <#rhs#>;<#><#>|} )
    | _ ->
        let ty = print_type state t in
        let rhs = print_exp state rhs in
        {%pla|const <#name#s>: <#ty#> = <#rhs#>;<#><#>|} )

let getTemplateCode (args : Util.Args.args) =
  match args.template with
  | None ->
      (Pla.unit, Pla.unit)
  | Some "performance" ->
      T_performance.generateZig args
  | Some name ->
      Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "'")

let voidFunctions (stmts : top_stmt list) =
  CCList.fold_left
    (fun acc t ->
      match t.top with
      | TopFunction (def, _) | TopExternal (def, _) -> (
        match (snd def.t).t with TVoid _ -> Set.add def.name acc | _ -> acc )
      | _ ->
          acc )
    Set.empty stmts

let generate (args : Util.Args.args) (stmts : top_stmt list) =
  let state = {args; ref_params= Set.empty; mutated= Set.empty; void_funcs= voidFunctions stmts} in
  let file = Common.setExt ".zig" args.output in
  let code = Pla.map_join (print_top_stmt state) stmts in
  let pre, post = getTemplateCode args in
  let version = Core.Version.version in
  let legend = {%pla|// This code was generated by the Vult compiler <#version#s> https://github.com/vult-dsp/vult|} in
  [({%pla|<#legend#><#runtime#><#pre#><#code#><#post#>|}, file)]

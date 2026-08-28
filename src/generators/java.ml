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

(* Only the runtime helpers used by the generated code are emitted. Each entry
   lists the call names that require the helper. *)
let runtime_helpers : (string list * Pla.t) list =
  [ ( ["clip"]
    , {%pla|
static int clip(int x, int minv, int maxv) {
    if(x > maxv)
        return maxv;
    else if(x < minv)
        return minv;
    else return x;
}

static float clip(float x, float minv, float maxv) {
    if(x > maxv)
        return maxv;
    else if(x < minv)
        return minv;
    else return x;
}
|}
    )
  ; ( ["makeArray"]
    , {%pla|
static int[] makeArray(int size, int init) {
    int a[] = new int[size];
    Arrays.fill(a, init);
    return a;
}

static float[] makeArray(int size, float init) {
    float a[] = new float[size];
    Arrays.fill(a, init);
    return a;
}

static boolean[] makeArray(int size, boolean init) {
    boolean a[] = new boolean[size];
    Arrays.fill(a, init);
    return a;
}
|}
    )
  ; (["not"], {%pla|
static boolean not(boolean x) {
    return !x;
}
|})
  ; (["int_to_float"], {%pla|
static float int_to_float(int x) {
    return (float)x;
}
|})
  ; (["bool_to_float"], {%pla|
static float bool_to_float(boolean x) {
    return x ? 1.0f : 0.0f;
}
|})
  ; (["float_to_int"], {%pla|
static int float_to_int(float x) {
    return (int)x;
}
|})
  ; ( ["int16"]
    , {%pla|
static short int16(int x) {
    return (short)Math.max(-32768, Math.min(32767, x));
}

static short int16(float x) {
    return (short)Math.max(-32768, Math.min(32767, (int)x));
}
|}
    )
  ; (["floor"], {%pla|
static float floor(float x) {
    return (float)Math.floor(x);
}
|})
  ; (["ceil"], {%pla|
static float ceil(float x) {
    return (float)Math.ceil(x);
}
|})
  ; (["asin"], {%pla|
static float asin(float x) {
    return (float)Math.asin(x);
}
|})
  ; (["acos"], {%pla|
static float acos(float x) {
    return (float)Math.acos(x);
}
|})
  ; (["atan"], {%pla|
static float atan(float x) {
    return (float)Math.atan(x);
}
|})
  ; (["atan2"], {%pla|
static float atan2(float y, float x) {
    return (float)Math.atan2(y, x);
}
|})
  ; ( ["min"]
    , {%pla|
static float min(float a, float b) {
    return Math.min(a, b);
}

static int min(int a, int b) {
    return Math.min(a, b);
}
|}
    )
  ; ( ["max"]
    , {%pla|
static float max(float a, float b) {
    return Math.max(a, b);
}

static int max(int a, int b) {
    return Math.max(a, b);
}
|}
    )
  ; (["random"; "irandom"], {%pla|
Random rand = new Random();
|})
  ; (["random"], {%pla|
float random() {
    return rand.nextFloat();
}
|})
  ; (["irandom"], {%pla|
int irandom() {
    return rand.nextInt();
}
|})
  ; ( ["get"]
    , {%pla|
static float get(float[] a, int i) {
    return a[i];
}

static int get(int[] a, int i) {
    return a[i];
}
|}
    )
  ; ( ["set"]
    , {%pla|
static void set(float[] a, int i, float val) {
    a[i] = val;
}

static void set(int[] a, int i, int val) {
    a[i] = val;
}
|}
    )
  ; ( ["wrap_array"]
    , {%pla|
static float[] wrap_array(float x[]) {
    return x;
}

static int[] wrap_array(int x[]) {
    return x;
}
|}
    )
  ; (["cosh"], {%pla|
static float cosh(float x) {
    return (float)Math.cosh(x);
}
|})
  ; (["cos"], {%pla|
static float cos(float x) {
    return (float)Math.cos(x);
}
|})
  ; (["sin"], {%pla|
static float sin(float x) {
    return (float)Math.sin(x);
}
|})
  ; (["sinh"], {%pla|
static float sinh(float x) {
    return (float)Math.sinh(x);
}
|})
  ; (["tan"], {%pla|
static float tan(float x) {
    return (float)Math.tan(x);
}
|})
  ; (["tanh"], {%pla|
static float tanh(float x) {
    return (float)Math.tanh(x);
}
|})
  ; (["sqrt"], {%pla|
static float sqrt(float x) {
    return (float)Math.sqrt(x);
}
|})
  ; (["pow"], {%pla|
static float pow(float x, float y) {
    return (float)Math.pow(x, y);
}
|})
  ; (["exp"], {%pla|
static float exp(float x) {
    return (float)Math.exp(x);
}
|})
  ; (["log"], {%pla|
static float log(float x) {
    return (float)Math.log(x);
}
|})
  ; (["log10"], {%pla|
static float log10(float x) {
    return (float)Math.log10(x);
}
|})
  ; ( ["abs"]
    , {%pla|
static float abs(float x) {
    return Math.abs(x);
}

static int abs(int x) {
    return Math.abs(x);
}
|}
    )
  ; ( ["initializeArray"]
    , {%pla|
static float[] initializeArray(float v, int size) {
    float[] a = new float[size];
    Arrays.fill(a, v);
    return a;
}

static int[] initializeArray(int v, int size) {
    int[] a = new int[size];
    Arrays.fill(a, v);
    return a;
}

static boolean[] initializeArray(boolean v, int size) {
    boolean[] a = new boolean[size];
    Arrays.fill(a, v);
    return a;
}
|}
    )
  ; (["eps"], {%pla|
static float eps() {
    return 1e-18f;
}
|})
  ; (["pi"], {%pla|
static float pi() {
    return 3.1415926535897932384f;
}
|})
  ; (["float_to_fix"], {%pla|
static int float_to_fix(float x) {
    return (int)(x * 65536.0f);
}
|})
  ; (["fix_to_int"], {%pla|
static int fix_to_int(int x) {
    return x >> 16;
}
|})
  ; (["fix_to_float"], {%pla|
static float fix_to_float(int x) {
    return ((float)x) / 65536.0f;
}
|}) ]

let runtime (stmts : prog) =
  let calls = Usage.calledFunctions stmts in
  (* makeArray is not a call in the program: the printer emits it when it
     initializes an array too large to write as a literal (see getInitValue). *)
  let uses_large_array =
    Usage.existsType (fun t -> match t.t with TArray (Some size, _) -> size >= 32 | _ -> false) stmts
  in
  let needed name = Util.Maps.Set.mem name calls || (name = "makeArray" && uses_large_array) in
  let fragments =
    CCList.filter_map (fun (names, code) -> if CCList.exists needed names then Some code else None) runtime_helpers
  in
  Pla.join fragments

let rec typeName (t : type_) : string =
  match t.t with
  | TInt ->
      "int"
  | TInt16 ->
      "int16"
  | TReal ->
      "real"
  | TBool ->
      "bool"
  | TString ->
      "string"
  | TFix16 ->
      "fix16"
  | TArray (_, t) ->
      "array_" ^ typeName t
  | TTuple l ->
      "tuple_" ^ String.concat "_" (List.map typeName l)
  | _ ->
      "obj"

let tupleName (l : type_ list) : string = "_tuple_" ^ String.concat "_" (List.map typeName l) ^ "_"

let rec isValueOrIf (e : exp) =
  match e.e with
  | EUnit | EBool _ | EInt _ | EReal _ | EString _ | EId _ | EMember _ ->
      true
  | EUnOp (_, e) ->
      isValueOrIf e
  | EIf {then_; else_; _} ->
      isValueOrIf then_ && isValueOrIf else_
  | _ ->
      false

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
      Pla.string "&&"
  | OpLor ->
      Pla.string "||"
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

let rec print_type_ (t : type_) =
  match t.t with
  | TEmptyType ->
      Pla.string "Object"
  | TVoid _ ->
      Pla.string "void"
  | TInt ->
      Pla.string "int"
  | TInt16 ->
      Pla.string "short"
  | TReal ->
      Pla.string "float"
  | TBool ->
      Pla.string "boolean"
  | TString ->
      Pla.string "String"
  | TFix16 ->
      Pla.string "int"
  | TTuple l ->
      Pla.string (tupleName l)
  | TArray (Some _, t) ->
      let t = print_type_ t in
      {%pla|<#t#>[]|}
  | TArray (None, t) ->
      let t = print_type_ t in
      {%pla|<#t#>[]|}
  | TList t ->
      let t = print_type_ t in
      {%pla|ArrayList<<#t#>>|}
  | TStruct {path; _} ->
      {%pla|<#path#s>|}

let rec print_exp e =
  match e.e with
  | EEmptyValue ->
      Pla.string "null"
  | EUnit ->
      Pla.string ""
  | EBool v ->
      Pla.string (if v then "true" else "false")
  | EInt n ->
      {%pla|<#n#i>|}
  | EReal n ->
      let n = Util.Vfloat.adapt n in
      {%pla|<#n#f>f|}
  | EFixed n ->
      let n = Common.toFixed n in
      {%pla|<#n#s>|}
  | EString s ->
      Pla.string_quoted s
  | EId id ->
      Pla.string id
  | EIndex {e; index} ->
      let e = print_exp e in
      let index = print_exp index in
      {%pla|<#e#>[<#index#>]|}
  | EArray l ->
      let l = Pla.map_sep Pla.commaspace print_exp l in
      {%pla|new float[]{<#l#>}|}
  | ECall {path= "size"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|<#e1#>.length|}
  | ECall {path= "length"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|<#e1#>.length()|}
  (* List operations *)
  | ECall {path= "list_size"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|<#e1#>.size()|}
  | ECall {path= "list_capacity"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|<#e1#>.size()|}
  | ECall {path= "list_append"; args= [l; v]} ->
      let l = print_exp l in
      let v = print_exp v in
      {%pla|<#l#>.add(<#v#>)|}
  | ECall {path= "list_insert"; args= [l; i; v]} ->
      let l = print_exp l in
      let i = print_exp i in
      let v = print_exp v in
      {%pla|<#l#>.add(<#i#>, <#v#>)|}
  | ECall {path= "list_remove"; args= [l; i]} ->
      let l = print_exp l in
      let i = print_exp i in
      {%pla|<#l#>.remove(<#i#>)|}
  | ECall {path= "list_clear"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|<#e1#>.clear()|}
  | ECall {path= "list_reserve"; args= [l; n]} ->
      let l = print_exp l in
      let n = print_exp n in
      {%pla|<#l#>.ensureCapacity(<#n#>)|}
  | ECall {path= "list_get"; args= [l; i]} ->
      let l = print_exp l in
      let i = print_exp i in
      {%pla|<#l#>.get(<#i#>)|}
  | ECall {path= "list_set"; args= [l; i; v]} ->
      let l = print_exp l in
      let i = print_exp i in
      let v = print_exp v in
      {%pla|<#l#>.set(<#i#>, <#v#>)|}
  | ECall {path= "not"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|!(<#e1#>)|}
  | ECall {path= "real"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|(float)(<#e1#>)|}
  | ECall {path= "int"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|(int)(<#e1#>)|}
  | ECall {path= "int16"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|int16(<#e1#>)|}
  | ECall {path= "bool"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|((<#e1#>) != 0)|}
  | ECall {path; args} ->
      let args = Pla.map_sep Pla.commaspace print_exp args in
      {%pla|<#path#s>(<#args#>)|}
  | EUnOp (op, e) ->
      let e = print_exp e in
      let op = uoperator op in
      {%pla|(<#op#><#e#>)|}
  | EOp (op, e1, e2) ->
      let se1 = print_exp e1 in
      let se2 = print_exp e2 in
      let op = operator op in
      {%pla|(<#se1#> <#op#> <#se2#>)|}
  | EIf {cond; then_; else_} ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      {%pla|(<#cond#> ? <#then_#> : <#else_#>)|}
  | ETuple l ->
      let class_name = match e.t.t with TTuple tl -> tupleName tl | _ -> "_tuple_" in
      let l = Pla.map_sep Pla.commaspace print_exp l in
      {%pla|new <#class_name#s>(<#l#>)|}
  | EMember (e, m) ->
      let e = print_exp e in
      {%pla|<#e#>.<#m#s>|}
  | ETMember (e, i) ->
      let e = print_exp e in
      {%pla|<#e#>.field<#i#i>|}
  | ERecord {path; elems} ->
      let printElem (_, v) =
        let v = print_exp v in
        {%pla|<#v#>|}
      in
      let elems = Pla.map_sep Pla.commaspace printElem elems in
      {%pla|new <#path#s>(<#elems#>)|}

let rec print_lexp e =
  match e.l with
  | LWild ->
      Pla.string "_wild"
  | LId s ->
      Pla.string s
  | LMember (e, m) ->
      let e = print_lexp e in
      {%pla|<#e#>.<#m#s>|}
  | LIndex {e; index} ->
      let e = print_lexp e in
      let index = print_exp index in
      {%pla|<#e#>[<#index#>]|}
  | LTuple _ ->
      failwith "Java:print_lexp LTuple"

let print_dexp (e : dexp) =
  match e.d with DId (id, None) -> {%pla|<#id#s>|} | DId (id, Some dim) -> {%pla|<#id#s>[<#dim#i>]|}

let rec getInitValue (t : type_) =
  match t.t with
  | TInt ->
      Pla.string "0"
  | TInt16 ->
      Pla.string "0"
  | TReal ->
      Pla.string "0.0f"
  | TBool ->
      Pla.string "false"
  | TString ->
      Pla.string "\"\""
  | TFix16 ->
      Pla.string "0"
  | TArray (Some size, elem_t) ->
      let elem_init = getInitValue elem_t in
      let elem_type = print_type_ elem_t in
      if size < 32 then
        let elems = CCList.init size (fun _ -> elem_init) |> Pla.join_sep Pla.comma in
        {%pla|new <#elem_type#>[]{<#elems#>}|}
      else {%pla|makeArray(<#size#i>, <#elem_init#>)|}
  | TStruct {path; _} ->
      {%pla|new <#path#s>()|}
  | TTuple l ->
      let class_name = tupleName l in
      {%pla|new <#class_name#s>()|}
  | _ ->
      Pla.string "null"

let rec print_stmt (s : stmt) =
  match s.s with
  (* Special case for _ctx structures *)
  | StmtDecl (({d= DId ("_ctx", _); t= {t= TStruct _; _}; _} as lhs_dexp), None) ->
      let lhs = print_dexp lhs_dexp in
      let t = print_type_ lhs_dexp.t in
      {%pla|<#t#> <#lhs#> = new <#t#>();|}
  (* Struct allocation *)
  | StmtDecl (({t= {t= TStruct {path; _}; _}; _} as lhs_dexp), None) ->
      let lhs = print_dexp lhs_dexp in
      let t = print_type_ lhs_dexp.t in
      {%pla|<#t#> <#lhs#> = new <#path#s>();|}
  | StmtDecl (lhs_dexp, None) ->
      let lhs = print_dexp lhs_dexp in
      let t = print_type_ lhs_dexp.t in
      let init = getInitValue lhs_dexp.t in
      {%pla|<#t#> <#lhs#> = <#init#>;|}
  | StmtDecl (lhs_dexp, Some rhs) ->
      let lhs = print_dexp lhs_dexp in
      let t = print_type_ lhs_dexp.t in
      let rhs = print_exp rhs in
      {%pla|<#t#> <#lhs#> = <#rhs#>;|}
  | StmtBind ({l= LWild; _}, rhs) ->
      let rhs = print_exp rhs in
      {%pla|<#rhs#>;|}
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
      {%pla|if (<#e#>) {<#then_#+><#>}|}
  | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_stmt then_ in
      let else_ = print_stmt else_ in
      {%pla|if (<#cond#>) {<#then_#+><#>} else {<#else_#+><#>}|}
  | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_stmt stmt in
      {%pla|while (<#cond#>) {<#stmt#+><#>}|}
  | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      {%pla|{<#stmt#+>}|}
  | StmtSwitch (e1, cases, default) -> (
      let if_ =
        CCList.fold_right
          (fun (e2, body) else_ ->
            let cond = C.eeq e1 e2 in
            Some (C.sif cond body else_) )
          cases default
      in
      match if_ with None -> Pla.unit | Some if_ -> print_stmt if_ )

let print_arg ({name; t; _} : param) =
  let t = print_type_ t in
  {%pla|<#t#> <#name#s>|}

let print_function_def ?(force_public = false) ?(is_performance = false) (def : function_def) =
  let name = def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  let ret = print_type_ (snd def.t) in
  let visibility =
    if def.info.is_root || force_public then "public"
    else if is_performance && String.contains name '_' then
      (* Make _alloc and _default functions public for performance template *)
      let suffix = String.sub name (String.rindex name '_') (String.length name - String.rindex name '_') in
      if suffix = "_alloc" || suffix = "_default" then "public" else "private"
    else "private"
  in
  let static_keyword = "" in
  {%pla|<#visibility#s> <#static_keyword#s><#ret#> <#name#s>(<#args#>) {|}

let print_body body =
  match body.s with
  | StmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
      {%pla|<#stmts#+>}|}
  | _ ->
      let stmt = print_stmt body in
      {%pla|<#stmt#+><#>}|}

let print_member (n, (t : type_), _, _) =
  let t = print_type_ t in
  {%pla|public <#t#> <#n#s>;|}

let print_struct_def {path; members; _} =
  let members_decl = Pla.map_sep_all Pla.newline print_member members in
  (* Default constructor *)
  let default_constructor =
    let init =
      Pla.map_sep_all Pla.newline
        (fun (name, t, _, _) ->
          let init_val = getInitValue t in
          {%pla|this.<#name#s> = <#init_val#>;|} )
        members
    in
    {%pla|public <#path#s>() {<#init#+>}|}
  in
  (* Constructor with parameters *)
  let param_constructor =
    let params =
      Pla.map_sep Pla.commaspace
        (fun (name, t, _, _) ->
          let t_str = print_type_ t in
          {%pla|<#t_str#> <#name#s>|} )
        members
    in
    let init = Pla.map_sep_all Pla.newline (fun (name, _, _, _) -> {%pla|this.<#name#s> = <#name#s>;|}) members in
    {%pla|public <#path#s>(<#params#>) {<#init#+>}|}
  in
  {%pla|public static class <#path#s> {<#members_decl#+><#default_constructor#+><#param_constructor#+>}|}

(* Generate type alias as inheritance *)
let print_type_alias alias_name base_name =
  {%pla|public static class <#alias_name#s> extends <#base_name#s> {
    public <#alias_name#s>() { super(); }
}|}

let print_top_stmt (args : Util.Args.args) root_context_types t =
  match t.top with
  | TopFunction (func_def, body) ->
      let is_performance = args.template = Some "performance" in
      let is_root_adjacent =
        match func_def.args with
        | {t= {t= TStruct {path; _}; _}; _} :: _ ->
            List.exists (String.equal path) root_context_types
        | _ ->
            false
      in
      let is_root_alloc =
        match (func_def.args, (snd func_def.t).t) with
        | [], TStruct {path; _} ->
            CCString.suffix ~suf:"_type_alloc" func_def.name && List.exists (String.equal path) root_context_types
        | _ ->
            false
      in
      let force_public = is_performance || is_root_adjacent || is_root_alloc in
      let def = print_function_def ~force_public ~is_performance func_def in
      let name = func_def.name in
      (* Check if this is a type alias allocation function *)
      if String.contains name '_' && CCString.suffix ~suf:"_type_alloc" name && List.length func_def.args > 0 then
        let parts = String.split_on_char '_' name in
        let len = List.length parts in
        if len >= 4 then
          (* This is a type alias allocation function - override body to create correct type *)
          let name_len = String.length name in
          let alias_type = String.sub name 0 (name_len - 6) in
          (* Remove "_alloc" *)
          let body = {%pla|
   return new <#alias_type#s>();
}|} in
          {%pla|<#def#><#body#><#><#>|}
        else
          let body = print_body body in
          {%pla|<#def#><#body#><#><#>|}
      else
        let body = print_body body in
        {%pla|<#def#><#body#><#><#>|}
  | TopExternal _ ->
      Pla.unit
  | TopType descr ->
      print_struct_def descr
  | TopAlias _ ->
      Pla.unit
  | TopConstant (name, _, t, rhs, _) -> (
    match (rhs.e, t.t, args.java_bin_tables) with
    | EArray elems, TArray (_, elem_t), true ->
        let size = List.length elems in
        let elem_type = print_type_ elem_t in
        let buffer_type = match elem_t.t with TReal -> "java.nio.FloatBuffer" | _ -> "java.nio.IntBuffer" in
        {%pla|<#elem_type#>[] <#name#s>;
public void set_<#name#s>(<#buffer_type#s> buffer) {
   <#name#s> = new <#elem_type#>[<#size#i>];
   buffer.get(<#name#s>);
}<#>|}
    | _ ->
        let t = print_type_ t in
        let rhs = print_exp rhs in
        {%pla|public static final <#t#> <#name#s> = <#rhs#>;<#>|} )

(* Extract the base type from a function call in the return statement *)
let extract_base_type_from_call = function
  | {e= ECall {path; _}; _} when CCString.suffix ~suf:"_type_alloc" path ->
      let len = String.length path in
      Some (String.sub path 0 (len - 6))
      (* Remove "_alloc" *)
  | _ ->
      None

(* Collect type aliases needed based on function signatures and bodies *)
let collect_type_aliases stmts =
  let aliases = ref [] in
  let collect_from_stmt stmt =
    match stmt.top with
    | TopFunction (def, body) ->
        let name = def.name in
        (* Look for pattern: *_function_type_alloc that takes arguments (indicating it's a type alias) *)
        if String.contains name '_' && CCString.suffix ~suf:"_type_alloc" name && List.length def.args > 0 then
          let name_len = String.length name in
          let alias_type = String.sub name 0 (name_len - 6) in
          (* Remove "_alloc" *)
          (* Try to extract the base type from the function body *)
          let base_type =
            match body.s with
            | StmtReturn exp -> (
              match extract_base_type_from_call exp with
              | Some base ->
                  base
              | None ->
                  (* Fallback to old logic if we can't parse the body *)
                  let parts = String.split_on_char '_' name in
                  let len = List.length parts in
                  if len >= 4 then
                    let module_parts = CCList.take (len - 3) parts in
                    let module_name = String.concat "_" module_parts in
                    module_name ^ "_process_type"
                  else alias_type ^ "_base_type" )
            | _ ->
                (* Fallback if no return statement found *)
                let parts = String.split_on_char '_' name in
                let len = List.length parts in
                if len >= 4 then
                  let module_parts = CCList.take (len - 3) parts in
                  let module_name = String.concat "_" module_parts in
                  module_name ^ "_process_type"
                else alias_type ^ "_base_type"
          in
          aliases := (alias_type, base_type) :: !aliases
    | _ ->
        ()
  in
  List.iter collect_from_stmt stmts ; !aliases

let binarizeElement (e : exp) : string =
  match e.e with
  | EReal n ->
      Util.Binarize.float_to_bin_string n
  | EFixed n ->
      Util.Binarize.float_to_bin_string n
  | EInt n ->
      Util.Binarize.int_to_bin_string n
  | _ ->
      failwith "Java:binarizeElement: unsupported element type"

let generateTableData (args : Util.Args.args) (stmts : top_stmt list) : (Pla.t * string) list =
  if not args.java_bin_tables then []
  else
    let base =
      match args.output with
      | Some output ->
          Filename.dirname output ^ "/" ^ Filename.basename output
      | None ->
          "output"
    in
    CCList.filter_map
      (fun (stmt : top_stmt) ->
        match stmt.top with
        | TopConstant (name, _, _, {e= EArray elems; _}, _) ->
            let table_name = base ^ "_" ^ name ^ ".table" in
            let binary = String.concat "" (CCList.map binarizeElement elems) in
            Some (Pla.string binary, table_name)
        | _ ->
            None )
      stmts

let collectTupleTypes (stmts : top_stmt list) : type_ list list =
  let tuples = ref [] in
  let add_tuple (l : type_ list) : unit =
    let name = tupleName l in
    if not (List.exists (fun existing -> String.equal (tupleName existing) name) !tuples) then tuples := l :: !tuples
  in
  let rec collect_type (t : type_) : unit =
    match t.t with
    | TTuple l ->
        add_tuple l ; List.iter collect_type l
    | TArray (_, t) ->
        collect_type t
    | TList t ->
        collect_type t
    | _ ->
        ()
  in
  let rec collect_exp (e : exp) : unit =
    collect_type e.t ;
    match e.e with
    | ETuple l ->
        List.iter collect_exp l
    | ECall {args; _} ->
        List.iter collect_exp args
    | EUnOp (_, e) ->
        collect_exp e
    | EOp (_, e1, e2) ->
        collect_exp e1 ; collect_exp e2
    | EIndex {e; index} ->
        collect_exp e ; collect_exp index
    | EArray l ->
        List.iter collect_exp l
    | EIf {cond; then_; else_} ->
        collect_exp cond ; collect_exp then_ ; collect_exp else_
    | EMember (e, _) | ETMember (e, _) ->
        collect_exp e
    | ERecord {elems; _} ->
        List.iter (fun (_, v) -> collect_exp v) elems
    | _ ->
        ()
  in
  let rec collect_lexp (le : lexp) : unit =
    match le.l with
    | LMember (e, _) ->
        collect_lexp e
    | LIndex {e; index} ->
        collect_lexp e ; collect_exp index
    | LTuple l ->
        List.iter collect_lexp l
    | _ ->
        ()
  in
  let rec collect_stmt (s : stmt) : unit =
    match s.s with
    | StmtDecl (d, rhs) ->
        collect_type d.t ; Option.iter collect_exp rhs
    | StmtBind (lhs, rhs) ->
        collect_lexp lhs ; collect_exp rhs
    | StmtReturn e ->
        collect_exp e
    | StmtIf (cond, then_, else_) ->
        collect_exp cond ; collect_stmt then_ ; Option.iter collect_stmt else_
    | StmtWhile (cond, body) ->
        collect_exp cond ; collect_stmt body
    | StmtBlock stmts ->
        List.iter collect_stmt stmts
    | StmtSwitch (e, cases, default) ->
        collect_exp e ;
        List.iter (fun (e, s) -> collect_exp e ; collect_stmt s) cases ;
        Option.iter collect_stmt default
  in
  let collect_top (t : top_stmt) : unit =
    match t.top with
    | TopFunction (def, body) ->
        collect_type (snd def.t) ;
        List.iter (fun (p : param) -> collect_type p.t) def.args ;
        collect_stmt body
    | TopType descr ->
        List.iter (fun (_, t, _, _) -> collect_type t) descr.members
    | TopConstant (_, _, t, rhs, _) ->
        collect_type t ; collect_exp rhs
    | TopExternal _ | TopAlias _ ->
        ()
  in
  List.iter collect_top stmts ; List.rev !tuples

let print_tuple_class (types : type_ list) : Pla.t =
  let class_name = tupleName types in
  let fields =
    Pla.map_sep_all Pla.newline
      (fun (i, t) ->
        let t_str = print_type_ t in
        {%pla|public <#t_str#> field<#i#i>;|} )
      (List.mapi (fun i t -> (i, t)) types)
  in
  let default_init =
    Pla.map_sep_all Pla.newline
      (fun (i, t) ->
        let init = getInitValue t in
        {%pla|this.field<#i#i> = <#init#>;|} )
      (List.mapi (fun i t -> (i, t)) types)
  in
  let params =
    Pla.map_sep Pla.commaspace
      (fun (i, t) ->
        let t_str = print_type_ t in
        {%pla|<#t_str#> field<#i#i>|} )
      (List.mapi (fun i t -> (i, t)) types)
  in
  let param_init =
    Pla.map_sep_all Pla.newline (fun i -> {%pla|this.field<#i#i> = field<#i#i>;|}) (List.mapi (fun i _ -> i) types)
  in
  {%pla|public static class <#class_name#s> {<#fields#+><#>
   public <#class_name#s>() {<#default_init#+><#>   }
   public <#class_name#s>(<#params#>) {<#param_init#+><#>   }
}<#>|}

let print_prog args t =
  let root_context_types =
    List.fold_left
      (fun acc s ->
        match s.top with
        | TopFunction (def, _) when def.info.is_root -> (
          match def.args with
          | {t= {t= TStruct {path; _}; _}; _} :: _ ->
              if List.exists (String.equal path) acc then acc else path :: acc
          | _ ->
              acc )
        | _ ->
            acc )
      [] t
  in
  let aliases = collect_type_aliases t in
  let tuple_types = collectTupleTypes t in
  let tuple_code = Pla.map_sep_all Pla.newline print_tuple_class tuple_types in
  let main_code = Pla.map_join (print_top_stmt args root_context_types) t in
  let alias_code = Pla.map_sep_all Pla.newline (fun (alias, base) -> print_type_alias alias base) aliases in
  let code = match tuple_types with [] -> main_code | _ -> {%pla|<#tuple_code#><#main_code#>|} in
  if aliases = [] then code else {%pla|<#code#><#alias_code#+>|}

let getTemplateCode (args : Util.Args.args) =
  match args.template with
  | None ->
      (Pla.unit, Pla.unit)
  | Some "performance" ->
      T_performance.generateJava args
  | Some name ->
      Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "'")

let generate (args : Util.Args.args) (stmts : top_stmt list) =
  let file = Common.setExt ".java" args.output in
  let class_name =
    match args.output with
    | Some output ->
        String.capitalize_ascii (Filename.basename (Filename.remove_extension output))
    | None ->
        "VultCode"
  in
  let package_name, external_import =
    match args.java_prefix with
    | Some prefix ->
        let module_name =
          match args.output with
          | Some output ->
              String.lowercase_ascii (Filename.basename (Filename.remove_extension output))
          | None ->
              "vult"
        in
        (prefix ^ "." ^ module_name, {%pla|import <#prefix#s>.external.*;<#>|})
    | None ->
        let name = match args.output_prefix with Some prefix -> prefix | None -> "vult" in
        (name, Pla.unit)
  in
  let code = print_prog args stmts in
  let runtime = runtime stmts in
  let pre, post = getTemplateCode args in
  let table_data = generateTableData args stmts in
  match args.template with
  | Some "performance" ->
      (* For performance template, generate two files: main class and performance test *)
      let main_code =
        {%pla|
package <#package_name#s>;

import java.util.Arrays;
import java.util.Random;
<#external_import#>
public class <#class_name#s> {
<#runtime#>
<#code#>
}
|}
      in
      let perf_file = Common.setExt "Perf.java" args.output in
      let perf_code = {%pla|
package <#package_name#s>;

<#post#>
|} in
      [(main_code, file); (perf_code, perf_file)] @ table_data
  | _ ->
      (* Regular template or no template *)
      let full_code =
        {%pla|
package <#package_name#s>;

import java.util.Arrays;
import java.util.Random;
<#external_import#>
public class <#class_name#s> {
<#runtime#>
<#pre#><#code#><#post#>
}
|}
      in
      [(full_code, file)] @ table_data

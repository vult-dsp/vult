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

let runtime =
  {%pla|

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

static boolean not(boolean x) {
    return !x;
}

static float int_to_float(int x) {
    return (float)x;
}

static int float_to_int(float x) {
    return (int)x;
}

static short int16(int x) {
    return (short)Math.max(-32768, Math.min(32767, x));
}

static short int16(float x) {
    return (short)Math.max(-32768, Math.min(32767, (int)x));
}

static float floor(float x) {
    return (float)Math.floor(x);
}

static Random rand = new Random();

static float random() {
    return rand.nextFloat();
}

static int irandom() {
    return rand.nextInt();
}

static float get(float[] a, int i) {
    return a[i];
}

static void set(float[] a, int i, float val) {
    a[i] = val;
}

static int get(int[] a, int i) {
    return a[i];
}

static void set(int[] a, int i, int val) {
    a[i] = val;
}

static float[] wrap_array(float x[]) {
    return x;
}

static int[] wrap_array(int x[]) {
    return x;
}

static float cosh(float x) {
    return (float)Math.cosh(x);
}

static float cos(float x) {
    return (float)Math.cos(x);
}

static float sin(float x) {
    return (float)Math.sin(x);
}

static float sinh(float x) {
    return (float)Math.sinh(x);
}

static float tan(float x) {
    return (float)Math.tan(x);
}

static float tanh(float x) {
    return (float)Math.tanh(x);
}

static float sqrt(float x) {
    return (float)Math.sqrt(x);
}

static float pow(float x, float y) {
    return (float)Math.pow(x, y);
}

static float exp(float x) {
    return (float)Math.exp(x);
}

static float log(float x) {
    return (float)Math.log(x);
}

static float log10(float x) {
    return (float)Math.log10(x);
}

static float abs(float x) {
    return Math.abs(x);
}

static int abs(int x) {
    return Math.abs(x);
}

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

static float eps() {
    return 1e-18f;
}

static float pi() {
    return 3.1415926535897932384f;
}

|}


let rec isValueOrIf (e : exp) =
  match e.e with
  | EUnit | EBool _ | EInt _ | EReal _ | EString _ | EId _ | EMember _ -> true
  | EUnOp (_, e) -> isValueOrIf e
  | EIf { then_; else_; _ } -> isValueOrIf then_ && isValueOrIf else_
  | _ -> false


let operator (op : operator) =
  match op with
  | OpAdd -> Pla.string "+"
  | OpSub -> Pla.string "-"
  | OpMul -> Pla.string "*"
  | OpDiv -> Pla.string "/"
  | OpMod -> Pla.string "%"
  | OpLand -> Pla.string "&&"
  | OpLor -> Pla.string "||"
  | OpBor -> Pla.string "|"
  | OpBand -> Pla.string "&"
  | OpBxor -> Pla.string "^"
  | OpLsh -> Pla.string "<<"
  | OpRsh -> Pla.string ">>"
  | OpEq -> Pla.string "=="
  | OpNe -> Pla.string "!="
  | OpLt -> Pla.string "<"
  | OpLe -> Pla.string "<="
  | OpGt -> Pla.string ">"
  | OpGe -> Pla.string ">="


let uoperator (op : uoperator) =
  match op with
  | UOpNeg -> Pla.string "-"
  | UOpNot -> Pla.string "!"


let rec print_type_ (t : type_) =
  match t.t with
  | TEmptyType -> Pla.string "Object"
  | TVoid _ -> Pla.string "void"
  | TInt -> Pla.string "int"
  | TInt16 -> Pla.string "short"
  | TReal -> Pla.string "float"
  | TBool -> Pla.string "boolean"
  | TString -> Pla.string "String"
  | TFix16 -> Pla.string "int"
  | TTuple _ ->
    (* Java doesn't have built-in tuples, we'll need custom classes for this *)
    failwith "Tuples not supported in Java generator"
  | TArray (Some _, t) ->
    let t = print_type_ t in
    {%pla|<#t#>[]|}
  | TArray (None, t) ->
    let t = print_type_ t in
    {%pla|<#t#>[]|}
  | TStruct { path; _ } -> {%pla|<#path#s>|}


let rec print_exp e =
  match e.e with
  | EEmptyValue -> Pla.string "null"
  | EUnit -> Pla.string ""
  | EBool v ->
    Pla.string
      (if v then
         "true"
       else
         "false")
  | EInt n -> {%pla|<#n#i>|}
  | EReal n ->
    let n = Util.Vfloat.adapt n in
    {%pla|<#n#f>f|}
  | EFixed n ->
    let n = Common.toFixed n in
    {%pla|<#n#s>|}
  | EString s -> Pla.string_quoted s
  | EId id -> Pla.string id
  | EIndex { e; index } ->
    let e = print_exp e in
    let index = print_exp index in
    {%pla|<#e#>[<#index#>]|}
  | EArray l ->
    let l = Pla.map_sep Pla.commaspace print_exp l in
    {%pla|new float[]{<#l#>}|}
  | ECall { path = "size"; args = [ e1 ] } ->
    let e1 = print_exp e1 in
    {%pla|<#e1#>.length|}
  | ECall { path = "length"; args = [ e1 ] } ->
    let e1 = print_exp e1 in
    {%pla|<#e1#>.length()|}
  | ECall { path = "not"; args = [ e1 ] } ->
    let e1 = print_exp e1 in
    {%pla|!(<#e1#>)|}
  | ECall { path = "real"; args = [ e1 ] } ->
    let e1 = print_exp e1 in
    {%pla|(float)(<#e1#>)|}
  | ECall { path = "int"; args = [ e1 ] } ->
    let e1 = print_exp e1 in
    {%pla|(int)(<#e1#>)|}
  | ECall { path = "int16"; args = [ e1 ] } ->
    let e1 = print_exp e1 in
    {%pla|int16(<#e1#>)|}
  | ECall { path = "bool"; args = [ e1 ] } ->
    let e1 = print_exp e1 in
    {%pla|((<#e1#>) != 0)|}
  | ECall { path; args } ->
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
  | EIf { cond; then_; else_ } ->
    let cond = print_exp cond in
    let then_ = print_exp then_ in
    let else_ = print_exp else_ in
    {%pla|(<#cond#> ? <#then_#> : <#else_#>)|}
  | ETuple _ ->
    (* For tuples, we'd need to create custom tuple classes *)
    failwith "Tuples not supported in Java generator"
  | EMember (e, m) ->
    let e = print_exp e in
    {%pla|<#e#>.<#m#s>|}
  | ETMember (e, i) ->
    let e = print_exp e in
    {%pla|<#e#>.field<#i#i>|}
  | ERecord { path; elems } ->
    let printElem (_, v) =
      let v = print_exp v in
      {%pla|<#v#>|}
    in
    let elems = Pla.map_sep Pla.commaspace printElem elems in
    {%pla|new <#path#s>(<#elems#>)|}


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_wild"
  | LId s -> Pla.string s
  | LMember (e, m) ->
    let e = print_lexp e in
    {%pla|<#e#>.<#m#s>|}
  | LIndex { e; index } ->
    let e = print_lexp e in
    let index = print_exp index in
    {%pla|<#e#>[<#index#>]|}
  | _ -> failwith "Java:print_lexp LTuple"


let print_dexp (e : dexp) =
  match e.d with
  | DId (id, None) -> {%pla|<#id#s>|}
  | DId (id, Some dim) -> {%pla|<#id#s>[<#dim#i>]|}


let rec getInitValue (t : type_) =
  match t.t with
  | TInt -> Pla.string "0"
  | TInt16 -> Pla.string "0"
  | TReal -> Pla.string "0.0f"
  | TBool -> Pla.string "false"
  | TString -> Pla.string "\"\""
  | TFix16 -> Pla.string "0"
  | TArray (Some size, elem_t) ->
    let elem_init = getInitValue elem_t in
    let elem_type = print_type_ elem_t in
    if size < 32 then
      let elems = CCList.init size (fun _ -> elem_init) |> Pla.join_sep Pla.comma in
      {%pla|new <#elem_type#>[]{<#elems#>}|}
    else
      {%pla|makeArray(<#size#i>, <#elem_init#>)|}
  | TStruct { path; _ } -> {%pla|new <#path#s>()|}
  | _ -> Pla.string "null"


let rec print_stmt (s : stmt) =
  match s.s with
  (* Special case for _ctx structures *)
  | StmtDecl (({ d = DId ("_ctx", _); t = { t = TStruct _; _ }; _ } as lhs_dexp), None) ->
    let lhs = print_dexp lhs_dexp in
    let t = print_type_ lhs_dexp.t in
    {%pla|<#t#> <#lhs#> = new <#t#>();|}
  (* Struct allocation *)
  | StmtDecl (({ t = { t = TStruct { path; _ }; _ }; _ } as lhs_dexp), None) ->
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
  | StmtBind ({ l = LWild; _ }, rhs) ->
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
          Some (C.sif cond body else_))
        cases
        default
    in
    match if_ with
    | None -> Pla.unit
    | Some if_ -> print_stmt if_)


let print_arg ({ name; t; _ } : param) =
  let t = print_type_ t in
  {%pla|<#t#> <#name#s>|}


let print_function_def ?(force_public = false) ?(is_performance = false) (def : function_def) =
  let name = def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  let ret = print_type_ (snd def.t) in
  let visibility =
    if def.info.is_root || force_public then
      "public"
    else if is_performance && String.contains name '_' then
      (* Make _alloc and _default functions public for performance template *)
      let suffix = String.sub name (String.rindex name '_') (String.length name - String.rindex name '_') in
      if suffix = "_alloc" || suffix = "_default" then
        "public"
      else
        "private"
    else
      "private"
  in
  let static_keyword =
    if is_performance then
      ""
    else
      "static "
  in
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


let print_struct_def { path; members; _ } =
  let members_decl = Pla.map_sep_all Pla.newline print_member members in
  (* Default constructor *)
  let default_constructor =
    let init =
      Pla.map_sep_all
        Pla.newline
        (fun (name, t, _, _) ->
          let init_val = getInitValue t in
          {%pla|this.<#name#s> = <#init_val#>;|})
        members
    in
    {%pla|public <#path#s>() {<#init#+>}|}
  in
  (* Constructor with parameters *)
  let param_constructor =
    let params =
      Pla.map_sep
        Pla.commaspace
        (fun (name, t, _, _) ->
          let t_str = print_type_ t in
          {%pla|<#t_str#> <#name#s>|})
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


let print_top_stmt (args : Util.Args.args) t =
  match t.top with
  | TopFunction (func_def, body) ->
    let is_performance = args.template = Some "performance" in
    let force_public = is_performance in
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
  | TopExternal _ -> Pla.unit
  | TopType descr -> print_struct_def descr
  | TopAlias _ -> Pla.unit
  | TopConstant (name, _, t, rhs, _) ->
    let t = print_type_ t in
    let rhs = print_exp rhs in
    {%pla|public static final <#t#> <#name#s> = <#rhs#>;<#>|}


(* Extract the base type from a function call in the return statement *)
let extract_base_type_from_call = function
  | { e = ECall { path; _ }; _ } when CCString.suffix ~suf:"_type_alloc" path ->
    let len = String.length path in
    Some (String.sub path 0 (len - 6))
    (* Remove "_alloc" *)
  | _ -> None


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
            | Some base -> base
            | None ->
              (* Fallback to old logic if we can't parse the body *)
              let parts = String.split_on_char '_' name in
              let len = List.length parts in
              if len >= 4 then
                let module_parts = CCList.take (len - 3) parts in
                let module_name = String.concat "_" module_parts in
                module_name ^ "_process_type"
              else
                alias_type ^ "_base_type")
          | _ ->
            (* Fallback if no return statement found *)
            let parts = String.split_on_char '_' name in
            let len = List.length parts in
            if len >= 4 then
              let module_parts = CCList.take (len - 3) parts in
              let module_name = String.concat "_" module_parts in
              module_name ^ "_process_type"
            else
              alias_type ^ "_base_type"
        in
        aliases := (alias_type, base_type) :: !aliases
    | _ -> ()
  in
  List.iter collect_from_stmt stmts;
  !aliases


let print_prog args t =
  let aliases = collect_type_aliases t in
  let main_code = Pla.map_join (print_top_stmt args) t in
  let alias_code = Pla.map_sep_all Pla.newline (fun (alias, base) -> print_type_alias alias base) aliases in
  if aliases = [] then
    main_code
  else
    {%pla|<#main_code#><#alias_code#+>|}


let getTemplateCode (args : Util.Args.args) =
  match args.template with
  | None -> Pla.unit, Pla.unit
  | Some "performance" -> T_performance.generateJava args
  | Some name -> Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "'")


let generate (args : Util.Args.args) (stmts : top_stmt list) =
  let file = Common.setExt ".java" args.output in
  let class_name =
    match args.output with
    | Some output -> String.capitalize_ascii (Filename.basename (Filename.remove_extension output))
    | None -> "VultCode"
  in
  let package_name =
    match args.output_prefix with
    | Some prefix -> prefix
    | None -> "vult"
  in
  let code = print_prog args stmts in
  let pre, post = getTemplateCode args in
  match args.template with
  | Some "performance" ->
    (* For performance template, generate two files: main class and performance test *)
    let main_code =
      {%pla|
package <#package_name#s>;

import java.util.Arrays;
import java.util.Random;

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
    [ main_code, file; perf_code, perf_file ]
  | _ ->
    (* Regular template or no template *)
    let full_code =
      {%pla|
package <#package_name#s>;

import java.util.Arrays;
import java.util.Random;

public class <#class_name#s> {
<#runtime#>
<#pre#><#code#><#post#>
}
|}
    in
    [ full_code, file ]

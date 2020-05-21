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

module Templates = struct
   let none code = [ code, FileKind.ExtOnly "java" ]

   let runtime : Pla.t =
      {pla|
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

int[] makeArray(int size, int init) {
   int a[] = new int[size];
   Arrays.fill(a, init);
   return a;
}

float[] makeArray(int size, float init) {
   float a[] = new float[size];
   Arrays.fill(a, init);
   return a;
}

boolean[] makeArray(int size, boolean init) {
   boolean a[] = new boolean[size];
   Arrays.fill(a, init);
   return a;
}

int int_to_fix16(int a) { return a * 0x00010000; }

int fix16_to_int(int a) { return (a >> 16); }

int fix16_add(int x, int y) { return x + y; }

int fix16_sub(int x, int y) { return x - y; }

int fix16_minus(int x) { return -x; }

float fix16_to_float(int a) { return (float)a / 0x00010000; }
int float_to_fix16(float a) {
   float temp = a * 0x00010000;
   return (int)temp;
}

int fix16_mul(int x, int y) {
   long res = (long)x * (long)y;
   return (int)(res >> 16);
}

int fix16_div(int a, int b) {
   if (b == 0)
      return 0;
   int result = (int)(((long)a << 16) / b);
   return result;
}

static boolean not(boolean x) {
   return !x;
}

static float int_to_float(int x) {
   return (float)x;
}

int float_to_int(float x) {
   return (int)x;
}

float floor(float x) {
   return (float)Math.floor(x);
}

static Random rand = new Random();

float random() {
   return rand.nextFloat();
}

int irandom() {
   return rand.nextInt();
}

float get(float[] a, int i) {
   return a[i];
}

void set(float[] a, int i, float val) {
   a[i] = val;
}

int get(int[] a, int i) {
   return a[i];
}

void set(int[] a, int i, int val) {
   a[i] = val;
}

float[] wrap_array(float x[]) {
   return x;
}

int[] wrap_array(int x[]) {
   return x;
}

float cosh(float x) {
   return (float)Math.cosh(x);
}

float cos(float x) {
   return (float)Math.cos(x);
}

float sin(float x) {
   return (float)Math.sin(x);
}

float sinh(float x) {
   return (float)Math.sinh(x);
}

float tan(float x) {
   return (float)Math.tan(x);
}

float tanh(float x) {
   return (float)Math.tanh(x);
}

float sqrt(float x) {
   return (float)Math.sqrt(x);
}

float pow(float x, float y) {
   return (float)Math.pow(x, y);
}

float exp(float x) {
   return (float)Math.exp(x);
}

|pla}


   let common package_prefix class_name code =
      let package_name = String.lowercase_ascii class_name in
      [ ( {pla|package <#package_prefix#s>.<#package_name#s>;

import java.util.Arrays;
import java.util.Random;
import <#package_prefix#s>.external.*;

public class <#class_name#s> {
<#runtime#>
<#code#>
}|pla}
        , FileKind.ExtOnly "java" )
      ]


   let apply (params : params) (template : string) (code : Pla.t) : (Pla.t * FileKind.t) list =
      let class_name = Filename.basename params.output in
      match template with
      | _ -> common params.prefix class_name code
end

let dot = Pla.map_sep (Pla.string ".") Pla.string

(** Returns true if the expression is simple and does not need parenthesis *)
let rec isSimple (e : cexp) : bool =
   match e with
   | CEInt _
   |CEFloat _
   |CEBool _
   |CEString _
   |CECall _
   |CEIndex _
   |CEVar _ ->
      true
   | CEAccess (e, _) -> isSimple e
   | _ -> false


let replaceFixed name =
   match name with
   | "fix16" -> "int"
   | _ -> name


(** Returns the base type name and a list of its sizes *)
let rec simplifyArray (typ : type_descr) : string * string list =
   match typ with
   | CTSimple name -> replaceFixed name, []
   | CTArray (sub, size) ->
      let name, sub_size = simplifyArray sub in
      name, sub_size @ [ string_of_int size ]


(** Returns the representation of a type description *)
let printTypeDescr (typ : type_descr) : Pla.t =
   let kind, sizes = simplifyArray typ in
   match sizes with
   | [] -> Pla.string kind
   | _ -> {pla|<#kind#s>[]|pla}


let rec getInitValue (descr : type_descr) : Pla.t =
   match descr with
   | CTSimple "int" -> Pla.string "0"
   | CTSimple "fix16" -> Pla.string "0"
   | CTSimple "abstract" -> Pla.string "0"
   | CTSimple "float" -> Pla.string "0.0f"
   | CTSimple "double" -> Pla.string "0.0"
   | CTSimple "real" -> Pla.string "0.0"
   | CTSimple "boolean" -> Pla.string "false"
   | CTSimple "unit" -> Pla.string "0"
   | CTArray (typ, size) ->
      let init = getInitValue typ in
      let typ_t = printTypeDescr typ in
      if size < 32 then
         let elems = CCList.init size (fun _ -> init) |> Pla.join_sep Pla.comma in
         {pla|new <#typ_t#>[]{<#elems#>}|pla}
      else
         {pla|makeArray(<#size#i>,<#init#>)|pla}
   | CTSimple name -> {pla|new <#name#s>()|pla}


(** Used to print declarations and rebindings of lhs variables *)
let printTypeAndName (is_decl : bool) (typ : type_descr) (name : string list) : Pla.t =
   match typ, name with
   | typ, [ name ] ->
      let kind, sizes = simplifyArray typ in
      begin
         match is_decl, sizes with
         (* Simple varible declaration (no sizes) *)
         | true, [] -> {pla|<#kind#s> <#name#s>|pla}
         (* Array declarations (with sizes) *)
         | true, _ ->
            (*let t_sizes = Pla.map_sep Pla.comma Pla.string sizes in*)
            {pla|<#kind#s> <#name#s>[]|pla}
         (* Simple rebinding (no declaration) *)
         | _, _ -> {pla|<#name#s>|pla}
      end
   | _ -> failwith "CodeC.printTypeAndName: invalid input"


(** Used to print assignments of a tuple field to a variable *)
let printLhsExpTuple (var : Pla.t) (is_var : bool) (i : int) (e : clhsexp) : Pla.t =
   match e with
   (* Assigning to a simple variable *)
   | CLId (CTSimple typ, name) ->
      let name_ = dot name in
      if is_var then (* with declaration *)
         {pla|<#typ#s> <#name_#> = <#var#>.field_<#i#i>;|pla}
      else (* with no declaration *)
         {pla|<#name_#> = <#var#>.field_<#i#i>;|pla}
   | CLId (typ, name) ->
      let tdecl = printTypeAndName is_var typ name in
      {pla|<#tdecl#> = <#var#>.field_<#i#i>;|pla}
   | CLWild -> Pla.unit
   | _ -> failwith ("printLhsExpTuple: All other cases should be already covered\n" ^ Code.show_clhsexp e)


(** Returns a template the print the expression *)
let rec printExp (params : params) (e : cexp) : Pla.t =
   match e with
   | CEEmpty -> Pla.unit
   | CEFloat (s, _) -> Pla.string s
   | CEInt n ->
      (* Parenthesize if it has a unary minus *)
      if n < 0 then
         Pla.parenthesize (Pla.int n)
      else
         Pla.int n
   | CEBool v -> Pla.string (if v then "true" else "false")
   | CEString s -> Pla.string_quoted s
   | CEArray (elems, _) ->
      let telems = Pla.map_sep Pla.comma (printExp params) elems in
      {pla|{<#telems#>}|pla}
   | CECall (name, args, _) ->
      let targs = Pla.map_sep Pla.comma (printExp params) args in
      {pla|<#name#s>(<#targs#>)|pla}
   | CEUnOp (op, e, _) ->
      let te = printExp params e in
      {pla|(<#op#s> <#te#>)|pla}
   | CEOp (op, elems, _) ->
      let sop = {pla| <#op#s> |pla} in
      let telems = Pla.map_sep sop (printExp params) elems in
      {pla|(<#telems#>)|pla}
   | CEVar (name, _) -> Pla.string name
   | CEIndex (e, index, _) ->
      let index = printExp params index in
      let e = printExp params e in
      {pla|<#e#>[<#index#>]|pla}
   | CEIf (cond, then_, else_, _) ->
      let tcond = printExp params cond in
      let tthen = printExp params then_ in
      let telse = printExp params else_ in
      {pla|(<#tcond#>?<#tthen#>:<#telse#>)|pla}
   | CETuple (elems, CTSimple name) ->
      let telems = Pla.map_sep Pla.comma (printChField params) elems in
      {pla|new <#name#s>(<#telems#>)|pla}
   | CETuple _ -> failwith "invalid tuple"
   | CEAccess (((CEVar _ | CEAccess _) as e), n) ->
      let e = printExp params e in
      {pla|<#e#>.<#n#s>|pla}
   | CEAccess (e, n) ->
      let e = printExp params e in
      {pla|(<#e#>).<#n#s>|pla}


(** Used to print the elements of a tuple *)

(** Used to print the elements of a tuple *)
and printChField (params : params) ((_name : string), (value : cexp)) =
   let tval = printExp params value in
   {pla|<#tval#>|pla}


(** Prints lhs values with and without declaration *)
and printLhsExp params (is_var : bool) (e : clhsexp) : Pla.t =
   match e with
   | CLId (typ, name) -> printTypeAndName is_var typ name
   (* if it was an '_' do not print anything *)
   | CLWild -> Pla.unit
   | CLIndex (CTSimple typ, [ name ], index) when is_var ->
      let index = printExp params index in
      {pla|<#typ#s> <#name#s>[<#index#>]|pla}
   | CLIndex (typ, name, _) when is_var ->
      let name = dot name in
      let typ, sizes = simplifyArray typ in
      let sizes_t = Pla.map_join (fun i -> {pla|[<#i#s>]|pla}) sizes in
      {pla|<#typ#s> <#name#><#sizes_t#>|pla}
   | CLIndex (CTSimple _, [ name ], index) ->
      let index = printExp params index in
      {pla|<#name#s>[<#index#>]|pla}
   | _ -> failwith "uncovered case"


(** Used to print assignments on to an array element *)
let printArrayBinding params (var : string list) (i : int) (e : cexp) : Pla.t =
   let te = printExp params e in
   let var = dot var in
   {pla|<#var#>[<#i#i>] = <#te#>; |pla}


(** Prints arguments to functions either pass by value or reference *)
let printFunArg (ntype, name) : Pla.t =
   match ntype with
   | Var typ ->
      let tdescr = printTypeDescr typ in
      {pla|<#tdescr#> <#name#s>|pla}
   | Ref (CTArray (typ, _)) ->
      let tdescr = printTypeDescr typ in
      {pla|<#tdescr#> <#name#s>[]|pla}
   | Ref typ ->
      let tdescr = printTypeDescr typ in
      {pla|<#tdescr#> <#name#s>|pla}


let rec isLastReturn stmt =
   match stmt with
   | CSReturn _ -> true
   | CSBlock stmts -> isLastReturnList stmts
   | _ -> false


and isLastReturnList stmts =
   match stmts with
   | [ s ] -> isLastReturn s
   | _ :: t -> isLastReturnList t
   | _ -> false


let rec printSwitchStmt params e cases def =
   let e_t = printExp params e in
   let cases_t =
      Pla.map_sep_all
         Pla.newline
         (fun (v, stmt) ->
             let v_t = printExp params v in
             let stmt_t = CCOpt.get_or ~default:Pla.unit (printStmt params stmt) in
             let break = if isLastReturn stmt then Pla.unit else Pla.string "break;" in
             {pla|case <#v_t#>:<#stmt_t#+><#><#break#>|pla})
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
   (* Strange case '_' *)
   | CSVar (CLWild, None) -> None
   (* Prints type x; *)
   | CSVar (((CLId (tdescr, _) | CLIndex (tdescr, _, _)) as lhs), None) ->
      let tlhs = printLhsExp params true lhs in
      let init = getInitValue tdescr in
      Some {pla|<#tlhs#> = <#init#>; |pla}
   | CSVar (lhs, Some value) ->
      let value_t = printExp params value in
      let tlhs = printLhsExp params true lhs in
      Some {pla|<#tlhs#> = <#value_t#>; |pla}
   (* All other cases of assigning tuples will be wrong *)
   | CSVar (CLTuple _, None) -> failwith "CodeJava.printStmt: invalid tuple assign"
   (* Prints _ = ... *)
   | CSBind (CLWild, value) ->
      let te = printExp params value in
      Some {pla|<#te#>;|pla}
   (* Print (x, y, z) = ... *)
   | CSBind (CLTuple elems, ((CEVar _ | CEAccess _) as rhs)) ->
      let rhs = printExp params rhs in
      let t = List.mapi (printLhsExpTuple rhs false) elems |> Pla.join in
      Some t
   (* All other cases of assigning tuples will be wrong *)
   | CSBind (CLTuple _, _) -> failwith "CodeJava.printStmt: invalid tuple assign"
   (* Prints x = [ ... ] *)
   | CSBind (CLId (_, name), CEArray (elems, _)) ->
      let t = List.mapi (printArrayBinding params name) elems |> Pla.join in
      Some t
   (* Prints x = ... *)
   | CSBind (CLId (_, name), value) ->
      let te = printExp params value in
      let name = dot name in
      Some {pla|<#name#> = <#te#>;|pla}
   | CSBind (CLIndex (_, name, index), value) ->
      let te = printExp params value in
      let name = dot name in
      let index = printExp params index in
      Some {pla|<#name#>[<#index#>] = <#te#>;|pla}
   | CSConst ((CLId (_, name) as lhs), CEArray (elems, _)) ->
      if params.target_file = Header then
         let size = List.length elems in
         let name = dot name in
         let tlhs = printLhsExp params true lhs in
         Some
            {pla|<#tlhs#>;
         public void set_<#name#>(java.nio.FloatBuffer buffer){
            <#name#> = new float[<#size#i>];
            buffer.get(<#name#>);
            }|pla}
      else
         None
   (* Prints const x = ... *)
   | CSConst (lhs, ((CEInt _ | CEFloat _ | CEBool _) as value)) ->
      if params.target_file = Header then
         let tlhs = printLhsExp params true lhs in
         let te = printExp params value in
         Some {pla|static <#tlhs#> = <#te#>;|pla}
      else
         None
   (* All other cases should be errors *)
   | CSConst _ -> failwith "printStmt: invalid constant declaration"
   (* Function declarations cotaining more than one statement *)
   | CSFunction (ntype, name, args, (CSBlock _ as body), attr) ->
      let ret = printTypeDescr ntype in
      let targs = Pla.map_sep Pla.commaspace printFunArg args in
      (* if we are printing a header, skip the body *)
      if params.target_file = Header then
         None
      else
         let scope = if attr.is_root then Pla.string "public" else Pla.string "private" in
         ( match printStmt params body with
           | Some tbody -> Some {pla|<#scope#> <#ret#> <#name#s>(<#targs#>)<#tbody#><#>|pla}
           (* Covers the case when the body is empty *)
           | None -> Some {pla|<#scope#> <#ret#> <#name#s>(<#targs#>){}<#>|pla} )
   (* Function declarations cotaining a single statement *)
   | CSFunction (ntype, name, args, body, attr) ->
      let ret = printTypeDescr ntype in
      let targs = Pla.map_sep Pla.commaspace printFunArg args in
      (* if we are printing a header, skip the body *)
      if params.target_file = Header then
         None
      else
         let scope = if attr.is_root then Pla.string "public" else Pla.string "private" in
         let tbody = CCOpt.get_or ~default:Pla.unit (printStmt params body) in
         Some {pla|<#scope#> <#ret#> <#name#s>(<#targs#>){<#tbody#>}<#>|pla}
   (* Prints return x *)
   | CSReturn e1 ->
      let te = printExp params e1 in
      Some {pla|return <#te#>;|pla}
   (* Printf while(cond) ... *)
   | CSWhile (cond, body) ->
      let tcond = printExp params cond in
      let tcond = if isSimple cond then Pla.parenthesize tcond else tcond in
      let tbody = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
      Some {pla|while<#tcond#><#tbody#>|pla}
   (* Prints a block of statements*)
   | CSBlock elems ->
      let telems = printStmtList params elems in
      Some {pla|{<#telems#+>}|pla}
   (* If-statement without an else*)
   | CSIf (cond, then_, None) ->
      let tcond = printExp params cond in
      let tcond = if isSimple cond then Pla.wrap (Pla.string "(") (Pla.string ")") tcond else tcond in
      let tthen = CCOpt.get_or ~default:Pla.semi (wrapStmtIfNotBlock params then_) in
      Some {pla|if<#tcond#><#tthen#>|pla}
   (* If-statement with else*)
   | CSIf (cond, then_, Some else_) ->
      let tcond = printExp params cond in
      let tcond = if isSimple cond then Pla.wrap (Pla.string "(") (Pla.string ")") tcond else tcond in
      let tthen = CCOpt.get_or ~default:Pla.semi (wrapStmtIfNotBlock params then_) in
      let telse = CCOpt.get_or ~default:Pla.semi (wrapStmtIfNotBlock params else_) in
      Some {pla|if<#tcond#><#tthen#><#>else<#><#telse#>|pla}
   (* Type declaration (only in headers) *)
   | CSType (name, members, attr) when params.target_file = Header ->
      let tmembers =
         Pla.map_sep_all
            Pla.newline
            (fun (typ, name) ->
                let tmember = printTypeAndName true typ [ name ] in
                {pla|public <#tmember#>;|pla})
            members
      in
      let constructor =
         let args =
            Pla.map_sep
               Pla.comma
               (fun (typ, name) ->
                   let tmember = printTypeAndName true typ [ name ] in
                   {pla|<#tmember#>|pla})
               members
         in
         let init = Pla.map_sep_all Pla.newline (fun (_, name) -> {pla|this.<#name#s> = <#name#s>;|pla}) members in
         {pla|<#name#s>(<#args#>){ <#init#> }|pla}
      in
      let constructor_default =
         let init =
            Pla.map_sep_all
               Pla.newline
               (fun (type_, name) ->
                   let value = getInitValue type_ in
                   {pla|this.<#name#s> = <#value#>;|pla})
               members
         in
         {pla|<#name#s>(){ <#init#> }|pla}
      in
      let scope = if attr.is_root then Pla.string "public" else Pla.string "private" in
      Some {pla|<#scope#> class <#name#s> {<#tmembers#+> <#constructor_default#+> <#constructor#+> }<#>|pla}
   (* Do not print type delcarations in implementation file *)
   | CSType (_, _, _) -> None
   (* Type declaration aliases (only in headers) *)
   | CSAlias (_t1, _t2) when params.target_file = Header ->
      (*let tdescr = printTypeDescr t2 in
        Some({pla|class <#tdescr#> extends <#t1#s>{}<#>|pla})*)
      None
   (* Do not print type delcarations in implementation file *)
   | CSAlias (_, _) -> None
   (* External function definitions (only in headers) *)
   | CSExtFunc (_ntype, _name, _args) when params.target_file = Header ->
      (*let ret = printTypeDescr ntype in
        let targs = Pla.map_sep Pla.commaspace printFunArg args in
        Some({pla|extern <#ret#> <#name#s>(<#targs#>);|pla})*)
      None
   (* Do not print external function delcarations in implementation file *)
   | CSExtFunc _ -> None
   | CSEmpty -> None
   | CSSwitch (e, cases, def) -> printSwitchStmt params e cases def


and printStmtList (params : params) (stmts : cstmt list) : Pla.t =
   (* Prints the statements and removes all elements that are None *)
   let tstmts = CCList.filter_map (printStmt params) stmts in
   Pla.map_sep_all Pla.newline (fun a -> a) tstmts


and wrapStmtIfNotBlock params stmt =
   match stmt with
   | CSBlock _ -> printStmt params stmt
   | _ ->
      match printStmt params stmt with
      | Some t -> Some (Pla.wrap (Pla.string "{ ") (Pla.string " }") t)
      | _ -> None


let getBinarizedFloat elem =
   Pla.string
   @@
   match elem with
   | CEFloat (_, n) -> Binarize.float_to_bin_string n
   | CEInt n -> Binarize.float_to_bin_string (float_of_int n)
   | _ -> raise (Invalid_argument "not a numeric value")


let rec generateTableData (params : params) (stmts : cstmt list) =
   match stmts with
   | [] -> []
   | CSConst (CLId (_, name), CEArray (elems, _)) :: rest ->
      let table_name = String.concat "_" name ^ ".table" in
      let telems = Pla.map_join getBinarizedFloat elems in
      (telems, FileKind.ExtOnly table_name) :: generateTableData params rest
   | _ :: rest -> generateTableData params rest


(** Generates the .c and .h file contents for the given parsed files *)
let print (params : params) (stmts : Code.cstmt list) : (Pla.t * FileKind.t) list =
   let h = printStmtList { params with target_file = Header } stmts in
   let cpp = printStmtList { params with target_file = Implementation } stmts in
   let tables = generateTableData params stmts in
   let files = Templates.apply params params.template (Pla.join [ h; cpp ]) in
   tables @ files

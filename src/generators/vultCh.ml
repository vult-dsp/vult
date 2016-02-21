open TypesVult
open PrintBuffer

type arg_type =
   | Ref of string
   | Var of string

type cexp =
   | CEInt    of int
   | CEFloat  of float
   | CEBool   of bool
   | CEString of string
   | CECall   of string * cexp list
   | CEUnOp   of string * cexp
   | CEOp     of string * cexp list
   | CEVar    of string
   | CEIf     of cexp * cexp * cexp
   | CETuple  of (string * cexp) list
   | CECast   of string * cexp
   | CENewObj

type clhsexp =
   | CLWild
   | CLId    of string * string
   | CLTuple of clhsexp list

type cstmt =
   | CSVarDecl  of clhsexp * cexp option
   | CSBind     of clhsexp * cexp
   | CSFunction of string * string * (arg_type * string) list * cstmt
   | CSReturn   of cexp
   | CSWhile    of cexp * cstmt
   | CSBlock    of cstmt list
   | CSIf       of cexp * cstmt * cstmt option
   | CSType     of string * (string * string) list
   | CSAlias    of string * string
   | CSEmpty

type real_type =
   | Float
   | Fixed


module Templates = struct

   type t =
      | None
      | Header
      | Implementation

   let get template =
      match template with
      | "none"   -> None
      | "default" -> None
      | t -> failwith (Printf.sprintf "The template '%s' is not available for this generator" t)

   let none code = code

   let header (name:string) (code:string) : string =
      let file = String.uppercase name in
Printf.sprintf
"#ifndef %s_H
#define %s_H
#include <stdint.h>
#include <math.h>

%s

#endif // %s_H
"
file file code file

   let implementation (file:string) (code:string) : string =
Printf.sprintf
"#include \"%s.h\"

%s
"
file code

   let apply template file code =
      match template with
      | None -> none code
      | Header -> header file code
      | Implementation -> implementation file code

end


type parameters =
   {
      real : real_type;
      template : Templates.t;
      is_header : bool;
      output : string;
   }

let fixKeyword key =
   match key with
   | "default" -> "default_"
   | _ -> key

let rec join (sep:string) (id:string list) : string =
   match id with
   | [] -> ""
   | [ name ] -> fixKeyword name
   | h :: t -> (fixKeyword h) ^ sep ^ (join sep t)

let convertId (id:id) : string =
   match id with
   | [a]   -> fixKeyword a
   | [a;b] -> (fixKeyword a)^"."^(fixKeyword b)
   | _ -> failwith ("VType.convertId: invalid identifier " ^ PrintTypes.identifierStr id)

let underscoreId (id:id) : string =
   join "_" id

let rec convertType params (tp:VType.t) : string =
   match !tp with
   | VType.TId(["real"],_) when params.real = Float -> "float"
   | VType.TId(["real"],_) when params.real = Fixed -> "fixed"
   | VType.TId(["bool"],_) -> "uint8_t"
   | VType.TId(["unit"],_) -> "void"
   | VType.TId(id,_) -> underscoreId id
   | VType.TComposed(["tuple"],_,_) -> VType.getTupleName tp
   | VType.TComposed(_,_,_) ->
      failwith ("VultCh.convertType: unsupported type in c code generation" ^ PrintTypes.typeStr tp)
   | VType.TLink(tp) -> convertType params tp
   | VType.TArrow _
   | VType.TUnbound _
   | VType.TExpAlt _ ->
      failwith ("VultCh.convertType: unsupported type in c code generation" ^ PrintTypes.typeStr tp)

let getCast params (name:id) : string =
   convertType params (ref (VType.TId(name,None)))

let isValue (typ:VType.t) : bool =
   match !(VType.unlink typ) with
   | VType.TId(name,_) when name=["int"] || name=["real"] || name=["bool"] || name=["void"] ->
      true
   | _ -> false

let isReal (typ:VType.t) : bool =
   match !(VType.unlink typ) with
   | VType.TId(name,_) when name=["real"] -> true
   | _ -> false

let isInt (typ:VType.t) : bool =
   match !(VType.unlink typ) with
   | VType.TId(name,_) when name=["int"] -> true
   | _ -> false

let isBool (typ:VType.t) : bool =
   match !(VType.unlink typ) with
   | VType.TId(name,_) when name=["bool"] -> true
   | _ -> false

let attrType (attr:attr) : VType.t =
   match attr.typ with
   | Some(t) -> t
   | _ -> failwith "VultCh.attrType: everything should have types"

let convertTypedId params (e:typed_id) : arg_type * string =
   match e with
   | SimpleId(_,_)  -> failwith "VultCh.convertTypedId: everything should have types"
   | TypedId(id,typ,_) ->
      let typ_c = convertType params typ in
      let typ_ref = if isValue typ then Var(typ_c) else Ref(typ_c) in
      typ_ref, convertId id

let convertOperator params (op:string) (typ:VType.t) (elems:cexp list) : VType.t * cexp =
   let is_float = (params.real = Float) && (isReal typ) in
   let is_int   = (isInt typ) in
   let is_bool  = (isBool typ) in
   let is_builtin = is_float || is_int || is_bool in
   match op with
   | "<>" when is_builtin -> typ, CEOp("!=",elems)
   | "%"  when is_float   -> typ, CECall("fmodf",elems)

   | _ -> typ, CEOp(op,elems)

let convertFunction params (fn:id) (typ:VType.t) (elems:cexp list) : VType.t * cexp =
   let is_float = (params.real = Float) && (isReal typ) in
   let is_int   = (isInt typ) in
   let fixed_fn =
      match fn with
      | ["abs"]   when is_float -> `FunctionName(["fabsf"])
      | ["exp"]   when is_float -> `FunctionName(["expf"])
      | ["floor"] when is_float -> `FunctionName(["floorf"])
      | ["max"]   when is_float -> `FunctionName(["fmax"])
      | ["min"]   when is_float -> `FunctionName(["fmin"])
      | ["sin"]   when is_float -> `FunctionName(["sinf"])
      | ["cos"]   when is_float -> `FunctionName(["cosf"])
      | ["tan"]   when is_float -> `FunctionName(["tanf"])
      | ["tanh"]  when is_float -> `FunctionName(["tanhf"])
      | ["clip"]  when is_float -> `FunctionName(["clip_float"])
      | ["clip"]  when is_int   -> `FunctionName(["clip_int"])
      | ["not"] -> `UnOperatorName("!")
      | _ -> `FunctionName(fn)
   in
   match fixed_fn with
   | `FunctionName(fn)   -> typ, CECall(convertId fn, elems)
   | `UnOperatorName(op) -> typ, CEUnOp(op,List.hd elems)

let rec convertExp params (e:exp) : VType.t * cexp =
   match e with
   | PUnit(attr)       -> attrType attr, CEInt(0)
   | PBool(v,attr)     -> attrType attr, CEBool(v)
   | PInt(n,attr)      -> attrType attr, CEInt(n)
   | PReal(v,attr)     -> attrType attr, CEFloat(v)
   | PId(id,attr)      -> attrType attr, CEVar(convertId id)
   | PUnOp("|-|",e1,attr) ->
      let _, e1' = convertExp params e1 in
      attrType attr, CEUnOp("-",e1')
   | PUnOp(op,e1,attr) ->
      let _, e1' = convertExp params e1 in
      attrType attr, CEUnOp(op,e1')
   | POp(op,elems,attr) ->
      let _, elems' = convertExpList params elems in
      let typ = attrType attr in
      convertOperator params op typ elems'
   | PCall(_,[name],[arg],attr) when name="real" || name="int" || name="bool" ->
      let _, arg' = convertExp params arg in
      attrType attr, CECast(getCast params [name], arg')
   | PCall(_,name,elems,attr) ->
      let _, elems' = convertExpList params elems in
      let typ = attrType attr in
      convertFunction params name typ elems'
   | PIf(cond,then_,else_,attr) ->
      let _, cond'  = convertExp params cond in
      let _, then_' = convertExp params then_ in
      let _, else_' = convertExp params else_ in
      attrType attr, CEIf(cond', then_', else_')
   | PGroup(e1,_)      -> convertExp params e1
   | PTuple([e1],_)    -> convertExp params e1
   | PTuple(elems,attr)   ->
      let elems' =
         List.mapi
            (fun i a ->
                let _,a' = convertExp params a in
                "field_"^(string_of_int i), a')
            elems
      in
      attrType attr, CETuple(elems')
   | PSeq _            -> failwith "VultCh.convertExp: Sequences are not yet supported for js"
   | PEmpty            -> failwith "VultCh.convertExp: Empty expressions are not allowed"

and convertExpList params (e:exp list) : VType.t list * cexp list =
   List.map (convertExp params) e |> List.split

let rec convertLhsExp is_val params (e:lhs_exp) : clhsexp =
   match e with
   | LId(id,Some(typ),_) ->
      let new_id = convertId id in
      CLId(convertType params typ, new_id)
   | LId(_,None,_)   -> failwith "VultCh.convertLhsExp: everything should have types"
   | LTyped(e1,_,_)  -> convertLhsExp is_val params e1
   | LTuple(elems,_) ->
      let elems' = convertLhsExpList is_val params elems in
      CLTuple(elems')
   | LWild _ -> CLWild
   | LGroup(e,_) -> convertLhsExp is_val params e

and convertLhsExpList is_val params (lhsl:lhs_exp list) : clhsexp list =
   let lhsl_rev =
      List.fold_left
         (fun acc lhs ->
             (convertLhsExp is_val params lhs) :: acc)
         [] lhsl
   in
   List.rev lhsl_rev

let rec convertStmt params (s:stmt) : cstmt =
   match s with
   | StmtVal(lhs,None,_) ->
      let lhs' = convertLhsExp true params lhs in
      CSVarDecl(lhs',None)
   | StmtVal(lhs,Some(rhs),_) ->
      let lhs' = convertLhsExp true params lhs in
      let _, rhs' = convertExp params rhs in
      CSVarDecl(lhs',Some(rhs'))
   | StmtMem _                -> CSEmpty
   | StmtWhile(cond,stmt,_) ->
      let _, cond' = convertExp params cond in
      let stmt' = convertStmt params stmt in (* the env is ignored *)
      CSWhile(cond', stmt')
   | StmtReturn(e1,_) ->
      let _,e1' = convertExp params e1 in
      CSReturn(e1')
   | StmtIf(cond,then_,None,_) ->
      let _, cond' = convertExp params cond in
      let then_' = convertStmt params then_ in
      CSIf(cond',then_', None)
   | StmtIf(cond,then_,Some(else_),_) ->
      let _, cond' = convertExp params cond in
      let then_' = convertStmt params then_ in
      let else_' = convertStmt params else_ in
      CSIf(cond', then_', Some(else_'))
   | StmtFun(_,_,_,None,_) -> failwith "VultCh.convertStmt: everything should have types"
   | StmtFun(name,args,body,Some(ret),_) ->
      let arg_names = List.map (convertTypedId params) args in
      let body'  = convertStmt params body in
      CSFunction(convertType params ret, convertId name,arg_names,body')
   | StmtBind(lhs,rhs,_) ->
      let lhs' = convertLhsExp false params lhs in
      let _, rhs' = convertExp params rhs in
      CSBind(lhs', rhs')
   | StmtBlock(_,stmts,_) ->
      let stmts' = convertStmtList params stmts in
      CSBlock(stmts')
   | StmtType(name,members,_) ->
      let type_name    = convertType params name in
      let member_pairs = List.map (fun (id,typ,_) -> convertType params typ, convertId id) members in
      CSType(type_name,member_pairs)
   | StmtAliasType(t1,t2,_) ->
      let t1_name    = convertType params t1 in
      let t2_name    = convertType params t2 in
      CSAlias(t2_name,t1_name)
   | StmtEmpty       -> CSEmpty
   | StmtExternal _  -> CSEmpty

and convertStmtList params (stmts:stmt list) : cstmt list =
   let stmts_rev =
      List.fold_left
         (fun acc stmt ->
             convertStmt params stmt :: acc)
         [] stmts
   in
   List.rev stmts_rev


(** Prints the C code *)
module PrintC = struct

   let isSimple (e:cexp) : bool =
      match e with
      | CEInt _
      | CEFloat _
      | CEBool _
      | CEString _
      | CECall _
      | CEVar _
      | CENewObj -> true
      | _ -> false

   let rec printExp buffer (e:cexp) : unit =
      match e with
      | CEFloat(n)  -> append buffer ((string_of_float n)^"f")
      | CEInt(n)    -> append buffer (string_of_int n)
      | CEBool(v)   -> append buffer (if v then "1" else "0")
      | CEString(s) -> append buffer ("\"" ^ s ^ "\"")
      | CECall(name,args) ->
         append buffer name;
         append buffer "(";
         printList buffer printExp "," args;
         append buffer ")"
      | CEUnOp(op,e) ->
         append buffer "(";
         append buffer op;
         append buffer " ";
         printExp buffer e;
         append buffer ")"
      | CEOp(op,elems) ->
         append buffer "(";
         printExpList buffer (" "^(op)^" ") elems;
         append buffer ")";
      | CEVar(name) ->
         append buffer name
      | CEIf(cond,then_,else_) ->
         append buffer "(";
         printExp buffer cond;
         append buffer "?";
         printExp buffer then_;
         append buffer ":";
         printExp buffer else_;
         append buffer ")"
      | CENewObj -> append buffer "{}"
      | CETuple(elems) ->
         append buffer "{ ";
         printList buffer printChField ", " elems;
         append buffer " }"
      | CECast(typ,exp) ->
         append buffer "((";
         append buffer typ;
         append buffer ")";
         printExp buffer exp;
         append buffer ")"

   and printChField buffer (name,value) =
      append buffer ".";
      append buffer name;
      append buffer " = ";
      printExp buffer value

   and printExpList buffer (sep:string) (e:cexp list) : unit =
      match e with
      | []     -> ()
      | [ h ]  -> printExp buffer h
      | h :: t ->
         printExp buffer h;
         append buffer sep;
         printExpList buffer sep t

   let isSpecial name =
      match name with
      | "process" -> true
      | "noteOn" -> true
      | "noteOff" -> true
      | "controlChange" -> true
      | "default" -> true
      | _ -> false

   let printLhsExpTuple buffer (var:string) (is_var:bool) (i:int) (e:clhsexp) : unit =
      match e with
      | CLId(typ,name) ->
         if is_var then (append buffer typ; append buffer " ");
         append buffer name;
         append buffer " = ";
         append buffer var;
         append buffer (".field_"^(string_of_int i));
         append buffer "; ";
      | CLWild -> ()

      | _ -> failwith "printLhsExp: All other cases should be already covered"

   let printFunArg buffer (ntype,name) =
      match ntype with
      | Var(typ) ->
         append buffer typ;
         append buffer " ";
         append buffer name
      | Ref(typ) ->
         append buffer typ;
         append buffer " &";
         append buffer name

   let rec printStmt buffer (params:parameters) (stmt:cstmt) : bool =
      match stmt with
      | CSVarDecl(CLWild,None) -> false
      | CSVarDecl(CLWild,Some(value)) ->
         printExp buffer value;
         append buffer ";";
         true
      | CSVarDecl(CLId(ntype,name),Some(value)) ->
         append buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer " = ";
         printExp buffer value;
         append buffer ";";
         true
      | CSVarDecl(CLId(ntype,name),None) ->
         append buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer ";";
         true
      | CSVarDecl(CLTuple(elems),Some(CEVar(name))) ->
         List.iteri (printLhsExpTuple buffer name true) elems;
         true
      | CSVarDecl(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLWild,value) ->
         printExp buffer value;
         append buffer ";";
         true
      | CSBind(CLTuple(elems),CEVar(name)) ->
         List.iteri (printLhsExpTuple buffer name false) elems;
         true
      | CSBind(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLId(_,name),value) ->
         append buffer name;
         append buffer " = ";
         printExp buffer value;
         append buffer ";";
         true
      | CSFunction(ntype,name,args,(CSBlock(_) as body)) ->
         append buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer "(";
         printList buffer printFunArg ", " args;
         append buffer ")";
         if params.is_header then
            append buffer ";"
         else
            printStmt buffer params body |> ignore;
         newline buffer;
         true
      | CSFunction(ntype,name,args,body) ->
         append buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer "(";
         printList buffer printFunArg ", " args;
         append buffer ")";
         if params.is_header then
            append buffer ";"
         else
            begin
               append buffer "{ ";
               printStmt buffer params body |> ignore;
               append buffer "}";
            end;
         newline buffer;
         true
      | CSReturn(e1) ->
         append buffer "return ";
         printExp buffer e1;
         append buffer ";";
         true
      | CSWhile(cond,body) ->
         append buffer "while(";
         printExp buffer cond;
         append buffer ")";
         printStmt buffer params body;
      | CSBlock(elems) ->
         append buffer "{";
         indent buffer;
         printStmtList buffer params elems;
         outdent buffer;
         append buffer "}";
         true
      | CSIf(cond,then_,None) ->
         append buffer "if";
         if isSimple cond then append buffer "(";
         printExp buffer cond;
         if isSimple cond then append buffer ")";
         printStmt buffer params then_;
      | CSIf(cond,then_,Some(else_)) ->
         append buffer "if";
         if isSimple cond then append buffer "(";
         printExp buffer cond;
         if isSimple cond then append buffer ")";
         printStmt buffer params then_ |> ignore;
         newline buffer;
         append buffer "else";
         newline buffer;
         printStmt buffer params else_ |> ignore;
         true
      | CSType(name,members) when params.is_header ->
         append buffer "typedef struct ";
         append buffer name;
         append buffer " {";
         indent buffer;
         List.iter
            (fun (typ, name) ->
                append buffer typ;
                append buffer " ";
                append buffer name;
                append buffer ";";
                newline buffer;
            ) members;
         outdent buffer;
         append buffer "} ";
         append buffer name;
         append buffer ";";
         newline buffer;
         true
      | CSType(_,_) -> false
      | CSAlias(t1,t2) when params.is_header ->
         append buffer "typedef ";
         append buffer t1;
         append buffer " ";
         append buffer t2;
         append buffer ";";
         newline buffer;
         true
      | CSAlias(_,_) -> false
      | CSEmpty -> false

   and printStmtList buffer (params:parameters) (stmts:cstmt list) : unit =
      match stmts with
      | [] -> ()
      | h :: t ->
         let insert_new_line = printStmt buffer params h in
         if insert_new_line then newline buffer;
         printStmtList buffer params t

   let printChCode (params:parameters) (stmts:cstmt list) : string =
      let buffer = makePrintBuffer () in
      let _      = printStmtList buffer params stmts in
      let code   = contents buffer in
      Templates.apply params.template params.output code

end

let createParameters (args:arguments) : parameters =
   let real     = match args.real with | "fixed" -> Fixed | _ -> Float in
   let template = Templates.get args.template in
   let output = Filename.basename args.output in
   { real = real; template = template; is_header = false; output = output; }

(** Generates the .c and .h file contents for the given parsed files *)
let generateChCode (args:arguments) (parser_results:parser_results list) : (string * string) list =
   let stmts =
      parser_results
      |> List.map (
         fun a -> match a.presult with
            | `Ok(b) -> b
            | _ -> [] )
      |> List.flatten
   in
   let params = createParameters args in
   let js_stmts  = convertStmtList params stmts in
   let h   = PrintC.printChCode { params with is_header = true; template = Templates.Header } js_stmts in
   let cpp = PrintC.printChCode { params with is_header = false; template = Templates.Implementation } js_stmts in
   [h,"h"; cpp,"cpp"]

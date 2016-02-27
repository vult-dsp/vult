open TypesVult
open PrintBuffer

type type_descr =
   | CTSimple of string
   | CTArray  of type_descr * int

type arg_type =
   | Ref of type_descr
   | Var of type_descr

type cexp =
   | CEInt    of int
   | CEFloat  of float
   | CEBool   of bool
   | CEString of string
   | CEArray  of cexp list
   | CECall   of string * cexp list
   | CEUnOp   of string * cexp
   | CEOp     of string * cexp list
   | CEVar    of string
   | CEIf     of cexp * cexp * cexp
   | CETuple  of (string * cexp) list
   | CENewObj

type clhsexp =
   | CLWild
   | CLId    of type_descr * string
   | CLTuple of clhsexp list

type cstmt =
   | CSVarDecl  of clhsexp * cexp option
   | CSBind     of clhsexp * cexp
   | CSFunction of type_descr * string * (arg_type * string) list * cstmt
   | CSReturn   of cexp
   | CSWhile    of cexp * cstmt
   | CSBlock    of cstmt list
   | CSIf       of cexp * cstmt * cstmt option
   | CSType     of string * (type_descr * string) list
   | CSAlias    of string * type_descr
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
#include \"vultin.h\"

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

let rec convertType params (tp:VType.t) : type_descr =
   match !tp with
   | VType.TId(["real"],_) when params.real = Float -> CTSimple("float")
   | VType.TId(["real"],_) when params.real = Fixed -> CTSimple("int32_t")
   | VType.TId(["bool"],_) -> CTSimple("uint8_t")
   | VType.TId(["unit"],_) -> CTSimple("void")
   | VType.TId(id,_) -> CTSimple(underscoreId id)
   | VType.TComposed(["tuple"],_,_) -> CTSimple(VType.getTupleName tp)
   | VType.TComposed(["array"],[kind;{ contents = VType.TInt(n,_)}],_) ->
      let sub = convertType params kind in
      CTArray(sub,n)
   | VType.TComposed(_,_,_) ->
      failwith ("VultCh.convertType: unsupported type in c code generation" ^ PrintTypes.typeStr tp)
   | VType.TLink(tp) -> convertType params tp
   | VType.TInt _
   | VType.TArrow _
   | VType.TUnbound _
   | VType.TExpAlt _ ->
      failwith ("VultCh.convertType: unsupported type in c code generation" ^ PrintTypes.typeStr tp)

let getCast params (from_typ:VType.t) (to_typ:VType.t) : string =
   match !(VType.unlink from_typ), !(VType.unlink to_typ) with
   | VType.TId(["real"],_),VType.TId(["int"],_) when params.real = Fixed ->
      "fix_to_int"
   | VType.TId(["real"],_),VType.TId(["int"],_) ->
      "float_to_int"
   | VType.TId(["int"],_),VType.TId(["real"],_) when params.real = Fixed ->
      "int_to_fix"
   | VType.TId(["int"],_),VType.TId(["real"],_) ->
      "int_to_float"
   | _ ->
      failwith ("VultCh.getCast: invalid casting of types " ^ PrintTypes.typeStr from_typ ^ " -> " ^ PrintTypes.typeStr to_typ)

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

let makeNestedCall (name:string) (args:cexp list) : cexp =
   let rec loop args acc =
      match args with
      | [] -> acc
      | h :: t -> loop t (CECall(name,[acc;h]))
   in
   match args with
   | [_;_] -> CECall(name,args)
   | [] -> failwith "VultCh.makeNestedCall: invalid number of arguments"
   | h :: t -> loop t h

let convertOperator params (op:string) (typ:VType.t) (elems:cexp list) : VType.t * cexp =
   let is_float = (params.real = Float) && (isReal typ) in
   let is_int   = (isInt typ) in
   let is_bool  = (isBool typ) in
   let is_builtin = is_float || is_int || is_bool in
   let is_fixed = (params.real = Fixed) && (isReal typ) in
   match op with
   | "<>" when is_builtin -> typ, CEOp("!=",elems)
   | "%"  when is_float   -> typ, CECall("fmodf",elems)

   | "+"  when is_fixed   -> typ, makeNestedCall "fix_add" elems
   | "-"  when is_fixed   -> typ, CECall("fix_sub",elems)
   | "*"  when is_fixed   -> typ, makeNestedCall  "fix_mul" elems
   | "/"  when is_fixed   -> typ, CECall("fix_div",elems)

   | _ -> typ, CEOp(op,elems)

let getFunctionSetType (elem_typs:VType.t list) : VType.t =
   match elem_typs with
   | [_;_;v] -> v
   | _ -> failwith "VultCh.getFunctionSetType: this is not a call to 'set'"

let convertFunction params (fn:id) (typ:VType.t) (elems:cexp list) (elem_typs:VType.t list): VType.t * cexp =
   let is_float = (params.real = Float) && (isReal typ) in
   let is_fixed = (params.real = Fixed) && (isReal typ) in
   let is_int   = (isInt typ) in
   let is_bool  = (isBool typ) in
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
      | ["clip"]  when is_float -> `FunctionName(["float_clip"])

      | ["abs"]   when is_fixed -> `FunctionName(["fix_abs"])
      | ["exp"]   when is_fixed -> `FunctionName(["fix_exp"])
      | ["floor"] when is_fixed -> `FunctionName(["fix_floor"])
      | ["max"]   when is_fixed -> `FunctionName(["fix_max"])
      | ["min"]   when is_fixed -> `FunctionName(["fix_min"])
      | ["sin"]   when is_fixed -> `FunctionName(["fix_sin"])
      | ["cos"]   when is_fixed -> `FunctionName(["fix_cos"])
      | ["tan"]   when is_fixed -> `FunctionName(["fix_tan"])
      | ["tanh"]  when is_fixed -> `FunctionName(["fix_tanh"])
      | ["clip"]  when is_fixed -> `FunctionName(["fix_clip"])
      | ["set"]   when is_fixed -> `FunctionName(["fix_set"])

      | ["clip"]  when is_int   -> `FunctionName(["int_clip"])

      | ["get"]   when is_float -> `FunctionName(["float_get"])
      | ["get"]   when is_fixed -> `FunctionName(["fix_get"])
      | ["get"]   when is_int   -> `FunctionName(["int_get"])
      | ["get"]   when is_bool  -> `FunctionName(["bool_get"])

      | ["set"] ->
         begin
            match getFunctionSetType elem_typs with
            | t when isReal t && params.real = Float ->`FunctionName(["float_set"])
            | t when isReal t && params.real = Fixed ->`FunctionName(["fix_set"])
            | t when isInt t ->`FunctionName(["int_set"])
            | t when isBool t ->`FunctionName(["bool_set"])
            | _ -> failwith "Invalid type of array"
         end

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
   | PReal(v,attr)     ->
      attrType attr, CEFloat(v)
   | PId(id,attr)      -> attrType attr, CEVar(convertId id)
   | PArray(elems,attr) ->
      let _, elems' = convertExpList params elems in
      attrType attr, CEArray(elems')
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
      let from_typ, arg' = convertExp params arg in
      let to_typ = attrType attr in
      if VType.compare from_typ to_typ <> 0 then
         to_typ, CECall(getCast params from_typ to_typ, [arg'])
      else
         to_typ, arg'
   | PCall(_,name,elems,attr) ->
      let elem_typ, elems' = convertExpList params elems in
      let typ = attrType attr in
      convertFunction params name typ elems' elem_typ
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
      let type_name =
         match convertType params name with
         | CTSimple(t) -> t
         | _ -> failwith "VultCh.convertStmt: invalid alias type"
      in
      let member_pairs = List.map (fun (id,typ,_) -> convertType params typ, convertId id) members in
      CSType(type_name,member_pairs)
   | StmtAliasType(t1,t2,_) ->
      let t1_name    = convertType params t1 in
      let type_name =
         match convertType params t2 with
         | CTSimple(t) -> t
         | _ -> failwith "VultCh.convertStmt: invalid alias type"
      in
      CSAlias(type_name,t1_name)
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

   let toFixed (n:float) : string =
      (*let value =
         if n < 0.0 then
            Int32.of_float ((-. n) *. (float_of_int 0x10000))
            |> Int32.lognot
            |> Int32.add Int32.one
         else
            Int32.of_float (n *. (float_of_int 0x10000))
      in Printf.sprintf "0x%lx /* %f */" value n*)
      let value =
         if n < 0.0 then
            int_of_float ((-. n) *. (float_of_int 0x10000))
            |> lnot
            |> (+) 1
         else
            int_of_float (n *. (float_of_int 0x10000))
      in Printf.sprintf "0x%x /* %f */" value n

   let rec printExp (params:parameters) buffer (e:cexp) : unit =
      match e with
      | CEFloat(n) when params.real = Fixed ->
         if n < 0.0 then append buffer "(";
         append buffer (toFixed n);
         if n < 0.0 then append buffer ")"
      | CEFloat(n)  ->
         if n < 0.0 then append buffer "(";
         append buffer ((string_of_float n)^"f");
         if n < 0.0 then append buffer ")";
      | CEInt(n)    ->
         if n < 0 then append buffer "(";
         append buffer (string_of_int n);
         if n < 0 then append buffer ")"
      | CEBool(v)   -> append buffer (if v then "1" else "0")
      | CEString(s) -> append buffer ("\"" ^ s ^ "\"")
      | CEArray(elems) ->
         append buffer "{";
         printList buffer (printExp params) "," elems;
         append buffer "}"
      | CECall(name,args) ->
         append buffer name;
         append buffer "(";
         printList buffer (printExp params) "," args;
         append buffer ")"
      | CEUnOp(op,e) ->
         append buffer "(";
         append buffer op;
         append buffer " ";
         printExp params buffer e;
         append buffer ")"
      | CEOp(op,elems) ->
         append buffer "(";
         printList buffer (printExp params) (" "^(op)^" ") elems;
         append buffer ")";
      | CEVar(name) ->
         append buffer name
      | CEIf(cond,then_,else_) ->
         append buffer "(";
         printExp params buffer cond;
         append buffer "?";
         printExp params buffer then_;
         append buffer ":";
         printExp params buffer else_;
         append buffer ")"
      | CENewObj -> append buffer "{}"
      | CETuple(elems) ->
         append buffer "{ ";
         printList buffer (printChField params) ", " elems;
         append buffer " }"

   and printChField params buffer (name,value) =
      append buffer ".";
      append buffer name;
      append buffer " = ";
      printExp params buffer value

   let isSpecial name =
      match name with
      | "process" -> true
      | "noteOn" -> true
      | "noteOff" -> true
      | "controlChange" -> true
      | "default" -> true
      | _ -> false

   let rec simplifyArray (typ:type_descr) : string * string list =
      match typ with
      | CTSimple(name) -> name, []
      | CTArray(sub,size) ->
         let name,sub_size = simplifyArray sub in
         name, sub_size @ [string_of_int size]

   let printTypeDescr buffer (typ:type_descr) =
      let kind, sizes = simplifyArray typ in
      match sizes with
      | [] ->
         append buffer kind
      | _ ->
         append buffer kind;
         append buffer "[";
         printList buffer append ", " sizes;
         append buffer "]"

   let printTypeAndName (is_decl:bool) buffer (typ:type_descr) (name:string) =
      let kind, sizes = simplifyArray typ in
      if is_decl then (append buffer kind; append buffer " ");
      match sizes with
      | [] ->
         append buffer name
      | _ ->
         append buffer name;
         if is_decl then begin
            append buffer "[";
            printList buffer append ", " sizes;
            append buffer "]"
         end

   let printLhsExpTuple buffer (var:string) (is_var:bool) (i:int) (e:clhsexp) : unit =
      match e with
      | CLId(CTSimple(typ),name) ->
         if is_var then (append buffer typ; append buffer " ");
         append buffer name;
         append buffer " = ";
         append buffer var;
         append buffer (".field_"^(string_of_int i));
         append buffer "; "
      | CLId(typ,name) ->
         printTypeAndName is_var buffer typ name;
         append buffer " = ";
         append buffer var;
         append buffer (".field_"^(string_of_int i));
         append buffer "; "
      | CLWild -> ()

      | _ -> failwith "printLhsExpTuple: All other cases should be already covered"

   let printArrayBinding params buffer (var:string) (i:int) (e:cexp) : unit =
      append buffer var;
      append buffer "[";
      append buffer (string_of_int i);
      append buffer "] = ";
      printExp params buffer e;
      append buffer "; "

   let printLhsExp buffer (is_var:bool) (e:clhsexp) : unit =
      match e with
      | CLId(CTSimple(typ),name) ->
         if is_var then (append buffer typ; append buffer " ");
         append buffer name;
      | CLId(typ,name) ->
         printTypeAndName is_var buffer typ name
      | CLWild -> ()

      | _ -> failwith "printLhsExp: All other cases should be already covered"

   let printFunArg buffer (ntype,name) =
      match ntype with
      | Var(typ) ->
         printTypeDescr buffer typ;
         append buffer " ";
         append buffer name
      | Ref(typ) ->
         printTypeDescr buffer typ;
         append buffer " &";
         append buffer name

   let rec printStmt (params:parameters) buffer (stmt:cstmt) : bool =
      match stmt with
      | CSVarDecl(CLWild,None) -> false
      | CSVarDecl(CLWild,Some(value)) ->
         printExp params buffer value;
         append buffer ";";
         true
      | CSVarDecl((CLId(_,_) as lhs),Some(value)) ->
         printLhsExp buffer true lhs;
         append buffer " = ";
         printExp params buffer value;
         append buffer ";";
         true
      | CSVarDecl((CLId(_,_) as lhs),None) ->
         printLhsExp buffer true lhs;
         append buffer ";";
         true
      | CSVarDecl(CLTuple(elems),Some(CEVar(name))) ->
         List.iteri (printLhsExpTuple buffer name true) elems;
         true
      | CSVarDecl(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLWild,value) ->
         printExp params buffer value;
         append buffer ";";
         true
      | CSBind(CLTuple(elems),CEVar(name)) ->
         List.iteri (printLhsExpTuple buffer name false) elems;
         true
      | CSBind(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLId(_,name),CEArray(elems)) ->
         List.iteri (printArrayBinding params buffer name) elems;
         true
      | CSBind(CLId(_,name),value) ->
         append buffer name;
         append buffer " = ";
         printExp params buffer value;
         append buffer ";";
         true
      | CSFunction(ntype,name,args,(CSBlock(_) as body)) ->
         printTypeDescr buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer "(";
         printList buffer printFunArg ", " args;
         append buffer ")";
         if params.is_header then
            append buffer ";"
         else
            printStmt params buffer body |> ignore;
         newline buffer;
         true
      | CSFunction(ntype,name,args,body) ->
         printTypeDescr buffer ntype;
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
               printStmt params buffer body |> ignore;
               append buffer "}";
            end;
         newline buffer;
         true
      | CSReturn(e1) ->
         append buffer "return ";
         printExp params buffer e1;
         append buffer ";";
         true
      | CSWhile(cond,body) ->
         append buffer "while(";
         printExp params buffer cond;
         append buffer ")";
         printStmt params buffer body;
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
         printExp params buffer cond;
         if isSimple cond then append buffer ")";
         printStmt params buffer then_;
      | CSIf(cond,then_,Some(else_)) ->
         append buffer "if";
         if isSimple cond then append buffer "(";
         printExp params buffer cond;
         if isSimple cond then append buffer ")";
         printStmt params buffer then_ |> ignore;
         newline buffer;
         append buffer "else";
         newline buffer;
         printStmt params buffer else_ |> ignore;
         true
      | CSType(name,members) when params.is_header ->
         append buffer "typedef struct ";
         append buffer name;
         append buffer " {";
         indent buffer;
         List.iter
            (fun (typ, name) ->
                printTypeAndName true buffer typ name;
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
         printTypeDescr buffer t2;
         append buffer ";";
         newline buffer;
         true
      | CSAlias(_,_) -> false
      | CSEmpty -> false

   and printStmtList buffer (params:parameters) (stmts:cstmt list) : unit =
      match stmts with
      | [] -> ()
      | h :: t ->
         let insert_new_line = printStmt params buffer h in
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

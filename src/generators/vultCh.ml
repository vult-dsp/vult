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
   | CECall   of string * cexp_type list
   | CEUnOp   of string * cexp
   | CEOp     of string * cexp list
   | CEVar    of string
   | CEIf     of cexp * cexp * cexp
   | CETuple  of (string * cexp) list
   | CECast   of string * cexp
   | CENewObj

and cexp_type =
   | CRef of cexp
   | CVar of cexp

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

type parameters =
   {
      real : real_type;
      env  : string list list;
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

let look env name =
   match env with
   | [] -> false
   | h::_ -> List.exists (fun a -> a = name) h

let newEnv params =
   { params with env = []::params.env }

let add params name =
   let current, rest =
      match params.env with
      | []   -> [],[]
      | h::t -> h, t
   in
   { params with env = (name::current)::rest }

let convertId params (id:id) : string =
   match id with
   | [a]   -> fixKeyword a
   | [a;b] when look params.env a -> (fixKeyword a)^"."^(fixKeyword b)
   | [a;b] -> (fixKeyword a)^"->"^(fixKeyword b)
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

let wrapArgument (typ:VType.t) (e:cexp) =
   if isValue typ then CVar(e) else CRef(e)
let wrapArguments = List.map2 wrapArgument

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
      typ_ref, convertId params id

let convertOperator params (op:string) (typ:VType.t) (elems:cexp list) (elem_types:VType.t list) : VType.t * cexp =
   let is_float = (params.real = Float) && (isReal typ) in
   let is_int   = (isInt typ) in
   let is_bool  = (isBool typ) in
   let is_builtin = is_float || is_int || is_bool in
   match op with
   | "<>" when is_builtin -> typ, CEOp("!=",elems)
   | "%"  when is_float   -> typ, CECall("fmod",wrapArguments elem_types elems)

   | _ -> typ, CEOp(op,elems)

let convertFunction params (fn:id) (typ:VType.t) (elems:cexp list) (elem_types:VType.t list) : VType.t * cexp =
   let is_float = (params.real = Float) && (isReal typ) in
   let is_int   = (isInt typ) in
   let fixed_fn =
      match fn with
      | ["abs"]   when is_float -> `FunctionName(["fabsf"])
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
   | `FunctionName(fn)   -> typ, CECall(convertId params fn,wrapArguments elem_types elems)
   | `UnOperatorName(op) -> typ, CEUnOp(op,List.hd elems)

let rec convertExp params (e:exp) : VType.t * cexp =
   match e with
   | PUnit(attr)       -> attrType attr, CEInt(0)
   | PBool(v,attr)     -> attrType attr, CEBool(v)
   | PInt(n,attr)      -> attrType attr, CEInt(n)
   | PReal(v,attr)     -> attrType attr, CEFloat(v)
   | PId(id,attr)      -> attrType attr, CEVar(convertId params id)
   | PUnOp("|-|",e1,attr) ->
      let _, e1' = convertExp params e1 in
      attrType attr, CEUnOp("-",e1')
   | PUnOp(op,e1,attr) ->
      let _, e1' = convertExp params e1 in
      attrType attr, CEUnOp(op,e1')
   | POp(op,elems,attr) ->
      let elem_typ, elems' = convertExpList params elems in
      let typ = attrType attr in
      convertOperator params op typ elems' elem_typ
   | PCall(_,[name],[arg],attr) when name="real" || name="int" || name="bool" ->
      let _, arg' = convertExp params arg in
      attrType attr, CECast(getCast params [name], arg')
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

and convertArgument params (e:exp) : cexp_type =
   let typ, e' = convertExp params e in
   let arg = wrapArgument typ e' in
   let () = Printf.printf "- %s -> %s \n" (PrintTypes.expressionStr e) (PrintTypes.typeStr typ) in
   arg

let rec convertLhsExp is_val params (e:lhs_exp) : parameters * clhsexp =
   match e with
   | LId(id,Some(typ),_) ->
      let new_id = convertId params id in
      let params' = if is_val then add params new_id else params in
      params', CLId(convertType params typ, new_id)
   | LId(_,None,_)   -> failwith "VultCh.convertLhsExp: everything should have types"
   | LTyped(e1,_,_)  -> convertLhsExp is_val params e1
   | LTuple(elems,_) ->
      let params', elems' = convertLhsExpList is_val params elems in
      params', CLTuple(elems')
   | LWild _ -> params, CLWild
   | LGroup(e,_) -> convertLhsExp is_val params e

and convertLhsExpList is_val params (lhsl:lhs_exp list) : parameters * clhsexp list =
   let params', lhsl_rev =
      List.fold_left
         (fun (params,acc) lhs ->
             let params',lhs' = convertLhsExp is_val params lhs in
             params', lhs'::acc)
         (params,[]) lhsl
   in
   params', List.rev lhsl_rev

let rec convertStmt params (s:stmt) : parameters * cstmt =
   match s with
   | StmtVal(lhs,None,_) ->
      let params', lhs' = convertLhsExp true params lhs in
      params', CSVarDecl(lhs',None)
   | StmtVal(lhs,Some(rhs),_) ->
      let params', lhs' = convertLhsExp true params lhs in
      let _, rhs' = convertExp params rhs in
      params', CSVarDecl(lhs',Some(rhs'))
   | StmtMem _                -> params, CSEmpty
   | StmtTable _              -> failwith "VultCh.convertStmt: tables not implemented yet"
   | StmtWhile(cond,stmt,_) ->
      let _, cond' = convertExp params cond in
      let _, stmt' = convertStmt params stmt in (* the env is ignored *)
      params, CSWhile(cond', stmt')
   | StmtReturn(e1,_) ->
      let _,e1' = convertExp params e1 in
      params, CSReturn(e1')
   | StmtIf(cond,then_,None,_) ->
      let _, cond' = convertExp params cond in
      let _, then_' = convertStmt params then_ in
      params, CSIf(cond',then_', None)
   | StmtIf(cond,then_,Some(else_),_) ->
      let _, cond' = convertExp params cond in
      let _, then_' = convertStmt params then_ in
      let _, else_' = convertStmt params else_ in
      params, CSIf(cond', then_', Some(else_'))
   | StmtFun(_,_,_,None,_) -> failwith "VultCh.convertStmt: everything should have types"
   | StmtFun(name,args,body,Some(ret),_) ->
      let arg_names = List.map (convertTypedId params) args in
      let _, body'  = convertStmt (newEnv params) body in
      params, CSFunction(convertType params ret, convertId params name,arg_names,body')
   | StmtBind(lhs,rhs,_) ->
      let params', lhs' = convertLhsExp false params lhs in
      let _, rhs' = convertExp params rhs in
      params', CSBind(lhs', rhs')
   | StmtBlock(_,stmts,_) ->
      let params', stmts' = convertStmtList params stmts in
      params', CSBlock(stmts')
   | StmtType(name,members,_) ->
      let type_name    = convertType params name in
      let member_pairs = List.map (fun (id,typ,_) -> convertType params typ, convertId params id) members in
      params, CSType(type_name,member_pairs)
   | StmtAliasType(t1,t2,_) ->
      let t1_name    = convertType params t1 in
      let t2_name    = convertType params t2 in
      params, CSAlias(t2_name,t1_name)
   | StmtEmpty       -> params, CSEmpty
   | StmtExternal _  -> params, CSEmpty

and convertStmtList params (stmts:stmt list) : parameters * cstmt list =
   let params', stmts_rev =
      List.fold_left
         (fun (params,acc) stmt ->
             let params',stmt' = convertStmt params stmt in
             params', stmt'::acc)
         (params,[]) stmts
   in
   params', List.rev stmts_rev


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
      | CEFloat(n)  -> append buffer (string_of_float n)
      | CEInt(n)    -> append buffer (string_of_int n)
      | CEBool(v)   -> append buffer (if v then "1" else "0")
      | CEString(s) -> append buffer ("\"" ^ s ^ "\"")
      | CECall(name,args) ->
         append buffer name;
         append buffer "(";
         printList buffer printArgument "," args;
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
      append buffer name;
      append buffer " : ";
      printExp buffer value

   and printArgument buffer (arg:cexp_type) =
      match arg with
      | CVar(v) -> printExp buffer v
      | CRef(v) ->
         append buffer "&(";
         printExp buffer v;
         append buffer ")"

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
      | CLId(_,name) ->
         if is_var then append buffer "var ";
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
         append buffer " *";
         append buffer name

   let rec printStmt buffer (stmt:cstmt) : unit =
      match stmt with
      | CSVarDecl(CLWild,None) -> ()
      | CSVarDecl(CLWild,Some(value)) ->
         printExp buffer value;
         append buffer ";";
      | CSVarDecl(CLId(ntype,name),Some(value)) ->
         append buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer " = ";
         printExp buffer value;
         append buffer ";";
      | CSVarDecl(CLId(ntype,name),None) ->
         append buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer ";";
      | CSVarDecl(CLTuple(elems),Some(CEVar(name))) ->
         List.iteri (printLhsExpTuple buffer name true) elems;
      | CSVarDecl(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLWild,value) ->
         printExp buffer value;
         append buffer ";";
      | CSBind(CLTuple(elems),CEVar(name)) ->
         List.iteri (printLhsExpTuple buffer name false) elems;
      | CSBind(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLId(_,name),value) ->
         append buffer name;
         append buffer " = ";
         printExp buffer value;
         append buffer ";";
      | CSFunction(ntype,name,args,(CSBlock(_) as body)) ->
         append buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer "(";
         printList buffer printFunArg ", " args;
         append buffer ")";
         printStmt buffer body;
         newline buffer;
      | CSFunction(ntype,name,args,body) ->
         append buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer "(";
         printList buffer printFunArg ", " args;
         append buffer ") { ";
         printStmt buffer body;
         append buffer "}";
         newline buffer;
      | CSReturn(e1) ->
         append buffer "return ";
         printExp buffer e1;
         append buffer ";";
      | CSWhile(cond,body) ->
         append buffer "while(";
         printExp buffer cond;
         append buffer ")";
         printStmt buffer body;
      | CSBlock(elems) ->
         append buffer "{";
         indent buffer;
         printStmtList buffer elems;
         outdent buffer;
         append buffer "}";
      | CSIf(cond,then_,None) ->
         append buffer "if";
         if isSimple cond then append buffer "(";
         printExp buffer cond;
         if isSimple cond then append buffer ")";
         printStmt buffer then_;
      | CSIf(cond,then_,Some(else_)) ->
         append buffer "if";
         if isSimple cond then append buffer "(";
         printExp buffer cond;
         if isSimple cond then append buffer ")";
         printStmt buffer then_;
         newline buffer;
         append buffer "else";
         newline buffer;
         printStmt buffer else_;
      | CSType(name,members) ->
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
      | CSAlias(t1,t2) ->
         append buffer "typedef ";
         append buffer t1;
         append buffer " ";
         append buffer t2;
         append buffer ";";
         newline buffer;
      | CSEmpty -> ()

   and printStmtList buffer (stmts:cstmt list) : unit =
      match stmts with
      | [] -> ()
      | h :: t ->
         printStmt buffer h;
         newline buffer;
         printStmtList buffer t

   let template code =
      "
#include \"stdint.h\"
#include \"math.h\"

"^code^""

   let printChCode (params:parameters) (stmts:stmt list) : string =
      let buffer = makePrintBuffer () in
      let _, js  = convertStmtList params stmts in
      let _      = printStmtList buffer js in
      let code   = contents buffer in
      template code

end

let createParameters (args:arguments) : parameters =
   let real = match args.real with | "fixed" -> Fixed | _ -> Float in
   { real = real; env = [] }

(** Generates the .c and .h file contents for the given parsed files *)
let generateChCode (args:arguments) (parser_results:parser_results list) : string =
   let stmts =
      parser_results
      |> List.map (
         fun a -> match a.presult with
            | `Ok(b) -> b
            | _ -> [] )
      |> List.flatten
   in
   let params = createParameters args in
   PrintC.printChCode params stmts

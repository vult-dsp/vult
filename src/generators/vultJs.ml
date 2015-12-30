open TypesVult
open PrintBuffer

type jsexp =
   | JENumber of float
   | JEBool   of bool
   | JEString of string
   | JECall   of string * jsexp list
   | JEUnOp   of string * jsexp
   | JEOp     of string * jsexp list
   | JEVar    of string
   | JEIf     of jsexp * jsexp * jsexp
   | JENewObj

type jsstmt =
   | JSVarDecl  of string * jsexp
   | JSBind     of string * jsexp
   | JSFunction of string * string list * jsstmt
   | JSReturn   of jsexp
   | JSWhile    of jsexp * jsstmt
   | JSBlock    of jsstmt list
   | JSIf       of jsexp * jsstmt * jsstmt option
   | JSEmpty

let rec join (sep:string) (id:string list) : string =
   match id with
   | [] -> ""
   | [ name ] -> name
   | h :: t -> h ^ sep ^ (join sep t)

let rec convertId (id:id) : string =
   join "." id

let rec convertExp (e:exp) : jsexp =
   match e with
   | PUnit(_)        -> JENumber(0.0)
   | PBool(v,_)      -> JEBool(v)
   | PInt(n,_)       -> JENumber(float_of_int n)
   | PReal(v,_)      -> JENumber(v)
   | PId(id,_)       -> JEVar(convertId id)
   | PUnOp(op,e1,_)  -> JEUnOp(op, convertExp e1)
   | POp(op,elems,_) -> JEOp(op, convertExpList elems )
   | PCall(_,name,args,_)    -> JECall(convertId name, convertExpList args)
   | PIf(cond,then_,else_,_) -> JEIf(convertExp cond, convertExp then_, convertExp else_)
   | PGroup(e1,_)    -> convertExp e1
   | PTuple([e1],_)  -> convertExp e1
   | PTuple _        ->
      let e_str = PrintTypes.expressionStr e in
      failwith ("VultJs.convertExp: Tuples are not supported in js: " ^ e_str)
   | PSeq _          -> failwith "VultJs.convertExp: Sequences are not yet supported for js"
   | PEmpty          -> failwith "VultJs.convertExp: Empty expressions are not allowed"

and convertExpList (e:exp list) : jsexp list =
   List.map convertExp e

let rec convertLhsExp (e:lhs_exp) : string =
   match e with
   | LId(id,_,_)    -> convertId id
   | LTyped(e1,_,_) -> convertLhsExp e1
   | LTuple _       -> failwith "VultJs.convertLhsExp: Tuples are not supported in js"
   | LWild _        -> "_"

let convertTypedId (e:typed_id) : string =
   match e with
   | SimpleId(id,_)  -> convertId id
   | TypedId(id,_,_) -> convertId id

let rec convertStmt (s:stmt) : jsstmt =
   match s with
   | StmtVal(lhs,None,_)    -> JSVarDecl(convertLhsExp lhs,JENewObj)
   | StmtVal(lhs,Some(rhs),_) -> JSVarDecl(convertLhsExp lhs,convertExp rhs)
   | StmtMem _              -> JSEmpty
   | StmtTable _            -> failwith "VultJs.convertStmt: tables not implemented yet"
   | StmtWhile(cond,stmt,_) -> JSWhile(convertExp cond, convertStmt stmt)
   | StmtReturn(e1,_)       -> JSReturn(convertExp e1)
   | StmtIf(cond,then_,None,_) -> JSIf(convertExp cond, convertStmt then_, None)
   | StmtIf(cond,then_,Some(else_),_) -> JSIf(convertExp cond, convertStmt then_, Some(convertStmt else_))
   | StmtFun(name,args,body,_,_) ->
      let arg_names = List.map convertTypedId args in
      JSFunction(convertId name,arg_names,convertStmt body)
   | StmtBind(lhs,rhs,_)    -> JSBind(convertLhsExp lhs, convertExp rhs)
   | StmtBlock(_,stmts,_)   -> JSBlock(convertStmtList stmts)
   | StmtType _             -> JSEmpty
   | StmtAliasType _        -> JSEmpty
   | StmtEmpty              -> JSEmpty
   | StmtExternal _         -> JSEmpty

and convertStmtList (stmts:stmt list) : jsstmt list =
   List.map convertStmt stmts

let rec printExp buffer (e:jsexp) : unit =
   match e with
   | JENumber(n) -> append buffer (string_of_float n)
   | JEBool(v)   -> append buffer (if v then "true" else "false")
   | JEString(s) -> append buffer ("\"" ^ s ^ "\"")
   | JECall(name,args) ->
      append buffer name;
      append buffer "(";
      printExpList buffer "," args;
      append buffer ")"
   | JEUnOp(op,e) ->
      append buffer "(";
      append buffer op;
      append buffer " ";
      printExp buffer e;
      append buffer ")"
   | JEOp(op,elems) ->
      append buffer "(";
      printExpList buffer (" "^op^" ") elems;
      append buffer ")";
   | JEVar(name) ->
      append buffer name
   | JEIf(cond,then_,else_) ->
      append buffer "(";
      printExp buffer cond;
      append buffer "?";
      printExp buffer then_;
      append buffer ":";
      printExp buffer else_;
      append buffer ")"
   | JENewObj -> append buffer "{}"

and printExpList buffer (sep:string) (e:jsexp list) : unit =
   match e with
   | []     -> ()
   | [ h ]  -> printExp buffer h
   | h :: t ->
      printExp buffer h;
      append buffer sep;
      printExpList buffer sep t

let rec printStmt buffer (stmt:jsstmt) : unit =
   match stmt with
   | JSVarDecl(name,value) ->
      append buffer "var ";
      append buffer name;
      append buffer " = ";
      printExp buffer value;
      append buffer ";";
   | JSBind("_",value) ->
      printExp buffer value;
      append buffer ";";
   | JSBind(name,value) ->
      append buffer name;
      append buffer " = ";
      printExp buffer value;
      append buffer ";";
   | JSFunction(name,args,(JSBlock(_) as body)) ->
      append buffer "function ";
      append buffer name;
      append buffer "(";
      append buffer (join "," args);
      append buffer ")";
      printStmt buffer body;
   | JSFunction(name,args,body) ->
      append buffer "function ";
      append buffer name;
      append buffer "(";
      append buffer (join "," args);
      append buffer ") { ";
      printStmt buffer body;
      append buffer "}";
   | JSReturn(e1) ->
      append buffer "return ";
      printExp buffer e1;
      append buffer ";";
   | JSWhile(cond,body) ->
      append buffer "while(";
      printExp buffer cond;
      append buffer ")";
      printStmt buffer body;
   | JSBlock(elems) ->
      append buffer "{";
      indent buffer;
      printStmtList buffer elems;
      outdent buffer;
      append buffer "}";
   | JSIf(cond,then_,None) ->
      append buffer "if(";
      printExp buffer cond;
      append buffer ")";
      printStmt buffer then_;
   | JSIf(cond,then_,Some(else_)) ->
      append buffer "if(";
      printExp buffer cond;
      append buffer ")";
      printStmt buffer then_;
      newline buffer;
      append buffer "else";
      newline buffer;
      printStmt buffer else_;
   | JSEmpty -> ()

and printStmtList buffer (stmts:jsstmt list) : unit =
   match stmts with
   | [] -> ()
   | h :: t ->
      printStmt buffer h;
      newline buffer;
      printStmtList buffer t

let generateJsCode (stmts:stmt list) : string =
   let buffer = makePrintBuffer () in
   let js = convertStmtList stmts in
   let _ = printStmtList buffer js in
   contents buffer


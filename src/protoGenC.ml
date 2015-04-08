
open TypesVult

type ident = string

type ctyp =
  | TReal
  | TInt
  | TObj of ident

type op =
  | OPlus
  | OTimes
  | ODiv
  | OMinus
  | OLt
  | OGt
  | OEq
  | OUEq
  | OAnd
  | OOr

type cexp =
  | EOp     of cexp * op * cexp
  | ECall   of ident * cexp list
  | EVar    of ident list
  | EString of string
  | EReal   of float
  | EInt    of int
  | EUop    of op * cexp
  | ERef    of ident
  | EIf     of cexp * cexp * cexp

type cstmt =
  | SDecl     of ctyp * ident
  | SFunction of ident * (ctyp * ident) list * cstmt list
  | SBind     of ident * cexp
  | SWhile    of cexp * cstmt
  | SReturn   of cexp
  | SBlock    of cstmt list
  | SIf       of cexp * cstmt * cstmt option
  | SEmpty

type creal_type =
  | Fixed
  | Float
  | Double


let convertOp (op:string) : op =
   match op with
   | "+"  -> OPlus
   | "*"  -> OTimes
   | "/"  -> ODiv
   | "-"  -> OMinus
   | "<"  -> OLt
   | ">"  -> OGt
   | "==" -> OEq
   | "!=" -> OUEq
   | "&&" -> OAnd
   | "||" -> OOr
   | _    -> failwith ("Operator not implemented "^op)

let rec convertExp (e:exp) : cexp =
   match e with
   | PUnit(_)    -> EInt(0)
   | PBool(v,_)  -> if v then EInt(1) else EInt(0)
   | PInt(v,_)   -> EInt(int_of_string v)
   | PReal(v,_)  -> EReal(float_of_string v)
   | PId(id,_,_) -> EVar(id)
   | PUnOp(op,e1,_) ->
      EUop(convertOp op,convertExp e1)
   | PBinOp(op,e1,e2,_) ->
      EOp(convertExp e1,convertOp op,convertExp e2)
   | PCall(Some([inst]),[fname],args,_,_) ->
      ECall(fname,ERef(inst)::convertExpList args)
   | PCall(None,[fname],args,_,_) ->
      ECall(fname,convertExpList args)
   | PIf(cond,e1,e2,_) ->
      EIf(convertExp cond,convertExp e1,convertExp e2)
   | PGroup(e1,_) ->
      convertExp e1
   | PGroup(_,_) -> failwith "Tuples are not yet supported in expression context"
   | PSeq(_,_,_) -> failwith "Sequence expressions are not yet supported in expression context"
   | _ -> failwith "convertExp: not an expression"

and convertExpList (e:exp list) : cexp list =
   List.map convertExp e

let convertType (e:exp option) : ctyp =
   match e with
   | Some(PId(["int"],None,_))  -> TInt
   | Some(PId(["real"],None,_)) -> TReal
   | Some(PId([name],None,_))   -> TObj(name)
   | Some(_) -> failwith "convertType: unsupported type"
   | None -> TReal



let rec convertStmt (e:exp) : cstmt =
   match e with
   | StmtVal(PId([name],tp,_),None,_) -> SDecl(convertType tp,name)
   | StmtVal(_,_,_)   -> failwith "convertStmt: unsupported val declaration"
   | StmtMem(_,_,_,_) -> SEmpty
   | StmtWhile(cond,stmts,_) ->
      SWhile(convertExp cond,convertStmt stmts)
   | StmtReturn(v,_) -> SReturn(convertExp v)
   | StmtIf(cond,then_,Some(else_),_) ->
      SIf(convertExp cond,convertStmt then_,Some(convertStmt else_))
   | StmtIf(cond,then_,None,_) ->
      SIf(convertExp cond,convertStmt then_,None)
   | _ -> failwith "convertStmt: unsupported statement"


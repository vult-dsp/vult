open TypesVult
open PrintBuffer

type cexp =
   | CENumber of float
   | CEBool   of bool
   | CEString of string
   | CECall   of string * cexp list
   | CEUnOp   of string * cexp
   | CEOp     of string * cexp list
   | CEVar    of string
   | CEIf     of cexp * cexp * cexp
   | CETuple  of (string * cexp) list
   | CENewObj

type clhsexp =
   | CLWild
   | CLId    of string * string
   | CLTuple of clhsexp list

type cstmt =
   | CSVarDecl  of clhsexp * cexp option
   | CSBind     of clhsexp * cexp
   | CSFunction of string * string * (string * string) list * cstmt
   | CSReturn   of cexp
   | CSWhile    of cexp * cstmt
   | CSBlock    of cstmt list
   | CSIf       of cexp * cstmt * cstmt option
   | CSEmpty

let fixOp op =
   match op with
   | "<>" -> "!="
   | _ -> op

let fixKeyword key =
   match key with
   | "default" -> "default_"
   | _ -> key

let rec join (sep:string) (id:string list) : string =
   match id with
   | [] -> ""
   | [ name ] -> fixKeyword name
   | h :: t -> (fixKeyword h) ^ sep ^ (join sep t)

let rec convertId (id:id) : string =
   join "->" id

let convertType (typ:VType.t) : string =
   match VType.unlink typ with
   | { contents = VType.TId(name,_) } -> convertId name
   | _ -> failwith "VType.convertType: invalid type"

let convertTypedId (e:typed_id) : string * string =
   match e with
   | SimpleId(_,_)  -> failwith "VultCh.convertTypedId: everything should have types"
   | TypedId(id,typ,_) -> convertId id, convertType typ

let rec convertExp (e:exp) : cexp =
   match e with
   | PUnit(_)          -> CENumber(0.0)
   | PBool(v,_)        -> CEBool(v)
   | PInt(n,_)         -> CENumber(float_of_int n)
   | PReal(v,_)        -> CENumber(v)
   | PId(id,_)         -> CEVar(convertId id)
   | PUnOp("|-|",e1,_) -> CEUnOp("-", convertExp e1)
   | PUnOp(op,e1,_)    -> CEUnOp(op, convertExp e1)
   | POp(op,elems,_)   -> CEOp(fixOp op, convertExpList elems )
   | PCall(_,name,args,_)    -> CECall(convertId name, convertExpList args)
   | PIf(cond,then_,else_,_) -> CEIf(convertExp cond, convertExp then_, convertExp else_)
   | PGroup(e1,_)      -> convertExp e1
   | PTuple([e1],_)    -> convertExp e1
   | PTuple(elems,_)   ->
      let jselems = List.mapi (fun i a -> "field_"^(string_of_int i), convertExp a ) elems in
      CETuple(jselems)
   | PSeq _            -> failwith "VultCh.convertExp: Sequences are not yet supported for js"
   | PEmpty            -> failwith "VultCh.convertExp: Empty expressions are not allowed"

and convertExpList (e:exp list) : cexp list =
   List.map convertExp e

let rec convertLhsExp (e:lhs_exp) : clhsexp =
   match e with
   | LId(id,Some(typ),_) -> CLId(convertType typ, convertId id)
   | LId(_,None,_)   -> failwith "VultCh.convertLhsExp: everything should have types"
   | LTyped(e1,_,_)  -> convertLhsExp e1
   | LTuple(elems,_) -> CLTuple(List.map convertLhsExp elems)
   | LWild _         -> CLWild
   | LGroup(e,_)    -> convertLhsExp e

let attrType (attr:attr) : string =
   match attr.typ with
   | Some(t) -> convertType t
   | _ -> failwith "VultCh.attrType: everything should have types"

let rec convertStmt (s:stmt) : cstmt =
   match s with
   | StmtVal(lhs,None,_)    -> CSVarDecl(convertLhsExp lhs,None)
   | StmtVal(lhs,Some(rhs),_) -> CSVarDecl(convertLhsExp lhs,Some(convertExp rhs))
   | StmtMem _              -> CSEmpty
   | StmtTable _            -> failwith "VultCh.convertStmt: tables not implemented yet"
   | StmtWhile(cond,stmt,_) -> CSWhile(convertExp cond, convertStmt stmt)
   | StmtReturn(e1,_)       -> CSReturn(convertExp e1)
   | StmtIf(cond,then_,None,_) -> CSIf(convertExp cond, convertStmt then_, None)
   | StmtIf(cond,then_,Some(else_),_) -> CSIf(convertExp cond, convertStmt then_, Some(convertStmt else_))
   | StmtFun(_,_,_,None,_) -> failwith "VultCh.convertStmt: everything should have types"
   | StmtFun(name,args,body,Some(ret),_) ->
      let arg_names = List.map convertTypedId args in
      CSFunction(convertType ret, convertId name,arg_names,convertStmt body)
   | StmtBind(lhs,rhs,_)    -> CSBind(convertLhsExp lhs, convertExp rhs)
   | StmtBlock(_,stmts,_)   -> CSBlock(convertStmtList stmts)
   | StmtType _             -> CSEmpty
   | StmtAliasType _        -> CSEmpty
   | StmtEmpty              -> CSEmpty
   | StmtExternal _         -> CSEmpty

and convertStmtList (stmts:stmt list) : cstmt list =
   List.map convertStmt stmts

let rec printExp buffer (e:cexp) : unit =
   match e with
   | CENumber(n) -> append buffer (string_of_float n)
   | CEBool(v)   -> append buffer (if v then "true" else "false")
   | CEString(s) -> append buffer ("\"" ^ s ^ "\"")
   | CECall(name,args) ->
      append buffer name;
      append buffer "(";
      printExpList buffer "," args;
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
      printList buffer printJsField ", " elems;
      append buffer " }"

and printJsField buffer (name,value) =
   append buffer name;
   append buffer " : ";
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
   append buffer ntype;
   append buffer " ";
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
   | CSFunction(ntype,name,args,body) ->
      append buffer ntype;
      append buffer " ";
      append buffer name;
      append buffer "(";
      printList buffer printFunArg ", " args;
      append buffer ") { ";
      printStmt buffer body;
      append buffer "}";
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
      append buffer "if(";
      printExp buffer cond;
      append buffer ")";
      printStmt buffer then_;
   | CSIf(cond,then_,Some(else_)) ->
      append buffer "if(";
      printExp buffer cond;
      append buffer ")";
      printStmt buffer then_;
      newline buffer;
      append buffer "else";
      newline buffer;
      printStmt buffer else_;
   | CSEmpty -> ()

and printStmtList buffer (stmts:cstmt list) : unit =
   match stmts with
   | [] -> ()
   | h :: t ->
      printStmt buffer h;
      newline buffer;
      printStmtList buffer t

let template code =
   ""^code^""

let printChCode (stmts:stmt list) : string =
   let buffer = makePrintBuffer () in
   let js = convertStmtList stmts in
   let _ = printStmtList buffer js in
   let code = contents buffer in
   template code

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
   let js_text = printChCode stmts in
   if args.output<>"" then
      begin
         let oc = open_out (args.output^".js") in
         Printf.fprintf oc "%s\n" js_text;
         close_out oc;
         js_text
      end
   else
      begin
         print_endline js_text;
         js_text
      end


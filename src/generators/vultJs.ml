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
   | JETuple  of (string * jsexp) list
   | JENewObj

type jslhsexp =
   | JLWild
   | JLId    of string
   | JLTuple of jslhsexp list

type jsstmt =
   | JSVarDecl  of jslhsexp * jsexp
   | JSBind     of jslhsexp * jsexp
   | JSFunction of string * string list * jsstmt
   | JSReturn   of jsexp
   | JSWhile    of jsexp * jsstmt
   | JSBlock    of jsstmt list
   | JSIf       of jsexp * jsstmt * jsstmt option
   | JSEmpty

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
   join "." id

let rec convertExp (e:exp) : jsexp =
   match e with
   | PUnit(_)        -> JENumber(0.0)
   | PBool(v,_)      -> JEBool(v)
   | PInt(n,_)       -> JENumber(float_of_int n)
   | PReal(v,_)      -> JENumber(v)
   | PId(id,_)       -> JEVar(convertId id)
   | PUnOp("|-|",e1,_)-> JEUnOp("-", convertExp e1)
   | PUnOp(op,e1,_)  -> JEUnOp(op, convertExp e1)
   | POp(op,elems,_) -> JEOp(fixOp op, convertExpList elems )
   | PCall(_,name,args,_)    -> JECall(convertId name, convertExpList args)
   | PIf(cond,then_,else_,_) -> JEIf(convertExp cond, convertExp then_, convertExp else_)
   | PGroup(e1,_)    -> convertExp e1
   | PTuple([e1],_)  -> convertExp e1
   | PTuple(elems,_)   ->
      let jselems = List.mapi (fun i a -> "field_"^(string_of_int i), convertExp a ) elems in
      JETuple(jselems)
   | PSeq _          -> failwith "VultJs.convertExp: Sequences are not yet supported for js"
   | PEmpty          -> failwith "VultJs.convertExp: Empty expressions are not allowed"

and convertExpList (e:exp list) : jsexp list =
   List.map convertExp e

let rec convertLhsExp (e:lhs_exp) : jslhsexp =
   match e with
   | LId(id,_,_)     -> JLId(convertId id)
   | LTyped(e1,_,_)  -> convertLhsExp e1
   | LTuple(elems,_) -> JLTuple(List.map convertLhsExp elems)
   | LWild _         -> JLWild
   | LGroup(e,_)    -> convertLhsExp e

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
      append buffer "this.";
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
      printExpList buffer (" "^(op)^" ") elems;
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
   | JETuple(elems) ->
      append buffer "{ ";
      printList buffer printJsField ", " elems;
      append buffer " }"

and printJsField buffer (name,value) =
   append buffer name;
   append buffer " : ";
   printExp buffer value

and printExpList buffer (sep:string) (e:jsexp list) : unit =
   match e with
   | []     -> ()
   | [ h ]  -> printExp buffer h
   | h :: t ->
      printExp buffer h;
      append buffer sep;
      printExpList buffer sep t

let fixContext is_special args =
   if is_special then
      match args with
      | [] -> ["_ctx"]
      | "_ctx"::_ -> args
      | t -> "_ctx"::t
   else args

let isSpecial name =
   match name with
   | "process" -> true
   | "noteOn" -> true
   | "noteOff" -> true
   | "controlChange" -> true
   | "default" -> true
   | _ -> false

let printLhsExpTuple buffer (var:string) (is_var:bool) (i:int) (e:jslhsexp) : unit =
   match e with
   | JLId(name) ->
      if is_var then append buffer "var ";
      append buffer name;
      append buffer " = ";
      append buffer var;
      append buffer (".field_"^(string_of_int i));
      append buffer "; ";
   | JLWild -> ()

   | _ -> failwith "printLhsExp: All other cases should be already covered"

let rec printStmt buffer (stmt:jsstmt) : unit =
   match stmt with
   | JSVarDecl(JLWild,value) ->
      printExp buffer value;
      append buffer ";";
   | JSVarDecl(JLId(name),value) ->
      append buffer "var ";
      append buffer name;
      append buffer " = ";
      printExp buffer value;
      append buffer ";";
   | JSVarDecl(JLTuple(elems),JEVar(name)) ->
      List.iteri (printLhsExpTuple buffer name true) elems;
   | JSVarDecl(JLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
   | JSBind(JLWild,value) ->
      printExp buffer value;
      append buffer ";";
   | JSBind(JLTuple(elems),JEVar(name)) ->
      List.iteri (printLhsExpTuple buffer name false) elems;
   | JSBind(JLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
   | JSBind(JLId(name),value) ->
      append buffer name;
      append buffer " = ";
      printExp buffer value;
      append buffer ";";
   | JSFunction(name,args,(JSBlock(_) as body)) ->
      append buffer "this.";
      append buffer name;
      append buffer " = function(";
      let args' = fixContext (isSpecial name) args in
      append buffer (join "," args');
      append buffer ")";
      printStmt buffer body;
   | JSFunction(name,args,body) ->
      append buffer "this. ";
      append buffer name;
      append buffer " = function(";
      let args' = fixContext (isSpecial name) args in
      append buffer (join "," args');
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

let template code =
   "function vultProcess(){
    this.clip = function(x,low,high) { return x<low?low:(x>high?high:x); };
    this.not  = function(x)          { return x==0?1:0; };
    this.real = function(x)          { return x; };
    this.int  = function(x)          { return x; };
    this.sin  = function(x)          { return Math.sin(x); };
    this.cos  = function(x)          { return Math.cos(x); };
    this.abs  = function(x)          { return Math.abs(x); };
    this.exp  = function(x)          { return Math.exp(x); };
    this.floor= function(x)          { return Math.floor(x); };
    this.tan  = function(x)          { return Math.tan(x); };
    this.tanh = function(x)          { return Math.tanh(x); };
    this.sqrt = function(x)          { return x; };
    this.process_init = null;
    this.default_ = null;
"^code^"
    if(this.process_init)  this.context =  this.process_init(); else this.context = {};
    if(this.default_)      this.default_(this.context);
    this.liveNoteOn        = function(note,velocity) { if(this.noteOn)        this.noteOn(this.context,note,velocity); };
    this.liveNoteOff       = function(note,velocity) { if(this.noteOff)       this.noteOff(this.context,note,velocity); };
    this.liveControlChange = function(note,velocity) { if(this.controlChange) this.controlChange(this.context,note,velocity); };
    this.liveProcess       = function(input)         { if(this.process)       return this.process(this.context,input); else return 0; };
    this.liveDefault       = function()              { if(this.default_)      return this.default_(this.context); };
}
"

let printJsCode (stmts:stmt list) : string =
   let buffer = makePrintBuffer () in
   let js = convertStmtList stmts in
   let _ = printStmtList buffer js in
   let code = contents buffer in
   template code

(** Generates the .c and .h file contents for the given parsed files *)
let generateJSCode (_:arguments) (parser_results:parser_results list) : string =
   let stmts =
      parser_results
      |> List.map (
         fun a -> match a.presult with
            | `Ok(b) -> b
            | _ -> [] )
      |> List.flatten
   in
   let js_text = printJsCode stmts in
   js_text


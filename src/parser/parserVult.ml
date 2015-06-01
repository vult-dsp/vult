(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz, Carl Jönsson

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

(** Vult Parser *)

open LexerVult
open TypesVult
open Lexing
open ParserTypes
open TokenStream


module TokenKind = struct
   type kind    = token_enum
   let next     = next_token
   let kindStr  = kindToString
   let tokenStr = tokenToString
   let isEOF  x = x=EOF
   let getEOF   = EOF
end

module Stream = TokenStream(TokenKind)

let splitOnDot s = CCString.Split.list_cpy "." s

(** Consumes tokens until it finds the begining of a new statememt or the end of the current *)
let rec moveToNextStatement (buffer:Stream.stream) : unit =
   match Stream.peek buffer with
   | SEMI -> Stream.skip buffer
   | EOF -> ()
   | FUN | VAL
   | IF  | RET -> ()
   | RBRAC -> Stream.skip buffer
   | _ ->
      let _ = Stream.skip buffer in
      moveToNextStatement buffer

(** Returns the location of an expression *)
let getExpLocation (e:exp) : Location.t =
   match e with
   | PUnit(loc)
   | PInt(_,loc)
   | PBool(_,loc)
   | PReal(_,loc) -> loc
   | PId(_,_,loc) -> loc
   | PUnOp(_,_,loc)
   | PBinOp(_,_,_,loc)
   | PCall(_,_,_,_,loc)
   | PIf(_,_,_,loc)
   | PGroup(_,loc)
   | PTuple(_,loc) -> loc
   | PSeq(_,_,loc) -> loc
   | PEmpty -> Location.default

let getLhsExpLocation (e:lhs_exp) : Location.t =
   match e with
   | LWild(loc)
   | LId(_,loc)
   | LTuple(_,loc)
   | LTyped(_,_,loc) -> loc

(** Returns the location of an statement *)
let getStmtLocation (s:stmt)  : Location.t =
   match s with
   | StmtVal(_,_,loc)
   | StmtMem(_,_,_,loc)
   | StmtTable(_,_,loc)
   | StmtReturn(_,loc)
   | StmtIf(_,_,_,loc)
   | StmtFun(_,_,_,_,_,loc)
   | StmtBind(_,_,loc) -> loc
   | StmtEmpty -> Location.default
   | StmtBlock(_,_,loc) -> loc
   | StmtWhile(_,_,loc) -> loc
   | StmtType(_,_,_,loc) -> loc
   | StmtAliasType(_,_,_,loc) -> loc

(** Returns the left binding powers of the token *)
let getLbp (token:'kind token) : int =
   match token.kind,token.value with
   | COLON,_ -> 10
   | COMMA,_ -> 20
   | OP,"||" -> 30
   | OP,"&&" -> 30
   | OP,"==" -> 40
   | OP,"!=" -> 40
   | OP,">"  -> 40
   | OP,"<"  -> 40
   | OP,">=" -> 40
   | OP,"<=" -> 40
   | OP,"+"  -> 50
   | OP,"-"  -> 50
   | OP,"*"  -> 60
   | OP,"/"  -> 60
   | OP,"%"  -> 60
   | _       -> 0

(** Creates Pratt parser functions *)
let prattParser (rbp:int) (buffer:Stream.stream)
   (lbp:'kind token -> int)
   (nud:Stream.stream -> 'kind token -> 'exp)
   (led:Stream.stream -> 'kind token -> 'exp -> 'exp) =
   let current_token = Stream.current buffer in
   let _             = Stream.skip buffer in
   let left          = nud buffer current_token in
   let next_token    = Stream.current buffer in
   let rec loop token left repeat =
      if repeat then
         let _         = Stream.skip buffer in
         let new_left  = led buffer token left in
         let new_token = Stream.current buffer in
         loop new_token new_left (rbp < (lbp new_token))
      else
         left
   in loop next_token left (rbp < (lbp next_token))

let identifierToken (buffer:Stream.stream) (token:'kind token) : identifier =
   splitOnDot token.value

(** Parses a type expression using a Pratt parser *)
let rec typeExpression (rbp:int) (buffer:Stream.stream) : type_exp =
   prattParser rbp buffer getLbp type_nud type_led

and type_nud (buffer:Stream.stream) (token:'kind token) : type_exp =
   match token.kind with
   | ID ->
      let id = identifierToken buffer token in
      begin
         match Stream.peek buffer with
         | LPAREN ->
            composedType buffer token id
         | _ -> TId(id,token.loc)
      end
   | LPAREN ->
      begin
         let start_loc = token.loc in
         match Stream.peek buffer with
         | RPAREN ->
            let _ = Stream.skip buffer in
            TUnit(start_loc)
         | _ ->
            let el = typeArgList buffer in
            begin
               match el with
               | []   -> TUnit(start_loc)
               | [tp] -> tp
               | _ ->
                  let _ = Stream.consume buffer RPAREN in
                  TTuple(el,start_loc)
            end
      end
   | _ ->
      let message = Stream.notExpectedError token in
      raise (ParserError(message))

and type_led (buffer:Stream.stream) (token:'kind token) (left:type_exp) : type_exp =
   let message = Stream.notExpectedError token in
   raise (ParserError(message))

and composedType (buffer:Stream.stream) (token:'kind token) (id:identifier) : type_exp =
   let _ = Stream.skip buffer in
   let args =
      match Stream.peek buffer with
      | RPAREN -> []
      | _ -> typeArgList buffer
   in
   let _ = Stream.consume buffer RPAREN in
   TComposed(id,args,token.loc)

and typeArgList (buffer:Stream.stream) : type_exp list =
   let rec loop acc =
      (* power of 20 avoids returning a tuple instead of a list*)
      let e = typeExpression 20 buffer in
      match Stream.peek buffer with
      | COMMA ->
         let _ = Stream.skip buffer in
         loop (e::acc)
      | _ -> List.rev (e::acc)
   in loop []


(** Parses left hand side expression using a Pratt parser *)
let rec lhs_expression (rbp:int) (buffer:Stream.stream) : lhs_exp =
   prattParser rbp buffer getLbp lhs_nud lhs_led

and lhs_nud (buffer:Stream.stream) (token:'kind token) : lhs_exp =
   match token.kind with
   | WILD -> LWild(token.loc)
   | ID   ->
      let id = identifierToken buffer token in
      LId(id,token.loc)
   | LPAREN ->
      begin
         match Stream.peek buffer with
         | RPAREN ->
            let message = Stream.notExpectedError token in
            raise (ParserError(message))
         | _ ->
            let e = lhs_expression 0 buffer in
            let _ = Stream.consume buffer RPAREN in
            e
      end
   | _ ->
      let message = Stream.notExpectedError token in
      raise (ParserError(message))

and lhs_led (buffer:Stream.stream) (token:'kind token) (left:lhs_exp) : lhs_exp =
   match token.kind with
   | COLON ->
      let type_exp = typeExpression 0 buffer in
      LTyped(left,type_exp,token.loc)
   | COMMA ->
      lhs_pair buffer token left
   | _ -> failwith "lhs_led"

(** <pair> :=  <expression>  ',' <expression> [ ',' <expression> ] *)
and lhs_pair (buffer:Stream.stream) (token:'kind token) (left:lhs_exp) : lhs_exp =
   let right = lhs_expression (getLbp token) buffer in
   let getElems e =
      match e with
      | LTuple(elems,_) -> elems
      | _ -> [e]
   in
   let elems1 = left |> getElems in
   let elems2 = right |> getElems in
   LTuple(elems1@elems2,getLhsExpLocation left)

(** Parses an expression using a Pratt parser *)
let rec expression (rbp:int) (buffer:Stream.stream) : exp =
   prattParser rbp buffer getLbp exp_nud exp_led

(** Nud function for the Pratt parser *)
and exp_nud (buffer:Stream.stream) (token:'kind token) : exp =
   match token.kind,token.value with
   | OP,"-" -> (* Unary minus *)
      unaryOp buffer token
   | ID,_   -> (* Id or function call *)
      let id = identifierToken buffer token in
      begin
         match Stream.peek buffer with
         | LPAREN ->
            functionCall buffer token id
         | COLON ->
            let _ = Stream.skip buffer in
            let type_exp = expression 20 buffer in
            PId(id,Some(type_exp),token.loc)
         | _ -> PId(id,None,token.loc)
      end
   | LPAREN,_ ->
      begin
         let start_loc = token.loc in
         match Stream.peek buffer with
         | RPAREN ->
            let _ = Stream.skip buffer in
            PUnit(start_loc)
         | _ ->
            let e = expression 0 buffer in
            let _ = Stream.consume buffer RPAREN in
            PGroup(e,start_loc)
      end
   | INT,_   -> PInt(int_of_string token.value,token.loc)
   | REAL,_  -> PReal(float_of_string token.value,token.loc)
   | TRUE,_  -> PBool(true,token.loc)
   | FALSE,_ -> PBool(false,token.loc)
   | IF,_ ->
      let cond = expression 0 buffer in
      let _ = Stream.consume buffer THEN in
      let then_exp = expression 0 buffer in
      let _ = Stream.consume buffer ELSE in
      let else_exp = expression 0 buffer in
      PIf(cond,then_exp,else_exp,token.loc)
   | LSEQ,_ ->
      let stmts = pseqList buffer in
      PSeq(None,stmts,token.loc)
   | _ ->
      let message = Stream.notExpectedError token in
      raise (ParserError(message))

(** Led function for the Pratt parser *)
and exp_led (buffer:Stream.stream) (token:'kind token) (left:exp) : exp =
   match token.kind,token.value with
   | OP,_ -> (* Binary operators *)
      binaryOp buffer token left
   | COMMA,_ ->
      pair buffer token left
   | _ -> failwith "exp_led"
   (*| _ -> token*)

(** <pair> :=  <expression>  ',' <expression> [ ',' <expression> ] *)
and pair (buffer:Stream.stream) (token:'kind token) (left:exp) : exp =
   let right = expression (getLbp token) buffer in
   let getElems e =
      match e with
      | PTuple(elems,_) -> elems
      | _ -> [e]
   in
   let elems1 = left |> getElems in
   let elems2 = right |> getElems in
   let start_loc = getExpLocation left in
   PTuple(elems1@elems2,start_loc)

(** <functionCall> := <identifier> '(' <expressionList> ')' *)
and functionCall (buffer:Stream.stream) (token:'kind token) (id:identifier) : exp =
   let _ = Stream.skip buffer in
   let args =
      match Stream.peek buffer with
      | RPAREN -> []
      | _ -> expressionList buffer
   in
   let _ = Stream.consume buffer RPAREN in
   PCall(None,id,args,[],token.loc)

(** <unaryOp> := OP <expression> *)
and unaryOp (buffer:Stream.stream) (token:'kind token) : exp =
   let right = expression 70 buffer in
   PUnOp(token.value,right,token.loc)

(** <binaryOp> := <expression> OP <expression> *)
and binaryOp (buffer:Stream.stream) (token:'kind token) (left:exp) : exp =
   let right = expression (getLbp token) buffer in
   PBinOp(token.value,left,right,token.loc)

(** <expressionList> := <expression> [',' <expression> ] *)
and expressionList (buffer:Stream.stream) : exp list =
   let rec loop acc =
      (* power of 20 avoids returning a tuple instead of a list*)
      let e = expression 20 buffer in
      match Stream.peek buffer with
      | COMMA ->
         let _ = Stream.skip buffer in
         loop (e::acc)
      | _ -> List.rev (e::acc)
   in loop []

(** typedArg := <ID> [ ':' <ID>]  *)
and typedArg (buffer:Stream.stream) : typed_id =
   let _     = Stream.expect buffer ID in
   let token = Stream.current buffer in
   let _     = Stream.skip buffer in
   match Stream.peek buffer with
   | COLON ->
      let _ = Stream.skip buffer in
      let e = typeExpression 20 buffer in
      TypedId(splitOnDot token.value,e,token.loc)
   | _ -> SimpleId(splitOnDot token.value,token.loc)

and identifier (buffer:Stream.stream) : identifier =
   let _     = Stream.expect buffer ID in
   let token = Stream.current buffer in
   let _     = Stream.skip buffer in
   identifierToken buffer token

(** typedArgList := typedArg [',' typedArg ] *)
and typedArgList (buffer:Stream.stream) : typed_id list =
   match Stream.peek buffer with
   | ID ->
      let first = typedArg buffer in
      begin
         match Stream.peek buffer with
         | COMMA ->
            let _ = Stream.consume buffer COMMA in
            first::(typedArgList buffer)
         | _ -> [first]
      end
   | _ -> []

(** <optStartValue> := '(' <expression> ')' *)
and optStartValue (buffer:Stream.stream) : exp option =
   match Stream.peek buffer with
   | LPAREN ->
      let _ = Stream.consume buffer LPAREN in
      let e = expression 0 buffer in
      let _ = Stream.consume buffer RPAREN in
      Some(e)
   | _ -> None

(** initExpression := '(' expression ')'*)
and initExpression (buffer:Stream.stream) : exp option =
   match Stream.peek buffer with
   | AT ->
      let _ = Stream.skip buffer in
      let e = expression 0 buffer in
      Some(e)
   | _ -> None

(** <statement> := | 'val' <valBindList> ';' *)
and stmtVal (buffer:Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let _ = Stream.consume buffer VAL in
   let lhs = lhs_expression 0 buffer in
   (* TODO: Add check of lhs *)
   match Stream.peek buffer with
   | EQUAL ->
      let _   = Stream.skip buffer in
      let rhs = expression 0 buffer in
      let _   = Stream.consume buffer SEMI in
      StmtVal(lhs,Some(rhs),start_loc)
   | _ ->
      let _ = Stream.consume buffer SEMI in
      StmtVal(lhs,None,start_loc)

(** <statement> := | 'mem' <valBindList> ';' *)
and stmtMem (buffer:Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let _    = Stream.consume buffer MEM in
   let lhs  = lhs_expression 0 buffer in
   let init = initExpression buffer in
   (* TODO: Add check of lhs *)
   match Stream.peek buffer with
   | EQUAL ->
      let _   = Stream.skip buffer in
      let rhs = expression 0 buffer in
      let _   = Stream.consume buffer SEMI in
      StmtMem(lhs,init,Some(rhs),start_loc)
   | _ ->
      let _ = Stream.consume buffer SEMI in
      StmtMem(lhs,init,None,start_loc)

and stmtTab (buffer: Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let _     = Stream.consume buffer TABLE in
   let name  = identifier buffer in
   let _     = Stream.consume buffer EQUAL in
   let _     = Stream.consume buffer LARR in
   let elems = expressionList buffer in
   let _     = Stream.consume buffer RARR in
   let _     = Stream.consume buffer SEMI in
   StmtTable(name,elems,start_loc)

(** <statement> := | 'return' <expression> ';' *)
and stmtReturn (buffer:Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let _ = Stream.consume buffer RET in
   let e = expression 0 buffer in
   let _ = Stream.consume buffer SEMI in
   StmtReturn(e,start_loc)

and stmtBind (buffer:Stream.stream) : stmt =
   let e1 = lhs_expression 0 buffer in
   let start_loc = getLhsExpLocation e1 in
   match Stream.peek buffer with
   | EQUAL ->
      let _  = Stream.consume buffer EQUAL in
      let e2 = expression 0 buffer in
      let _  = Stream.consume buffer SEMI in
      StmtBind(e1,e2,start_loc)
   (*
   | SEMI ->
      let _  = Stream.consume buffer SEMI in
      StmtBind(PUnit(start_loc),e1,start_loc)
   *)
   | kind ->
      let expected = kindToString EQUAL in
      let got = kindToString kind in
      let message = Printf.sprintf "Expecting a %s while trying to parse a binding (%s = ...) but got %s" expected (PrintTypes.lhsExpressionStr e1) got in
      raise (ParserError(Stream.makeError buffer message))

(** <statement> := 'if' '(' <expression> ')' <statementList> ['else' <statementList> ]*)
and stmtIf (buffer:Stream.stream) : stmt =
   let _    = Stream.consume buffer IF in
   let _    = Stream.consume buffer LPAREN in
   let cond = expression 0 buffer in
   let _    = Stream.consume buffer RPAREN in
   let tstm = stmtList buffer in
   let start_loc = getExpLocation cond in
   match Stream.peek buffer with
   | ELSE ->
      let _    = Stream.consume buffer ELSE in
      let fstm = stmtList buffer in
      StmtIf(cond,tstm,Some(fstm),start_loc)
   | _ -> StmtIf(cond,tstm,None,start_loc)

(** 'fun' <identifier> '(' <typedArgList> ')' <stmtList> *)
and stmtFunction (buffer:Stream.stream) : stmt =
   let isjoin = match Stream.peek buffer with | AND -> true | _ -> false in
   let _      = Stream.skip buffer in
   let name   = identifier buffer in
   let token  = Stream.current buffer in
   let _      = Stream.consume buffer LPAREN in
   let args   =
      match Stream.peek buffer with
      | RPAREN -> []
      | _ -> typedArgList buffer
   in
   let _        = Stream.consume buffer RPAREN in
   let type_exp =
      match Stream.peek buffer with
      | COLON ->
         let _ = Stream.skip buffer in
         Some(typeExpression 0 buffer)
      | _ -> None
   in
   let body = stmtList buffer in
   let start_loc = token.loc in
   let attr = if isjoin then [JoinFunction] else [] in
   StmtFun(name,args,body,type_exp,attr,start_loc)

(** 'type' <identifier> '(' <typedArgList> ')' <valDeclList> *)
and stmtType (buffer:Stream.stream) : stmt =
   let _     = Stream.consume buffer TYPE in
   let name  = identifier buffer in
   let token = Stream.current buffer in
   let start_loc = token.loc in
   let args  =
      match Stream.peek buffer with
      | LPAREN ->
         let _    = Stream.skip buffer in
         let args = typedArgList buffer in
         let _    = Stream.consume buffer RPAREN in
         args
      | _ -> []
   in
   match Stream.peek buffer with
   | COLON ->
      let _        = Stream.skip buffer in
      let type_exp = typeExpression 10 buffer in
      let _        = Stream.optConsume buffer SEMI in
      StmtAliasType(name,args,type_exp,start_loc)
   | LBRAC ->
      let _        = Stream.skip buffer in
      let val_decl = valDeclList buffer in
      let _        = Stream.consume buffer RBRAC in
      StmtType(name,args,val_decl,start_loc)
   | _ ->
      let got = tokenToString (Stream.current buffer) in
      let message = Printf.sprintf "Expecting a list of value declarations '{ val x:... }' or a type alias ': type' but got %s" got  in
      raise (ParserError(Stream.makeError buffer message))

and valDeclList (buffer:Stream.stream) : val_decl list =
   let rec loop acc =
      match Stream.peek buffer with
      | VAL ->
         let decl = valDecl buffer in
         let _    = Stream.consume buffer SEMI in
         loop (decl::acc)
      | _ -> List.rev acc
   in loop []

and valDecl (buffer:Stream.stream) : val_decl =
   let _         = Stream.expect buffer VAL in
   let token     = Stream.current buffer in
   let start_loc = token.loc in
   let _         = Stream.skip buffer in
   let id        = identifier buffer in
   let _         = Stream.consume buffer COLON in
   let val_type  = typeExpression 10 buffer in
   id,val_type,start_loc

(** 'while' (<expression>) <stmtList> *)
and stmtWhile (buffer:Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let _    = Stream.consume buffer WHILE in
   let _    = Stream.consume buffer LPAREN in
   let cond = expression 0 buffer in
   let _    = Stream.consume buffer RPAREN in
   let tstm = stmtList buffer in
   StmtWhile(cond,tstm,start_loc)

(** <statement> := ... *)
and stmt (buffer:Stream.stream) : stmt =
   try
      match Stream.peek buffer with
      | VAL   -> stmtVal     buffer
      | MEM   -> stmtMem     buffer
      | RET   -> stmtReturn  buffer
      | IF    -> stmtIf      buffer
      | FUN   -> stmtFunction buffer
      | AND   -> stmtFunction buffer
      | WHILE -> stmtWhile    buffer
      | TYPE  -> stmtType     buffer
      | TABLE -> stmtTab      buffer
      | _     -> stmtBind     buffer
   with
   | ParserError(error) ->
      let _ = Stream.appendError buffer error in
      let _ = moveToNextStatement buffer in
      let _ = Stream.setErrors buffer true in
      StmtEmpty

(** <statementList> := LBRACK <statement> [<statement>] RBRACK *)
and stmtList (buffer:Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let rec loop acc =
      match Stream.peek buffer with
      | RBRAC ->
         let end_loc = Stream.location buffer in
         let loc     = Location.merge start_loc end_loc in
         let _       = Stream.skip buffer in
         StmtBlock(None,List.rev acc,loc)
      | EOF ->
         let _ = Stream.expect buffer RBRAC in
         StmtBlock(None,[],start_loc)
      | _ ->
         let s = stmt buffer in
         loop (s::acc)
   in
   match Stream.peek buffer with
   | LBRAC ->
      let _ = Stream.skip buffer in
      loop []
   | _ ->
      let s = stmt buffer in
      let loc = getStmtLocation s in
      StmtBlock(None,[s],loc)

(** <statementList> :=  LSEQ <statement> [<statement>] RSEQ
    When called in exp_nud function LSEQ is already consumed *)
and pseqList (buffer:Stream.stream) : stmt list =
   let rec loop acc =
      match Stream.peek buffer with
      | RSEQ ->
         let _ = Stream.skip buffer in
         List.rev acc
      | EOF ->
         let _ = Stream.expect buffer RSEQ in
         []
      | _ ->
         let s = stmt buffer in
         loop (s::acc)
   in loop []

(** Parses an expression given a string *)
let parseExp (s:string) : exp =
   let buffer = Stream.fromString s in
   expression 0 buffer

(** Parses an statement given a string *)
let parseStmt (s:string) : stmt =
   let buffer = Stream.fromString s in
   let result = stmt buffer in
   result

(** Parses a list of statements given a string *)
let parseStmtList (s:string) : stmt =
   let buffer = Stream.fromString s in
   let result = stmtList buffer in
   result

(** Parses the given expression and prints it *)
let parseDumpExp (s:string) : string =
   let e = parseExp s in
   PrintTypes.expressionStr e

(** Parses a list of statements and prints them *)
let parseDumpStmtList (s:string) : string =
   let e = parseStmtList s in
   PrintTypes.stmtStr e

(** Parses a buffer containing a list of statements and returns the results *)
let parseBuffer (file:string) (buffer) : parser_results =
   try
      let rec loop acc =
         match Stream.peek buffer with
         | EOF -> List.rev acc
         | _ -> loop ((stmtList buffer)::acc)
      in
      let result = loop [] in
      let all_lines = getFileLines (Stream.lines buffer) in
      if Stream.hasErrors buffer then
         {
            presult = `Error(List.rev (Stream.getErrors buffer));
            lines = all_lines;
            file = file;
         }
      else
         {
            presult = `Ok(result);
            lines = all_lines;
            file = file;
         }
   with
   | ParserError(error) ->
      let all_lines = getFileLines (Stream.lines buffer) in
      {
         presult = `Error([error]);
         lines = all_lines;
         file = file;
      }
   | _ ->
      let all_lines = getFileLines (Stream.lines buffer) in
      {
         presult = `Error([Error.SimpleError("Failed to parse the file")]);
         lines = all_lines;
         file = file;
      }

(** Parses a file containing a list of statements and returns the results *)
let parseFile (filename:string) : parser_results =
   let chan = open_in filename in
   let buffer = Stream.fromChannel chan filename in
   let result = parseBuffer filename buffer in
   let _ = close_in chan in
   result

(** Parses a string containing a list of statements and returns the results *)
let parseString (text:string) : parser_results =
   let buffer = Stream.fromString text in
   let result = parseBuffer "live.vult" buffer in
   result

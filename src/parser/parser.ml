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

open Lexer
open Prog
open Ptypes
open TokenStream
open Common

module TokenKind = struct
   type kind = token_enum

   let next = next_token

   let kindStr = kindToString

   let tokenStr = tokenToString

   let isEOF x = x = EOF

   let getEOF = EOF
end

module Stream = TokenStream (TokenKind)

let splitOnDot s = CCString.Split.list_cpy "." s

(** Consumes tokens until it finds the begining of a new statememt or the end of the current *)
let rec moveToNextStatement (buffer : Stream.stream) : unit =
   match Stream.peek buffer with
   | SEMI -> Stream.skip buffer
   | EOF -> ()
   | FUN
   |VAL
   |IF
   |RET ->
      ()
   | RBRACE -> Stream.skip buffer
   | _ ->
      let _ = Stream.skip buffer in
      moveToNextStatement buffer


let getLhsExpLocation (e : lhs_exp) : Loc.t =
   match e with
   | LWild attr
   |LId (_, _, attr)
   |LTuple (_, attr)
   |LTyped (_, _, attr)
   |LGroup (_, attr)
   |LIndex (_, _, _, attr) ->
      attr.loc


(** Returns the location of an statement *)
let getStmtLocation (s : stmt) : Loc.t =
   match s with
   | StmtVal (_, _, attr)
   |StmtMem (_, _, attr)
   |StmtReturn (_, attr)
   |StmtIf (_, _, _, attr)
   |StmtFun (_, _, _, _, attr)
   |StmtBind (_, _, attr)
   |StmtBlock (_, _, attr)
   |StmtWhile (_, _, attr)
   |StmtType (_, _, attr)
   |StmtExternal (_, _, _, _, attr)
   |StmtAliasType (_, _, attr) ->
      attr.loc
   | StmtEmpty -> Loc.default


(** Returns the left binding powers of the token *)
let getLbp (token : 'kind token) : int =
   match token.kind, token.value with
   | ARROW, _ -> 10
   | COLON, _ -> 10
   | COMMA, _ -> 20
   | OP, "||" -> 30
   | OP, "&&" -> 35
   | OP, "==" -> 40
   | OP, "<>" -> 40
   | OP, ">" -> 40
   | OP, "<" -> 40
   | OP, ">=" -> 40
   | OP, "<=" -> 40
   | OP, ">>" -> 40
   | OP, "<<" -> 40
   | OP, "+" -> 50
   | OP, "|" -> 50
   | OP, "-" -> 50
   | OP, "&" -> 60
   | OP, "*" -> 60
   | OP, "/" -> 60
   | OP, "%" -> 60
   | _ -> 0


let string (buffer : Stream.stream) : string =
   let _ = Stream.expect buffer STRING in
   let token = Stream.current buffer in
   let _ = Stream.skip buffer in
   token.value


(** Creates Pratt parser functions *)
let prattParser
      (rbp : int)
      (buffer : Stream.stream)
      (lbp : 'kind token -> int)
      (nud : Stream.stream -> 'kind token -> 'exp)
      (led : Stream.stream -> 'kind token -> 'exp -> 'exp) =
   let current_token = Stream.current buffer in
   let _ = Stream.skip buffer in
   let left = nud buffer current_token in
   let next_token = Stream.current buffer in
   let rec loop token left repeat =
      if repeat then
         let _ = Stream.skip buffer in
         let new_left = led buffer token left in
         let new_token = Stream.current buffer in
         loop new_token new_left (rbp < lbp new_token)
      else
         left
   in
   loop next_token left (rbp < lbp next_token)


let identifierToken (token : 'kind token) : Id.t = splitOnDot token.value

let commaSepList parser buffer =
   let rec loop acc =
      (* power of 20 avoids returning a tuple instead of a list*)
      let e = parser 20 buffer in
      match Stream.peek buffer with
      | COMMA ->
         let _ = Stream.skip buffer in
         loop (e :: acc)
      | _ -> List.rev (e :: acc)
   in
   loop []


(** Parses tag expressions *)
let rec tagExpression (rbp : int) (buffer : Stream.stream) : tag = prattParser rbp buffer getLbp tag_nud tag_led

and tagExpressionList (buffer : Stream.stream) : tag list = commaSepList tagExpression buffer

and tag_nud (buffer : Stream.stream) (token : 'kind token) : tag =
   match token.kind, token.value with
   | ID, _
   |TABLE, _ ->
      let id = identifierToken token in
      begin
         match Stream.peek buffer with
         | LPAREN ->
            let _ = Stream.skip buffer in
            begin
               match Stream.peek buffer with
               | RPAREN ->
                  let _ = Stream.skip buffer in
                  TId (id, token.loc)
               | _ ->
                  let values = tagPairList buffer in
                  let _ = Stream.consume buffer RPAREN in
                  TFun (id, values, token.loc)
            end
         | _ -> TId (id, token.loc)
      end
   | OP, "-" -> tag_unaryOp buffer token
   | INT, _ -> TInt (token.value, token.loc)
   | TRUE, _ -> TBool (token.value, token.loc)
   | FALSE, _ -> TBool (token.value, token.loc)
   | REAL, _ -> TReal (token.value, token.loc)
   | STRING, _ -> TString (token.value, token.loc)
   | _ ->
      let message = Stream.notExpectedError token in
      raise (ParserError message)


and tag_unaryOp (buffer : Stream.stream) (token : 'kind token) : tag =
   let right = tagExpression 70 buffer in
   match right with
   | TInt (value, loc) -> TInt ("-" ^ value, loc)
   | TReal (value, loc) -> TReal ("-" ^ value, loc)
   | _ -> Error.raiseError "invalid value" token.loc


and tag_led (_ : Stream.stream) (token : 'kind token) (_ : tag) : tag =
   match token.kind with
   | _ ->
      let message = Stream.notExpectedError token in
      raise (ParserError message)


and tagPair (bp : int) (buffer : Stream.stream) : Id.t * tag =
   let id = identifierToken (Stream.current buffer) in
   let _ = Stream.skip buffer in
   let _ = Stream.consume buffer EQUAL in
   let value = tagExpression bp buffer in
   id, value


and tagPairList (buffer : Stream.stream) : (Id.t * tag) list = commaSepList tagPair buffer

let optTagExpressions (buffer : Stream.stream) : tag list =
   match Stream.peek buffer with
   | AT ->
      let _ = Stream.consume buffer AT in
      let _ = Stream.consume buffer LBRACK in
      let attr = tagExpressionList buffer in
      let _ = Stream.consume buffer RBRACK in
      attr
   | _ -> []


(** Parses a type expression using a Pratt parser *)
let rec typeExpression (rbp : int) (buffer : Stream.stream) : Typ.t = prattParser rbp buffer getLbp type_nud type_led

and type_nud (buffer : Stream.stream) (token : 'kind token) : Typ.t =
   match token.kind with
   | TICK ->
      begin
         match Stream.peek buffer with
         | ID ->
            let token = Stream.current buffer in
            let _ = Stream.skip buffer in
            begin
               match identifierToken token with
               | [ id ] -> ref (Typ.TUnbound ("'" ^ id, None, Some token.loc))
               | _ ->
                  let message = Error.makeError "invalid name for generic type" token.loc in
                  raise (ParserError message)
            end
         | _ ->
            let message = Stream.notExpectedError token in
            raise (ParserError message)
      end
   | ID ->
      let id = identifierToken token in
      begin
         match Stream.peek buffer with
         | LPAREN -> composedType buffer token id
         | _ -> ref (Typ.TId (id, Some token.loc))
      end
   | WILD -> ref (Typ.TUnbound ("", None, Some token.loc))
   | LPAREN ->
      let start_loc = token.loc in
      ( match Stream.peek buffer with
        | RPAREN ->
           let _ = Stream.skip buffer in
           ref (Typ.TId ([ "unit" ], Some start_loc))
        | _ ->
           let el = typeArgList buffer in
           begin
              match el with
              | [] -> ref (Typ.TId ([ "unit" ], Some start_loc))
              | [ tp ] -> tp
              | _ ->
                 let _ = Stream.consume buffer RPAREN in
                 ref (Typ.TComposed ([ "tuple" ], el, Some start_loc))
           end )
   | INT -> ref (Typ.TInt (int_of_string token.value, Some token.loc))
   | _ ->
      let message = Stream.notExpectedError token in
      raise (ParserError message)


and type_led (buffer : Stream.stream) (token : 'kind token) (left : Typ.t) : Typ.t =
   match token.kind with
   | ARROW ->
      let right = typeExpression 0 buffer in
      let typ = ref (Typ.TArrow (left, right, Some token.loc)) in
      let typ', _ = Typ.fixType [] typ in
      typ'
   | _ ->
      let message = Stream.notExpectedError token in
      raise (ParserError message)


and composedType (buffer : Stream.stream) (token : 'kind token) (id : Id.t) : Typ.t =
   let _ = Stream.skip buffer in
   let args =
      match Stream.peek buffer with
      | RPAREN -> []
      | _ -> typeArgList buffer
   in
   let _ = Stream.consume buffer RPAREN in
   ref (Typ.TComposed (id, args, Some token.loc))


and typeArgList (buffer : Stream.stream) : Typ.t list = commaSepList typeExpression buffer

(** Parses left hand side expression using a Pratt parser *)
let rec lhs_expression (rbp : int) (buffer : Stream.stream) : lhs_exp = prattParser rbp buffer getLbp lhs_nud lhs_led

and lhs_nud (buffer : Stream.stream) (token : 'kind token) : lhs_exp =
   match token.kind with
   | WILD -> LWild (makeAttr token.loc)
   | ID ->
      let id = identifierToken token in
      begin
         match Stream.peek buffer with
         | LBRACK ->
            let _ = Stream.skip buffer in
            let index = expression 0 buffer in
            let _ = Stream.consume buffer RBRACK in
            let attr = makeAttr token.loc in
            LIndex (id, None, index, attr)
         | _ -> LId (id, None, makeAttr token.loc)
      end
   | LPAREN ->
      begin
         match Stream.peek buffer with
         | RPAREN ->
            let message = Error.PointedError (token.loc, "Invalid left hand side of assignment") in
            raise (ParserError message)
         | _ ->
            let e = lhs_expression 0 buffer in
            let _ = Stream.consume buffer RPAREN in
            let attr = makeAttr token.loc in
            LGroup (e, attr)
      end
   | _ ->
      let message = Error.PointedError (token.loc, "Invalid left hand side of assignment") in
      raise (ParserError message)


and lhs_led (buffer : Stream.stream) (token : 'kind token) (left : lhs_exp) : lhs_exp =
   match token.kind with
   | COLON ->
      let vtype = typeExpression 0 buffer in
      LTyped (left, vtype, makeAttr token.loc)
   | COMMA -> lhs_pair buffer token left
   | _ ->
      let message = Error.PointedError (token.loc, "Invalid left hand side of assignment") in
      raise (ParserError message)


(** <pair> :=  <expression>  ',' <expression> [ ',' <expression> ] *)
and lhs_pair (buffer : Stream.stream) (token : 'kind token) (left : lhs_exp) : lhs_exp =
   let right = lhs_expression (getLbp token) buffer in
   let getElems e =
      match e with
      | LTuple (elems, _) -> elems
      | _ -> [ e ]
   in
   let elems1 = left |> getElems in
   let elems2 = right |> getElems in
   let loc = getLhsExpLocation left in
   LTuple (elems1 @ elems2, makeAttr loc)


(** Parses an expression using a Pratt parser *)
and expression (rbp : int) (buffer : Stream.stream) : exp = prattParser rbp buffer getLbp exp_nud exp_led

(** Nud function for the Pratt parser *)
and exp_nud (buffer : Stream.stream) (token : 'kind token) : exp =
   match token.kind, token.value with
   | OP, "-" ->
      (* Unary minus *)
      unaryOp buffer token
   | ID, _ ->
      (* Id or function call *)
      let id = identifierToken token in
      begin
         match Stream.peek buffer with
         | LPAREN -> functionCall buffer token id
         | LBRACK ->
            let _ = Stream.skip buffer in
            let index = expression 0 buffer in
            let _ = Stream.consume buffer RBRACK in
            let attr = makeAttr token.loc in
            PIndex (PId (id, attr), index, attr)
         | COLON ->
            let _ = Stream.skip buffer in
            let exp_call = expression 100 buffer in
            begin
               match exp_call with
               | PCall (NoInst, fname, args, attr) -> PCall (Named id, fname, args, attr)
               | _ ->
                  let loc = (GetAttr.fromExp exp_call).loc in
                  let error =
                     Error.PointedError (Loc.getNext loc, "After ':' you can only have a function call e.g. foo()")
                  in
                  raise (ParserError error)
            end
         | _ -> PId (id, makeAttr token.loc)
      end
   | LPAREN, _ ->
      let start_loc = token.loc in
      ( match Stream.peek buffer with
        | RPAREN ->
           let _ = Stream.skip buffer in
           PUnit (makeAttr start_loc)
        | _ ->
           let e = expression 0 buffer in
           let _ = Stream.consume buffer RPAREN in
           PGroup (e, makeAttr start_loc) )
   | INT, _ ->
      let attr = makeAttr token.loc in
      PInt (int_of_string token.value, attr)
   | REAL, _ ->
      let attr = makeAttr token.loc in
      PReal (float_of_string token.value, Float, attr)
   | STRING, _ ->
      let attr = makeAttr token.loc in
      PString (token.value, attr)
   | TRUE, _ ->
      let attr = makeAttr token.loc in
      PBool (true, attr)
   | FALSE, _ ->
      let attr = makeAttr token.loc in
      PBool (false, attr)
   | IF, _ ->
      let cond = expression 0 buffer in
      let _ = Stream.consume buffer THEN in
      let then_exp = expression 0 buffer in
      let _ = Stream.consume buffer ELSE in
      let else_exp = expression 0 buffer in
      let attr = makeAttr token.loc in
      PIf (cond, then_exp, else_exp, attr)
   | LSEQ, _ ->
      let stmts = pseqList buffer in
      let attr = makeAttr token.loc in
      PSeq (None, StmtBlock (None, stmts, attr), attr)
   | LBRACK, _ ->
      let start_loc = token.loc in
      ( match Stream.peek buffer with
        | RBRACK ->
           let attr = makeAttr start_loc in
           let _ = Stream.consume buffer RBRACK in
           PArray ([||], attr)
        | _ ->
           let start_loc = token.loc in
           let elems = expressionList buffer in
           let _ = Stream.consume buffer RBRACK in
           let attr = makeAttr start_loc in
           PArray (Array.of_list elems, attr) )
   | _ ->
      let message = Error.PointedError (token.loc, "Invalid expression") in
      raise (ParserError message)


(** Led function for the Pratt parser *)
and exp_led (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
   match token.kind, token.value with
   | OP, _ ->
      (* Binary operators *)
      binaryOp buffer token left
   | COMMA, _ -> pair buffer token left
   | _ ->
      let message = Error.PointedError (token.loc, "Invalid expression") in
      raise (ParserError message)


(** <pair> :=  <expression>  ',' <expression> [ ',' <expression> ] *)
and pair (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
   let right = expression (getLbp token) buffer in
   let getElems e =
      match e with
      | PTuple (elems, _) -> elems
      | _ -> [ e ]
   in
   let elems1 = left |> getElems in
   let elems2 = right |> getElems in
   let start_loc = (GetAttr.fromExp left).loc in
   let attr = makeAttr start_loc in
   PTuple (elems1 @ elems2, attr)


(** <functionCall> := <id> '(' <expressionList> ')' *)
and functionCall (buffer : Stream.stream) (token : 'kind token) (id : Id.t) : exp =
   let _ = Stream.skip buffer in
   let args =
      match Stream.peek buffer with
      | RPAREN -> []
      | _ -> expressionList buffer
   in
   let _ = Stream.consume buffer RPAREN in
   let attr = makeAttr token.loc in
   PCall (NoInst, id, args, attr)


(** <unaryOp> := OP <expression> *)
and unaryOp (buffer : Stream.stream) (token : 'kind token) : exp =
   let right = expression 70 buffer in
   let attr = makeAttr token.loc in
   PUnOp (token.value, right, attr)


(** <binaryOp> := <expression> OP <expression> *)
and binaryOp (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
   let right = expression (getLbp token) buffer in
   let attr = makeAttr token.loc in
   POp (token.value, [ left; right ], attr)


(** <expressionList> := <expression> [',' <expression> ] *)
and expressionList (buffer : Stream.stream) : exp list = commaSepList expression buffer

(** typedArg := <ID> [ ':' <ID>]  *)
and typedArgOpt (buffer : Stream.stream) : typed_id =
   let _ = Stream.expect buffer ID in
   let token = Stream.current buffer in
   let _ = Stream.skip buffer in
   match Stream.peek buffer with
   | COLON ->
      let _ = Stream.skip buffer in
      let e = typeExpression 20 buffer in
      let attr = makeAttr token.loc in
      TypedId (splitOnDot token.value, [ e ], InputArg, attr)
   | _ ->
      let attr = makeAttr token.loc in
      SimpleId (splitOnDot token.value, InputArg, attr)


(** typedArg := <ID> [ ':' <ID>]  *)
and typedArg (buffer : Stream.stream) : typed_id =
   let _ = Stream.expect buffer ID in
   let token = Stream.current buffer in
   let _ = Stream.skip buffer in
   let _ = Stream.consume buffer COLON in
   let e = typeExpression 20 buffer in
   let attr = makeAttr token.loc in
   TypedId (splitOnDot token.value, [ e ], InputArg, attr)


and id (buffer : Stream.stream) : Id.t =
   let _ = Stream.expect buffer ID in
   let token = Stream.current buffer in
   let _ = Stream.skip buffer in
   identifierToken token


(** typedArgList := typedArg [',' typedArg ] *)
and typedArgList (optional_type : bool) (buffer : Stream.stream) : typed_id list =
   match Stream.peek buffer with
   | ID ->
      let fn = if optional_type then typedArgOpt else typedArg in
      let first = fn buffer in
      begin
         match Stream.peek buffer with
         | COMMA ->
            let _ = Stream.consume buffer COMMA in
            first :: typedArgList optional_type buffer
         | _ -> [ first ]
      end
   | _ -> []


(** <statement> := | 'val' <valBindList> ';' *)
and stmtVal (buffer : Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let _ = Stream.consume buffer VAL in
   let lhs = lhs_expression 0 buffer in
   (* TODO: Add check of lhs *)
   match Stream.peek buffer with
   | EQUAL ->
      let _ = Stream.skip buffer in
      let rhs = expression 0 buffer in
      let _ = Stream.consume buffer SEMI in
      let attr = makeAttr start_loc in
      StmtVal (lhs, Some rhs, attr)
   | _ ->
      let _ = Stream.consume buffer SEMI in
      let attr = makeAttr start_loc in
      StmtVal (lhs, None, attr)


(** <statement> := | 'mem' <valBindList> ';' *)
and stmtMem (buffer : Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let _ = Stream.consume buffer MEM in
   let lhs = lhs_expression 0 buffer in
   (* TODO: Add check of lhs *)
   match Stream.peek buffer with
   | EQUAL ->
      let _ = Stream.skip buffer in
      let rhs = expression 0 buffer in
      let _ = Stream.consume buffer SEMI in
      let attr = makeAttr start_loc in
      StmtMem (lhs, Some rhs, attr)
   | _ ->
      let _ = Stream.consume buffer SEMI in
      let attr = makeAttr start_loc in
      StmtMem (lhs, None, attr)


(** <statement> := | 'return' <expression> ';' *)
and stmtReturn (buffer : Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let _ = Stream.consume buffer RET in
   let e = expression 0 buffer in
   let _ = Stream.consume buffer SEMI in
   StmtReturn (e, makeAttr start_loc)


and stmtBind (buffer : Stream.stream) : stmt =
   let e1 = lhs_expression 0 buffer in
   let start_loc = getLhsExpLocation e1 in
   match Stream.peek buffer with
   | EQUAL ->
      let _ = Stream.consume buffer EQUAL in
      let e2 = expression 0 buffer in
      let _ = Stream.consume buffer SEMI in
      StmtBind (e1, e2, makeAttr start_loc)
   | _ ->
      let message =
         Printf.sprintf "Invalid statement. All statements should be in the forms: \"a = b; \" or \"_ = b(); \" "
      in
      raise (ParserError (Stream.makeError buffer message))


(** <statement> := 'if' '(' <expression> ')' <statementList> ['else' <statementList> ]*)
and stmtIf (buffer : Stream.stream) : stmt =
   let _ = Stream.consume buffer IF in
   let _ = Stream.consume buffer LPAREN in
   let cond = expression 0 buffer in
   let _ = Stream.consume buffer RPAREN in
   let tstm = stmtList buffer in
   let start_loc = (GetAttr.fromExp cond).loc in
   match Stream.peek buffer with
   | ELSE ->
      let _ = Stream.consume buffer ELSE in
      let fstm = stmtList buffer in
      StmtIf (cond, tstm, Some fstm, makeAttr start_loc)
   | _ -> StmtIf (cond, tstm, None, makeAttr start_loc)


(** 'external' <id> '(' <typedArgList> ')' ':' type *)
and stmtExternal (buffer : Stream.stream) : stmt =
   let _ = Stream.skip buffer in
   let name = id buffer in
   let token = Stream.current buffer in
   let _ = Stream.consume buffer LPAREN in
   let args =
      match Stream.peek buffer with
      | RPAREN -> []
      | _ -> typedArgList false buffer
   in
   let _ = Stream.consume buffer RPAREN in
   let _ = Stream.consume buffer COLON in
   let vtype = typeExpression 0 buffer in
   let link_name, tags =
      match Stream.peek buffer with
      | STRING ->
         let link_name = string buffer in
         let tag_exp = optTagExpressions buffer in
         Some link_name, tag_exp
      | AT ->
         let tag = optTagExpressions buffer in
         None, tag
      | _ ->
         let message = Printf.sprintf "Expecting a string with a link name or a tag" in
         raise (ParserError (Stream.makeError buffer message))
   in
   let _ = Stream.consume buffer SEMI in
   let start_loc = token.loc in
   let attr = { (makeAttr start_loc) with ext_fn = link_name; tags } in
   StmtExternal (name, args, vtype, link_name, attr)


(** 'fun' <id> '(' <typedArgList> ')' [ ':' type ] <stmtList> *)

(** 'fun' <id> '(' <typedArgList> ')' [ ':' type ] <stmtList> *)
and stmtFunction (buffer : Stream.stream) : stmt =
   let isjoin =
      match Stream.peek buffer with
      | AND -> true
      | _ -> false
   in
   let _ = Stream.skip buffer in
   let start_loc = (Stream.current buffer).loc in
   let name = id buffer in
   let _ = Stream.consume buffer LPAREN in
   let args =
      match Stream.peek buffer with
      | RPAREN -> []
      | _ -> typedArgList true buffer
   in
   let _ = Stream.consume buffer RPAREN in
   let vtype =
      match Stream.peek buffer with
      | COLON ->
         let _ = Stream.skip buffer in
         Some (typeExpression 0 buffer)
      | _ -> None
   in
   let tags = optTagExpressions buffer in
   let body = stmtList buffer in
   let attr = { emptyAttr with loc = start_loc; tags } in
   let attr = if isjoin then { attr with fun_and = true } else attr in
   StmtFun (name, args, body, vtype, attr)


(** 'type' <id> '(' <typedArgList> ')' <valDeclList> *)
and stmtType (buffer : Stream.stream) : stmt =
   let _ = Stream.consume buffer TYPE in
   let token = Stream.current buffer in
   let start_loc = token.loc in
   let type_name = typeExpression 10 buffer in
   match Stream.peek buffer with
   | COLON ->
      let _ = Stream.skip buffer in
      let vtype = typeExpression 10 buffer in
      let _ = Stream.optConsume buffer SEMI in
      StmtAliasType (type_name, vtype, makeAttr start_loc)
   | LBRACE ->
      let _ = Stream.skip buffer in
      let val_decl = valDeclList buffer in
      let _ = Stream.consume buffer RBRACE in
      StmtType (type_name, val_decl, makeAttr start_loc)
   | _ ->
      let got = tokenToString (Stream.current buffer) in
      let message =
         Printf.sprintf "Expecting a list of value declarations '{ val x:... }' or a type alias ': type' but got %s" got
      in
      raise (ParserError (Stream.makeError buffer message))


and valDeclList (buffer : Stream.stream) : val_decl list =
   let rec loop acc =
      match Stream.peek buffer with
      | VAL ->
         let decl = valDecl buffer in
         let _ = Stream.consume buffer SEMI in
         loop (decl :: acc)
      | _ -> List.rev acc
   in
   loop []


and valDecl (buffer : Stream.stream) : val_decl =
   let _ = Stream.expect buffer VAL in
   let token = Stream.current buffer in
   let start_loc = token.loc in
   let _ = Stream.skip buffer in
   let id = id buffer in
   let _ = Stream.consume buffer COLON in
   let val_type = typeExpression 10 buffer in
   id, [ val_type ], makeAttr start_loc


(** 'while' (<expression>) <stmtList> *)
and stmtWhile (buffer : Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let _ = Stream.consume buffer WHILE in
   let _ = Stream.consume buffer LPAREN in
   let cond = expression 0 buffer in
   let _ = Stream.consume buffer RPAREN in
   let tstm = stmtList buffer in
   StmtWhile (cond, tstm, makeAttr start_loc)


and stmtTable (buffer : Stream.stream) : stmt =
   let e1 = lhs_expression 0 buffer in
   let start_loc = getLhsExpLocation e1 in
   match Stream.peek buffer with
   | EQUAL ->
      let _ = Stream.consume buffer EQUAL in
      let e2 = expression 0 buffer in
      let _ = Stream.consume buffer SEMI in
      StmtBind (e1, e2, { (makeAttr start_loc) with const = true })
   | _ ->
      let message =
         Printf.sprintf "Invalid statement. All statements should be in the forms: \"a = b; \" or \"_ = b(); \" "
      in
      raise (ParserError (Stream.makeError buffer message))


(** <statement> := ... *)
and stmt (buffer : Stream.stream) : stmt =
   try
      match Stream.peek buffer with
      | VAL -> stmtVal buffer
      | MEM -> stmtMem buffer
      | RET -> stmtReturn buffer
      | IF -> stmtIf buffer
      | FUN -> stmtFunction buffer
      | AND -> stmtFunction buffer
      | WHILE -> stmtWhile buffer
      | TYPE -> stmtType buffer
      | TABLE -> stmtTable buffer
      | EXTERNAL -> stmtExternal buffer
      | _ -> stmtBind buffer
   with
   | ParserError error ->
      let _ = Stream.appendError buffer error in
      let _ = moveToNextStatement buffer in
      let _ = Stream.setErrors buffer true in
      StmtEmpty


(** <statementList> := LBRAC <statement> [<statement>] RBRAC *)
and stmtList (buffer : Stream.stream) : stmt =
   let start_loc = Stream.location buffer in
   let rec loop acc =
      match Stream.peek buffer with
      | RBRACE ->
         let end_loc = Stream.location buffer in
         let loc = Loc.merge start_loc end_loc in
         let _ = Stream.skip buffer in
         StmtBlock (None, List.rev acc, makeAttr loc)
      | EOF ->
         let _ = Stream.expect buffer RBRACE in
         StmtBlock (None, [], makeAttr start_loc)
      | _ ->
         let s = stmt buffer in
         loop (s :: acc)
   in
   match Stream.peek buffer with
   | LBRACE ->
      let _ = Stream.skip buffer in
      loop []
   | _ ->
      let s = stmt buffer in
      let loc = getStmtLocation s in
      StmtBlock (None, [ s ], makeAttr loc)


(** <statementList> :=  LSEQ <statement> [<statement>] RSEQ
    When called in exp_nud function LSEQ is already consumed *)
and pseqList (buffer : Stream.stream) : stmt list =
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
         loop (s :: acc)
   in
   loop []


(** Parses an lhs-expression given a string *)
let parseLhsExp (s : string) : lhs_exp =
   let buffer = Stream.fromString s in
   lhs_expression 0 buffer


(** Parses an expression given a string *)
let parseExp (s : string) : exp =
   let buffer = Stream.fromString s in
   expression 0 buffer


(** Parses an id given a string *)
let parseId (s : string) : Id.t =
   let buffer = Stream.fromString s in
   id buffer


(** Parses an type given a string *)
let parseType (s : string) : Typ.t =
   let buffer = Stream.fromString s in
   typeExpression 0 buffer


(** Parses an statement given a string *)
let parseStmt (s : string) : stmt =
   let buffer = Stream.fromString s in
   let result = stmt buffer in
   result


(** Parses a list of statements given a string *)
let parseStmtList (s : string) : stmt =
   let buffer = Stream.fromString s in
   let result = stmtList buffer in
   result


(** Parses the given expression and prints it *)
let parseDumpExp (s : string) : string =
   let e = parseExp s in
   PrintProg.expressionStr e


(** Parses a list of statements and prints them *)
let parseDumpStmtList (s : string) : string =
   let e = parseStmtList s in
   PrintProg.stmtStr e


(** Parses a buffer containing a list of statements and returns the results *)
let parseBuffer (file : string) (buffer : Stream.stream) : parser_results =
   try
      let rec loop acc =
         match Stream.peek buffer with
         | EOF -> List.rev acc
         | _ -> loop (stmt buffer :: acc)
      in
      let result = loop [] in
      if Stream.hasErrors buffer then
         raise (Error.Errors (List.rev (Stream.getErrors buffer)))
      else
         { presult = result; file }
   with
   | ParserError error -> raise (Error.Errors [ error ])
   | Error.Errors _ as e -> raise e


(** Parses a file containing a list of statements and returns the results *)
let parseFile (filename : string) : parser_results =
   match FileIO.read filename with
   | Some contents ->
      let buffer = Stream.fromString ~file:filename contents in
      let result = parseBuffer filename buffer in
      result
   | None -> Error.raiseErrorMsg ("Could not open the file " ^ filename)


(** Parses a string containing a list of statements and returns the results *)
let parseString (file : string option) (text : string) : parser_results =
   let buffer =
      match file with
      | Some f -> Stream.fromString ~file:f text
      | None -> Stream.fromString text
   in
   let result = parseBuffer "live.vult" buffer in
   result

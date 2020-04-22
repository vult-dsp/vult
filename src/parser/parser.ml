(*
   The MIT License (MIT)

   Copyright (c) 2020 Leonardo Laguna Ruiz

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
open Ptypes
open TokenStream
open Syntax

module TokenKind = struct
  type kind = token_enum

  let next = next_token

  let kindStr = kindToString

  let tokenStr = tokenToString

  let isEOF x = x = EOF

  let getEOF = EOF
end

(*
exception LCall of Id.t * exp list * attr
*)
module Stream = TokenStream (TokenKind)

type parsed_file =
  { file : string
  ; name : string
  ; stmts : top_stmt list
  }

let rec moveToNextTopStatement (buffer : Stream.stream) : unit =
  match Stream.peek buffer with
  | EOF -> ()
  | FUN
   |TYPE
   |EXTERNAL ->
      ()
  | _ ->
      let _ = Stream.skip buffer in
      moveToNextTopStatement buffer


let rec moveToNextStatement (buffer : Stream.stream) : unit =
  match Stream.peek buffer with
  | SEMI -> Stream.skip buffer
  | EOF -> ()
  | VAL
   |IF
   |RET ->
      ()
  | RBRACE -> Stream.skip buffer
  | _ ->
      let _ = Stream.skip buffer in
      moveToNextStatement buffer


let rec expToPath error (exp : exp) : path =
  match exp with
  | { e = SEId id; loc } -> { id; n = None; loc }
  | { e = SEMember ({ e = SEId e }, id); loc } -> { id; n = Some e; loc }
  | _ -> error ()


(** Returns the left binding powers of the token *)
let getExpLbp (token : 'kind token) : int =
  match token.kind, token.value with
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
  | COLON, _ -> 70
  | LPAREN, _ -> 80
  | LBRACK, _ -> 80
  | DOT, _ -> 90
  | _ -> 0


let getLExpLbp (token : 'kind token) : int =
  match token.kind, token.value with
  | COMMA, _ -> 20
  | LPAREN, _ -> 80
  | LBRACK, _ -> 80
  | DOT, _ -> 90
  | COLON, _ -> 90
  | _ -> 0


let string (buffer : Stream.stream) : string =
  let _ = Stream.expect buffer STRING in
  let token = Stream.current buffer in
  let _ = Stream.skip buffer in
  token.value


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


let id_name (buffer : Stream.stream) : string * Loc.t =
  let () = Stream.expect buffer ID in
  let token = Stream.current buffer in
  let () = Stream.skip buffer in
  token.value, token.loc


let int_value (buffer : Stream.stream) : int * Loc.t =
  let () = Stream.expect buffer INT in
  let token = Stream.current buffer in
  let () = Stream.skip buffer in
  int_of_string token.value, token.loc


(** Parses tag expressions *)
let rec tag (rbp : int) (buffer : Stream.stream) : Tags.tag = prattParser rbp buffer getExpLbp tag_nud tag_led

and tagExpressionList (buffer : Stream.stream) : Tags.tag list = commaSepList tag buffer

and tag_nud (buffer : Stream.stream) (token : 'kind token) : Tags.tag =
  let loc = token.loc in
  match token.kind, token.value with
  | ID, _ ->
      let name = token.value in
      begin
        match Stream.peek buffer with
        | LPAREN ->
            let _ = Stream.skip buffer in
            begin
              match Stream.peek buffer with
              | RPAREN ->
                  let _ = Stream.skip buffer in
                  { g = TagId name; loc }
              | _ ->
                  let args = tag_pair_list buffer in
                  let _ = Stream.consume buffer RPAREN in
                  { g = TagCall { name; args }; loc }
            end
        | _ -> { g = TagId name; loc }
      end
  | OP, "-" -> tag_unary_op buffer token
  | INT, _ -> { g = TagInt (int_of_string token.value); loc }
  | TRUE, _ -> { g = TagBool true; loc }
  | FALSE, _ -> { g = TagBool false; loc }
  | REAL, _ -> { g = TagReal (float_of_string token.value); loc }
  | STRING, _ -> { g = TagString token.value; loc }
  | _ ->
      let message = Stream.notExpectedError token in
      raise (ParserError message)


and tag_unary_op (buffer : Stream.stream) (token : 'kind token) : Tags.tag =
  let right = tag 70 buffer in
  match right.g with
  | TagInt value -> { right with g = TagInt (-value) }
  | TagReal value -> { right with g = TagReal (-.value) }
  | _ -> Error.raiseError "invalid value" token.loc


and tag_led (_ : Stream.stream) (token : 'kind token) (_ : Tags.tag) : Tags.tag =
  match token.kind with
  | _ ->
      let message = Stream.notExpectedError token in
      raise (ParserError message)


and tag_pair (bp : int) (buffer : Stream.stream) : string * Tags.tag * Loc.t =
  let id, loc = id_name buffer in
  let _ = Stream.consume buffer EQUAL in
  let value = tag bp buffer in
  id, value, loc


and tag_pair_list (buffer : Stream.stream) : (string * Tags.tag * Loc.t) list = commaSepList tag_pair buffer

let optional_tag (buffer : Stream.stream) : Tags.tag list =
  match Stream.peek buffer with
  | AT ->
      let _ = Stream.consume buffer AT in
      let _ = Stream.consume buffer LBRACK in
      let attr = tagExpressionList buffer in
      let _ = Stream.consume buffer RBRACK in
      attr
  | _ -> []


let rec type_ (rbp : int) (buffer : Stream.stream) : type_ = prattParser rbp buffer getExpLbp type_nud type_led

(** Nud function for the Pratt parser *)
and type_nud (buffer : Stream.stream) (token : 'kind token) : type_ =
  match token.kind, token.value with
  | ID, _ ->
      let id = token.value in
      let loc = token.loc in
      { t = STId { id; n = None; loc }; loc }
  | INT, _ ->
      let loc = token.loc in
      { t = STSize (int_of_string token.value); loc }
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid type") in
      raise (ParserError message)


and type_led (buffer : Stream.stream) (token : 'kind token) (left : type_) : type_ =
  match token.kind with
  | DOT -> type_member buffer token left
  | LPAREN -> type_call buffer token left
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid expression") in
      raise (ParserError message)


and type_member (buffer : Stream.stream) (token : 'kind token) (left : type_) : type_ =
  let right = type_ (getExpLbp token) buffer in
  match right.t, left.t with
  | STId rpath, STId { id; n = None } -> { right with t = STId { rpath with n = Some id } }
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid expression") in
      raise (ParserError message)


and type_call (buffer : Stream.stream) (token : 'kind token) (left : type_) : type_ =
  let path =
    match left.t with
    | STId { id } -> id
    | _ -> failwith "invalid composed type"
  in
  let args =
    match Stream.peek buffer with
    | RPAREN -> []
    | _ -> type_list buffer
  in
  let _ = Stream.consume buffer RPAREN in
  let loc = token.loc in
  { t = STComposed (path, args); loc }


and type_list (buffer : Stream.stream) : type_ list = commaSepList type_ buffer

let rec dexp_expression (rbp : int) (buffer : Stream.stream) : dexp =
  prattParser rbp buffer getLExpLbp dexp_nud dexp_led


and dexp_nud (buffer : Stream.stream) (token : 'kind token) : dexp =
  match token.kind with
  | WILD -> { d = SDWild; loc = token.loc }
  | ID ->
      let id = token.value in
      { d = SDId (id, None); loc = token.loc }
  | LPAREN ->
      begin
        match Stream.peek buffer with
        | RPAREN ->
            let message = Error.PointedError (token.loc, "Invalid declaration of variables") in
            raise (ParserError message)
        | _ ->
            let e = dexp_expression 0 buffer in
            let _ = Stream.consume buffer RPAREN in
            let loc = token.loc in
            { d = SDGroup e; loc }
      end
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid left hand side of assignment") in
      raise (ParserError message)


and dexp_led (buffer : Stream.stream) (token : 'kind token) (left : dexp) : dexp =
  match token.kind with
  | COMMA -> dpair buffer token left
  | LBRACK -> darray buffer token left
  | COLON -> dtyped buffer token left
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid left hand side of assignment") in
      raise (ParserError message)


and dpair (buffer : Stream.stream) (token : 'kind token) (left : dexp) : dexp =
  let right = dexp_expression (getLExpLbp token) buffer in
  let getElems e =
    match e.d with
    | SDTuple elems -> elems
    | _ -> [ e ]
  in
  let elems1 = left |> getElems in
  let elems2 = right |> getElems in
  { d = SDTuple (elems1 @ elems2); loc = left.loc }


and dtyped (buffer : Stream.stream) (token : 'kind token) (left : dexp) : dexp =
  let right = type_ 0 buffer in
  { d = SDTyped (left, right); loc = token.loc }


and darray (buffer : Stream.stream) (token : 'kind token) (left : dexp) : dexp =
  let size, _ = int_value buffer in
  let () = Stream.consume buffer RBRACK in
  match left with
  | { d = SDId (id, None) } -> { d = SDId (id, Some size); loc = token.loc }
  | { d = SDId (_, Some _) } -> failwith "double dimmensions"
  | _ -> failwith "cannot apply dimmensions to this declaration"


let rec lexp_expression (rbp : int) (buffer : Stream.stream) : lexp =
  prattParser rbp buffer getLExpLbp lexp_nud lexp_led


and lexp_nud (buffer : Stream.stream) (token : 'kind token) : lexp =
  match token.kind with
  | WILD -> { l = SLWild; loc = token.loc }
  | ID ->
      let id = token.value in
      { l = SLId id; loc = token.loc }
  | LPAREN ->
      begin
        match Stream.peek buffer with
        | RPAREN ->
            let message = Error.PointedError (token.loc, "Invalid left hand side of assignment") in
            raise (ParserError message)
        | _ ->
            let e = lexp_expression 0 buffer in
            let _ = Stream.consume buffer RPAREN in
            { l = SLGroup e; loc = token.loc }
      end
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid left hand side of assignment") in
      raise (ParserError message)


and lexp_led (buffer : Stream.stream) (token : 'kind token) (left : lexp) : lexp =
  match token.kind with
  | COMMA -> lhs_pair buffer token left
  | DOT -> lexp_member buffer token left
  | LBRACK -> lexp_index buffer token left
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid left hand side of assignment") in
      raise (ParserError message)


and lexp_member (buffer : Stream.stream) (token : 'kind token) (left : lexp) : lexp =
  let right = lexp_expression (getLExpLbp token) buffer in
  match right.l with
  | SLMember (({ l = SLId id; _ } as i), n) -> { right with l = SLMember ({ i with l = SLMember (left, id) }, n) }
  | SLId id -> { right with l = SLMember (left, id) }
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid expression") in
      raise (ParserError message)


and lhs_pair (buffer : Stream.stream) (token : 'kind token) (left : lexp) : lexp =
  let right = lexp_expression (getLExpLbp token) buffer in
  let getElems e =
    match e.l with
    | SLTuple elems -> elems
    | _ -> [ e ]
  in
  let elems1 = left |> getElems in
  let elems2 = right |> getElems in
  { l = SLTuple (elems1 @ elems2); loc = left.loc }


and lexp_index (buffer : Stream.stream) (token : 'kind token) (left : lexp) : lexp =
  let index = expression 0 buffer in
  let _ = Stream.consume buffer RBRACK in
  { l = SLIndex { e = left; index }; loc = token.loc }


and expression (rbp : int) (buffer : Stream.stream) : exp = prattParser rbp buffer getExpLbp exp_nud exp_led

(** Nud function for the Pratt parser *)
and exp_nud (buffer : Stream.stream) (token : 'kind token) : exp =
  let loc = token.loc in
  match token.kind, token.value with
  | OP, "-" -> unaryOp buffer token
  | ID, _ ->
      let id = token.value in
      { e = SEId id; loc }
  | LPAREN, _ ->
      begin
        match Stream.peek buffer with
        | RPAREN ->
            let _ = Stream.skip buffer in
            { e = SEUnit; loc }
        | _ ->
            let e = expression 0 buffer in
            let _ = Stream.consume buffer RPAREN in
            { e = SEGroup e; loc }
      end
  | INT, _ -> { e = SEInt (int_of_string token.value); loc }
  | REAL, _ -> { e = SEReal (float_of_string token.value); loc }
  | STRING, _ -> { e = SEString token.value; loc }
  | TRUE, _ -> { e = SEBool true; loc }
  | FALSE, _ -> { e = SEBool false; loc }
  | IF, _ ->
      let cond = expression 0 buffer in
      let _ = Stream.consume buffer THEN in
      let then_ = expression 0 buffer in
      let _ = Stream.consume buffer ELSE in
      let else_ = expression 0 buffer in
      { e = SEIf { cond; then_; else_ }; loc }
  | LBRACK, _ ->
      begin
        match Stream.peek buffer with
        | RBRACK ->
            let _ = Stream.consume buffer RBRACK in
            { e = SEArray []; loc }
        | _ ->
            let elems = expressionList buffer in
            let _ = Stream.consume buffer RBRACK in
            { e = SEArray elems; loc }
      end
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid expression") in
      raise (ParserError message)


and exp_led (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  match token.kind with
  | OP -> binary_op buffer token left
  | COMMA -> pair buffer token left
  | DOT -> exp_member buffer token left
  | LPAREN -> call buffer token left
  | COLON -> named_call buffer token left
  | LBRACK -> exp_index buffer token left
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid expression") in
      raise (ParserError message)


and exp_member (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let right = expression (getExpLbp token) buffer in
  match right.e with
  | SEMember (({ e = SEId id; _ } as i), n) -> { right with e = SEMember ({ i with e = SEMember (left, id) }, n) }
  | SEId id -> { right with e = SEMember (left, id) }
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid expression") in
      raise (ParserError message)


and pair (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let right = expression (getExpLbp token) buffer in
  let getElems e =
    match e.e with
    | SETuple elems -> elems
    | _ -> [ e ]
  in
  let elems1 = left |> getElems in
  let elems2 = right |> getElems in
  { e = SETuple (elems1 @ elems2); loc = left.loc }


and named_call (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let right = expression (getExpLbp token) buffer in
  match left, right with
  | { e = SEId name; _ }, { e = SECall ({ instance = None; _ } as call); _ } ->
      { right with e = SECall { call with instance = Some name } }
  | { e = SEId _; _ }, _ ->
      let loc = right.loc in
      let error = Error.PointedError (Loc.getNext loc, "After ':' you can only have a function call e.g. name:foo()") in
      raise (ParserError error)
  | _, { e = SECall { instance = None; _ }; _ } ->
      let loc = left.loc in
      let error =
        Error.PointedError (Loc.getNext loc, "Instance names for functions must be simple identifier e.g. name:foo()")
      in
      raise (ParserError error)
  | _, { e = SECall { instance = Some _; _ }; _ } ->
      let loc = left.loc in
      let error = Error.PointedError (Loc.getNext loc, "This cannot be applied to an already named function") in
      raise (ParserError error)
  | _ ->
      let loc = Loc.merge left.loc right.loc in
      let error =
        Error.PointedError
          ( Loc.getNext loc
          , "This is an invalid function call. Did you missed an operator between the expression? e.g. a + (b)" )
      in
      raise (ParserError error)


and call (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let error () =
    let message = Error.PointedError (left.loc, "This is not a valid function name") in
    raise (ParserError message)
  in
  let path = expToPath error left in
  let args =
    match Stream.peek buffer with
    | RPAREN -> []
    | _ -> expressionList buffer
  in
  let _ = Stream.consume buffer RPAREN in
  { e = SECall { instance = None; path; args }; loc = token.loc }


and exp_index (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let index = expression 0 buffer in
  let _ = Stream.consume buffer RBRACK in
  { e = SEIndex { e = left; index }; loc = token.loc }


and unaryOp (buffer : Stream.stream) (token : 'kind token) : exp =
  let right = expression 70 buffer in
  { e = SEUnOp (token.value, right); loc = token.loc }


and binary_op (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let right = expression (getExpLbp token) buffer in
  { e = SEOp (token.value, left, right); loc = token.loc }


and expressionList (buffer : Stream.stream) : exp list = commaSepList expression buffer

and stmtVal (buffer : Stream.stream) : stmt =
  let loc = Stream.location buffer in
  let _ = Stream.consume buffer VAL in
  let lhs = dexp_expression 0 buffer in
  match Stream.peek buffer with
  | EQUAL ->
      let _ = Stream.skip buffer in
      let rhs = expression 0 buffer in
      let _ = Stream.consume buffer SEMI in
      { s = SStmtVal (lhs, Some rhs); loc }
  | _ ->
      let _ = Stream.consume buffer SEMI in
      { s = SStmtVal (lhs, None); loc }


and stmtMem (buffer : Stream.stream) : stmt =
  let loc = Stream.location buffer in
  let _ = Stream.consume buffer MEM in
  let lhs = dexp_expression 0 buffer in
  match Stream.peek buffer with
  | EQUAL ->
      let _ = Stream.skip buffer in
      let rhs = expression 0 buffer in
      let tags = optional_tag buffer in
      let _ = Stream.consume buffer SEMI in
      { s = SStmtMem (lhs, Some rhs, tags); loc }
  | _ ->
      let tags = optional_tag buffer in
      let _ = Stream.consume buffer SEMI in
      { s = SStmtMem (lhs, None, tags); loc }


and stmtReturn (buffer : Stream.stream) : stmt =
  let loc = Stream.location buffer in
  let _ = Stream.consume buffer RET in
  let e = expression 0 buffer in
  let _ = Stream.consume buffer SEMI in
  { s = SStmtReturn e; loc }


and stmtBind (buffer : Stream.stream) : stmt =
  match lexp_expression 0 buffer with
  | e1 ->
      let loc = e1.loc in
      begin
        match Stream.peek buffer with
        | EQUAL ->
            let _ = Stream.consume buffer EQUAL in
            let e2 = expression 0 buffer in
            let _ = Stream.consume buffer SEMI in
            { s = SStmtBind (e1, e2); loc }
        | _ ->
            let message =
              Printf.sprintf "Invalid statement. All statements should be in the forms: \"a = b; \" or \"_ = b(); \" "
            in
            raise (ParserError (Stream.makeError buffer message))
      end


and stmtIf (buffer : Stream.stream) : stmt =
  let _ = Stream.consume buffer IF in
  let _ = Stream.consume buffer LPAREN in
  let cond = expression 0 buffer in
  let _ = Stream.consume buffer RPAREN in
  let tstm = stmtList buffer in
  let loc = cond.loc in
  match Stream.peek buffer with
  | ELSE ->
      let _ = Stream.consume buffer ELSE in
      let fstm = stmtList buffer in
      { s = SStmtIf (cond, tstm, Some fstm); loc }
  | _ -> { s = SStmtIf (cond, tstm, None); loc }


and typedArgOpt (buffer : Stream.stream) =
  let _ = Stream.expect buffer ID in
  let token = Stream.current buffer in
  let _ = Stream.skip buffer in
  match Stream.peek buffer with
  | COLON ->
      let _ = Stream.skip buffer in
      let e = type_ 20 buffer in
      token.value, Some e, token.loc
  | _ -> token.value, None, token.loc


and typedArg (buffer : Stream.stream) =
  let _ = Stream.expect buffer ID in
  let token = Stream.current buffer in
  let _ = Stream.skip buffer in
  let _ = Stream.consume buffer COLON in
  let e = type_ 20 buffer in
  token.value, Some e, token.loc


and argList arg_parser (buffer : Stream.stream) =
  match Stream.peek buffer with
  | ID ->
      let first = arg_parser buffer in
      begin
        match Stream.peek buffer with
        | COMMA ->
            let _ = Stream.consume buffer COMMA in
            first :: argList arg_parser buffer
        | _ -> [ first ]
      end
  | _ -> []


and stmtExternal (buffer : Stream.stream) : top_stmt =
  let _ = Stream.skip buffer in
  let name, loc = id_name buffer in
  let _ = Stream.consume buffer LPAREN in
  let args =
    match Stream.peek buffer with
    | RPAREN -> []
    | _ -> argList typedArg buffer
  in
  let _ = Stream.consume buffer RPAREN in
  let _ = Stream.consume buffer COLON in
  let type_ = type_ 0 buffer in
  let link_name, tags =
    match Stream.peek buffer with
    | STRING ->
        let link_name = string buffer in
        let tags = optional_tag buffer in
        Some link_name, tags
    | AT ->
        let tags = optional_tag buffer in
        None, tags
    | _ ->
        let message = Printf.sprintf "Expecting a string with a link name or a tag" in
        raise (ParserError (Stream.makeError buffer message))
  in
  let _ = Stream.consume buffer SEMI in
  { top = STopExternal ({ name; args; t = Some type_; tags; next = None; loc }, link_name); loc }


and stmtFunctionDecl (buffer : Stream.stream) : function_def * stmt * Loc.t =
  let _ = Stream.skip buffer in
  let name, loc = id_name buffer in
  let _ = Stream.consume buffer LPAREN in
  let args =
    match Stream.peek buffer with
    | RPAREN -> []
    | _ -> argList typedArgOpt buffer
  in
  let _ = Stream.consume buffer RPAREN in
  let t =
    match Stream.peek buffer with
    | COLON ->
        let _ = Stream.skip buffer in
        Some (type_ 0 buffer)
    | _ -> None
  in
  let tags = optional_tag buffer in
  let body = stmtList buffer in
  let next =
    match Stream.peek buffer with
    | AND ->
        let def, body, _ = stmtFunctionDecl buffer in
        Some (def, body)
    | _ -> None
  in
  { name; args; t; next; tags; loc }, body, loc


and stmtFunctionSpec (buffer : Stream.stream) : function_def =
  let _ = Stream.consume buffer FUN in
  let name, loc = id_name buffer in
  let _ = Stream.consume buffer LPAREN in
  let args =
    match Stream.peek buffer with
    | RPAREN -> []
    | _ -> argList typedArg buffer
  in
  let _ = Stream.consume buffer RPAREN in
  let t =
    match Stream.peek buffer with
    | COLON ->
        let _ = Stream.skip buffer in
        Some (type_ 0 buffer)
    | _ -> None
  in
  let tags = optional_tag buffer in
  { name; args; t; next = None; tags; loc }


and stmtFunction (buffer : Stream.stream) : top_stmt =
  let def, body, loc = stmtFunctionDecl buffer in
  { top = STopFunction (def, body); loc }


and stmtType (buffer : Stream.stream) : top_stmt =
  let _ = Stream.consume buffer TYPE in
  let name, loc = id_name buffer in
  match Stream.peek buffer with
  (*
  | COLON ->
      let _ = Stream.skip buffer in
      let type_ = type_ 10 buffer in
      let _ = Stream.optConsume buffer SEMI in
      { top = STopTypeAlias (name, type_); loc }
  *)
  | LBRACE ->
      let _ = Stream.skip buffer in
      let members = type_member_list buffer in
      let _ = Stream.consume buffer RBRACE in
      { top = STopType { name; members }; loc }
  | _ ->
      let got = tokenToString (Stream.current buffer) in
      let message = Printf.sprintf "Expecting a list of value declarations '{ val x:... }' but got %s" got in
      raise (ParserError (Stream.makeError buffer message))


and type_member_list (buffer : Stream.stream) =
  let rec loop acc =
    match Stream.peek buffer with
    | VAL ->
        let decl = type_member buffer in
        let _ = Stream.consume buffer SEMI in
        loop (decl :: acc)
    | _ -> List.rev acc
  in
  loop []


and type_member (buffer : Stream.stream) =
  let _ = Stream.expect buffer VAL in
  let name, loc = id_name buffer in
  let _ = Stream.consume buffer COLON in
  let type_ = type_ 10 buffer in
  name, type_, loc


and stmtWhile (buffer : Stream.stream) : stmt =
  let loc = Stream.location buffer in
  let _ = Stream.consume buffer WHILE in
  let _ = Stream.consume buffer LPAREN in
  let cond = expression 0 buffer in
  let _ = Stream.consume buffer RPAREN in
  let tstm = stmtList buffer in
  { s = SStmtWhile (cond, tstm); loc }


and stmt (buffer : Stream.stream) : stmt =
  try
    match Stream.peek buffer with
    | VAL -> stmtVal buffer
    | MEM -> stmtMem buffer
    | RET -> stmtReturn buffer
    | IF -> stmtIf buffer
    | WHILE -> stmtWhile buffer
    | _ -> stmtBind buffer
  with
  | ParserError error ->
      let _ = Stream.appendError buffer error in
      let _ = moveToNextStatement buffer in
      let _ = Stream.setErrors buffer true in
      { s = SStmtError; loc = Loc.default }


and stmtList (buffer : Stream.stream) : stmt =
  let start_loc = Stream.location buffer in
  let rec loop acc =
    match Stream.peek buffer with
    | RBRACE ->
        let end_loc = Stream.location buffer in
        let loc = Loc.merge start_loc end_loc in
        let _ = Stream.skip buffer in
        { s = SStmtBlock (List.rev acc); loc }
    | EOF ->
        let _ = Stream.expect buffer RBRACE in
        { s = SStmtBlock []; loc = start_loc }
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
      { s = SStmtBlock [ s ]; loc = s.loc }


and topStmt (buffer : Stream.stream) : top_stmt =
  try
    match Stream.peek buffer with
    | FUN -> stmtFunction buffer
    | TYPE -> stmtType buffer
    | EXTERNAL -> stmtExternal buffer
    | _ ->
        let message = Printf.sprintf "Expecting a function or type declaration" in
        raise (ParserError (Stream.makeError buffer message))
  with
  | ParserError error ->
      let _ = Stream.appendError buffer error in
      let _ = moveToNextTopStatement buffer in
      let _ = Stream.setErrors buffer true in
      { top = STopError; loc = Loc.default }


and topstmtList (buffer : Stream.stream) : top_stmt list =
  let rec loop acc =
    match Stream.peek buffer with
    | EOF -> []
    | _ ->
        let s = topStmt buffer in
        loop (s :: acc)
  in
  List.rev (loop [])


let parseDExp (s : string) : dexp = dexp_expression 0 (Stream.fromString s)

let parseLhsExp (s : string) : lexp = lexp_expression 0 (Stream.fromString s)

let parseExp (s : string) : exp = expression 0 (Stream.fromString s)

let parseType (s : string) : type_ = type_ 0 (Stream.fromString s)

let parseId (s : string) : string =
  let buffer = Stream.fromString s in
  fst (id_name buffer)


(** Parses an type given a string *)
let parseType (s : string) : type_ =
  let buffer = Stream.fromString s in
  type_ 0 buffer


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


let parseFunctionSpec (s : string) : function_def =
  let buffer = Stream.fromString s in
  stmtFunctionSpec buffer


let moduleName file = file |> Filename.basename |> Filename.chop_extension |> String.capitalize_ascii

(** Parses a buffer containing a list of statements and returns the results *)
let parseBuffer (file : string) (buffer : Stream.stream) =
  try
    let rec loop acc =
      match Stream.peek buffer with
      | EOF -> List.rev acc
      | _ -> loop (topStmt buffer :: acc)
    in
    let stmts = loop [] in
    if Stream.hasErrors buffer then
      raise (Error.Errors (List.rev (Stream.getErrors buffer)))
    else
      let name = moduleName file in
      { stmts; file; name }
  with
  | ParserError error -> raise (Error.Errors [ error ])
  | Error.Errors _ as e -> raise e


(** Parses a file containing a list of statements and returns the results *)
let parseFile (filename : string) =
  match FileIO.read filename with
  | Some contents ->
      let buffer = Stream.fromString ~file:filename contents in
      let result = parseBuffer filename buffer in
      result
  | None -> Error.raiseErrorMsg ("Could not open the file " ^ filename)


(** Parses a string containing a list of statements and returns the results *)
let parseString (file : string option) (text : string) =
  let buffer =
    match file with
    | Some f -> Stream.fromString ~file:f text
    | None -> Stream.fromString text
  in
  let result = parseBuffer "live.vult" buffer in
  result

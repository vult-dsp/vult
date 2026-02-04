(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz

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
open Tokens
open Stream
open Syntax
open Util

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

(** Contextual wrapper functions for better error messages *)

let consumeInContext (buffer : Stream.stream) (kind : token_enum) (context : string) : unit =
  try Stream.consume buffer kind with
  | ParserError (Error.PointedError (loc, msg)) ->
    let enhanced_msg = Printf.sprintf "%s in %s" msg context in
    raise (ParserError (Error.PointedError (loc, enhanced_msg)))


let expectInContext (buffer : Stream.stream) (kind : token_enum) (context : string) : unit =
  try Stream.expect buffer kind with
  | ParserError (Error.PointedError (loc, msg)) ->
    let enhanced_msg = Printf.sprintf "%s in %s" msg context in
    raise (ParserError (Error.PointedError (loc, enhanced_msg)))


let notExpectedErrorInContext (token : token_enum token) (context : string) (expected : string) : Error.t =
  let token_str = tokenToString token in
  let message = Printf.sprintf "Found %s, but expected %s in %s" token_str expected context in
  Error.PointedError (Loc.getNext token.loc, message)


let rec moveToNextTopStatement (buffer : Stream.stream) : unit =
  match Stream.peek buffer with
  | EOF -> ()
  | FUN | TYPE | EXTERNAL | ENUM -> ()
  | _ ->
    let _ = Stream.skip buffer in
    moveToNextTopStatement buffer


let rec moveToNextStatement (buffer : Stream.stream) : unit =
  match Stream.peek buffer with
  | SEMI -> Stream.skip buffer
  | EOF -> ()
  | VAL | IF | RET -> ()
  | RBRACE -> Stream.skip buffer
  | _ ->
    let _ = Stream.skip buffer in
    moveToNextStatement buffer


let expToPath error (exp : exp) : path =
  match exp with
  | { e = SEId id; loc } -> { id; n = None; loc }
  | { e = SEMember ({ e = SEId e; _ }, id); loc } -> { id; n = Some e; loc }
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
  | GT, _ -> 40
  | OP, "<" -> 40
  | LT, _ -> 40
  | OP, ">=" -> 40
  | OP, "<=" -> 40
  | OP, ">>" -> 50
  | OP, "<<" -> 50
  | OP, "+" -> 55
  | OP, "-" -> 55
  | OP, "&" -> 60
  | OP, "^" -> 45
  | OP, "|" -> 40
  | OP, "*" -> 60
  | OP, "/" -> 60
  | OP, "%" -> 60
  | COLON, _ -> 70
  | LPAREN, _ -> 80
  | LBRACK, _ -> 80
  | LBRACE, _ -> 80
  | DOT, _ -> 90
  | _ -> 0


let getTypeLbp (token : 'kind token) : int =
  match token.kind, token.value with
  | COMMA, _ -> 20
  | GT, _ -> 21
  | LT, _ -> 21
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
  let _ = expectInContext buffer STRING "string literal" in
  let token = Stream.current buffer in
  let _ = Stream.skip buffer in
  token.value


let prattParser (rbp : int) (buffer : Stream.stream) (lbp : 'kind token -> int)
    (nud : Stream.stream -> 'kind token -> 'exp) (led : Stream.stream -> 'kind token -> 'exp -> 'exp) =
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
    let e = parser 21 buffer in
    match Stream.peek buffer with
    | COMMA ->
      let _ = Stream.skip buffer in
      loop (e :: acc)
    | _ -> CCList.rev (e :: acc)
  in
  loop []


let id_name (buffer : Stream.stream) : string * Loc.t =
  let () = expectInContext buffer ID "identifier" in
  let token = Stream.current buffer in
  let () = Stream.skip buffer in
  token.value, token.loc


let int_value (buffer : Stream.stream) : int * Loc.t =
  let () = expectInContext buffer INT "integer value" in
  let token = Stream.current buffer in
  let () = Stream.skip buffer in
  int_of_string token.value, token.loc


(** Parses tag expressions *)
let rec tag (rbp : int) (buffer : Stream.stream) : Ptags.tag = prattParser rbp buffer getExpLbp tag_nud tag_led

and tagExpressionList (buffer : Stream.stream) : Ptags.tag list = commaSepList tag buffer

and tag_nud (buffer : Stream.stream) (token : 'kind token) : Ptags.tag =
  let loc = token.loc in
  match token.kind, token.value with
  | ID, _ -> (
    let name = token.value in
    let is_type_intrinsic =
      String.equal name "typemax" || String.equal name "typemin" || String.equal name "typedefault"
    in
    match Stream.peek buffer with
    | LPAREN -> (
      let _ = Stream.skip buffer in
      match Stream.peek buffer with
      | RPAREN ->
        let _ = Stream.skip buffer in
        { g = TagId name; loc }
      | QUOTED_ID when is_type_intrinsic ->
        (* Parse type intrinsic: typemax('t) *)
        let type_param_token = Stream.current buffer in
        let _ = Stream.skip buffer in
        let _ = consumeInContext buffer RPAREN "type intrinsic call" in
        { g = TagTypeIntrinsic (name, type_param_token.value); loc }
      | _ ->
        let args = tag_pair_list buffer in
        let _ = consumeInContext buffer RPAREN "tag function call" in
        { g = TagCall { name; args }; loc })
    | _ -> { g = TagId name; loc })
  | OP, "-" -> tag_unary_op buffer token
  | INT, _ -> { g = TagInt (int_of_string token.value); loc }
  | XINT, _ -> { g = TagInt (int_of_string token.value); loc }
  | TRUE, _ -> { g = TagBool true; loc }
  | FALSE, _ -> { g = TagBool false; loc }
  | REAL, _ -> { g = TagReal (float_of_string token.value); loc }
  | STRING, _ -> { g = TagString token.value; loc }
  | _ ->
    let message =
      notExpectedErrorInContext token "tag expression" "identifier, number, boolean, string, or function call"
    in
    raise (ParserError message)


and tag_unary_op (buffer : Stream.stream) (token : 'kind token) : Ptags.tag =
  let right = tag 70 buffer in
  match right.g with
  | TagInt value -> { right with g = TagInt (-value) }
  | TagReal value -> { right with g = TagReal (-.value) }
  | _ -> Error.raiseError "Invalid unary operation. Can only apply '-' to numbers" token.loc


and tag_led (_ : Stream.stream) (token : 'kind token) (_ : Ptags.tag) : Ptags.tag =
  match token.kind with
  | _ ->
    let message = notExpectedErrorInContext token "tag expression" "operator or end of expression" in
    raise (ParserError message)


and tag_pair (bp : int) (buffer : Stream.stream) : string * Ptags.tag * Loc.t =
  let id, loc = id_name buffer in
  let _ = consumeInContext buffer EQUAL "tag assignment" in
  let value = tag bp buffer in
  id, value, loc


and tag_pair_list (buffer : Stream.stream) : (string * Ptags.tag * Loc.t) list = commaSepList tag_pair buffer

let optional_tag (buffer : Stream.stream) : Ptags.tag list =
  match Stream.peek buffer with
  | TAG ->
    let _ = consumeInContext buffer TAG "attribute declaration" in
    let attr = tagExpressionList buffer in
    let _ = consumeInContext buffer RBRACK "attribute list" in
    attr
  | _ -> []


let rec type_ (rbp : int) (buffer : Stream.stream) : type_ = prattParser rbp buffer getTypeLbp type_nud type_led

(** Nud function for the Pratt parser *)
and type_nud (_ : Stream.stream) (token : 'kind token) : type_ =
  match token.kind, token.value with
  | WILD, _ ->
    let loc = token.loc in
    { t = STUnbound; loc }
  | ID, _ ->
    let id = token.value in
    let loc = token.loc in
    { t = STId { id; n = None; loc }; loc }
  | QUOTED_ID, _ ->
    let id = token.value in
    let loc = token.loc in
    { t = STGenericType id; loc }
  | INT, _ ->
    let loc = token.loc in
    { t = STSize (int_of_string token.value); loc }
  | XINT, _ ->
    let loc = token.loc in
    { t = STSize (int_of_string token.value); loc }
  | _ ->
    let message =
      Error.PointedError
        (token.loc, "Unexpected token in type definition. Expected a type name, wildcard '_', number, or generic type")
    in
    raise (ParserError message)


and type_led (buffer : Stream.stream) (token : 'kind token) (left : type_) : type_ =
  match token.kind with
  | DOT -> type_member buffer token left
  | LPAREN -> type_call RPAREN buffer token left
  | LT -> type_call GT buffer token left
  | _ ->
    let message =
      Error.PointedError (token.loc, "Unexpected token in type expression. Expected a member access or function call")
    in
    raise (ParserError message)


and type_member (buffer : Stream.stream) (token : 'kind token) (left : type_) : type_ =
  let right = type_ (getExpLbp token) buffer in
  match right.t, left.t with
  | STId rpath, STId { id; n = None; _ } -> { right with t = STId { rpath with n = Some id } }
  | _ ->
    let message =
      Error.PointedError (token.loc, "Unexpected token in member access. Expected an identifier after the dot")
    in
    raise (ParserError message)


and type_call clossing (buffer : Stream.stream) (token : 'kind token) (left : type_) : type_ =
  let path =
    match left.t with
    | STId { id; _ } -> id
    | _ ->
      let message = Error.PointedError (token.loc, "Expected a type name for generic type parameters") in
      raise (ParserError message)
  in
  let args =
    if Stream.peek buffer = clossing then
      []
    else
      type_list buffer
  in
  let _ = consumeInContext buffer clossing "type parameter list" in
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
  | LPAREN -> (
    match Stream.peek buffer with
    | RPAREN ->
      let message =
        Error.PointedError
          ( token.loc
          , "Empty parentheses are not allowed in variable declarations. Use 'val x = expression;' or remove the \
             parentheses" )
      in
      raise (ParserError message)
    | _ ->
      let e = dexp_expression 0 buffer in
      let _ = consumeInContext buffer RPAREN "grouped declaration" in
      let loc = token.loc in
      { d = SDGroup e; loc })
  | _ ->
    let message =
      Error.PointedError
        (token.loc, "Invalid variable declaration. Expected a variable name, wildcard '_', or grouped declaration")
    in
    raise (ParserError message)


and dexp_led (buffer : Stream.stream) (token : 'kind token) (left : dexp) : dexp =
  match token.kind with
  | COMMA -> dpair buffer token left
  | LBRACK -> darray buffer token left
  | COLON -> dtyped buffer token left
  | _ ->
    let message =
      Error.PointedError
        (token.loc, "Invalid syntax in variable declaration. Expected comma, array index, or type annotation")
    in
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
  let () = consumeInContext buffer RBRACK "array size declaration" in
  match left with
  | { d = SDId (id, None); _ } -> { d = SDId (id, Some size); loc = token.loc }
  | _ ->
    let message = Error.PointedError (token.loc, "Array declaration syntax error. Expected 'variable_name[size]'") in
    raise (ParserError message)


let rec lexp_expression (rbp : int) (buffer : Stream.stream) : lexp =
  prattParser rbp buffer getLExpLbp lexp_nud lexp_led


and lexp_nud (buffer : Stream.stream) (token : 'kind token) : lexp =
  match token.kind with
  | WILD -> { l = SLWild; loc = token.loc }
  | ID ->
    let id = token.value in
    { l = SLId id; loc = token.loc }
  | LPAREN -> (
    match Stream.peek buffer with
    | RPAREN ->
      let message =
        Error.PointedError
          (token.loc, "Empty parentheses are not allowed on left side of assignment. Use variable names or patterns")
      in
      raise (ParserError message)
    | _ ->
      let e = lexp_expression 0 buffer in
      let _ = consumeInContext buffer RPAREN "grouped left-hand side" in
      { l = SLGroup e; loc = token.loc })
  | _ ->
    let message =
      Error.PointedError
        (token.loc, "Invalid assignment target. Expected a variable name, wildcard '_', or grouped expression")
    in
    raise (ParserError message)


and lexp_led (buffer : Stream.stream) (token : 'kind token) (left : lexp) : lexp =
  match token.kind with
  | COMMA -> lhs_pair buffer token left
  | DOT -> lexp_member buffer token left
  | LBRACK -> lexp_index buffer token left
  | _ ->
    let message =
      Error.PointedError (token.loc, "Invalid assignment syntax. Expected comma, member access, or array index")
    in
    raise (ParserError message)


and lexp_member (buffer : Stream.stream) (token : 'kind token) (left : lexp) : lexp =
  let right = lexp_expression (getLExpLbp token) buffer in
  match right.l with
  | SLMember (({ l = SLId id; _ } as i), n) -> { right with l = SLMember ({ i with l = SLMember (left, id) }, n) }
  | SLId id -> { right with l = SLMember (left, id) }
  | _ ->
    let message =
      Error.PointedError
        (token.loc, "Invalid member access on left side of assignment. Expected a field name after the dot")
    in
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
  let _ = consumeInContext buffer RBRACK "array index in assignment" in
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
    let e = expression 0 buffer in
    let _ = consumeInContext buffer RPAREN "grouped expression" in
    { e = SEGroup e; loc }
  | INT, _ -> { e = SEInt token.value; loc }
  | XINT, _ -> { e = SEInt token.value; loc }
  | REAL, _ -> { e = SEReal token.value; loc }
  | FIXED, _ -> { e = SEFixed token.value; loc }
  | STRING, _ -> { e = SEString token.value; loc }
  | TRUE, _ -> { e = SEBool true; loc }
  | FALSE, _ -> { e = SEBool false; loc }
  | IF, _ ->
    let cond = expression 0 buffer in
    let _ = consumeInContext buffer THEN "if-then-else expression" in
    let then_ = expression 0 buffer in
    let _ = consumeInContext buffer ELSE "if-then-else expression" in
    let else_ = expression 0 buffer in
    { e = SEIf { cond; then_; else_ }; loc }
  | LBRACK, _ -> (
    match Stream.peek buffer with
    | RBRACK ->
      let _ = consumeInContext buffer RBRACK "empty array" in
      { e = SEArray []; loc }
    | _ ->
      let elems = expressionList buffer in
      let _ = consumeInContext buffer RBRACK "array expression" in
      { e = SEArray elems; loc })
  | _ ->
    let message =
      Error.PointedError (token.loc, "Unexpected token in expression. Expected a value, variable, or operator")
    in
    raise (ParserError message)


and exp_led (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  match token.kind with
  | OP -> binary_op buffer token left
  | LT -> binary_op buffer token left
  | GT -> binary_op buffer token left
  | COMMA -> pair buffer token left
  | DOT -> exp_member buffer token left
  | LPAREN -> call buffer token left
  | LBRACE -> record buffer token left
  | COLON -> named_call buffer token left
  | LBRACK -> exp_index buffer token left
  | _ ->
    let message =
      Error.PointedError
        (token.loc, "Invalid expression syntax. Expected an operator, function call, or end of expression")
    in
    raise (ParserError message)


and pattern (rbp : int) (buffer : Stream.stream) : pattern = prattParser rbp buffer getExpLbp pattern_nud pattern_led

(** Nud function for the Pratt parser *)
and pattern_nud (buffer : Stream.stream) (token : 'kind token) : pattern =
  let loc = token.loc in
  match token.kind, token.value with
  | WILD, _ -> { p = SPWild; loc }
  | ID, _ -> { p = SPId token.value; loc }
  | LPAREN, _ ->
    let p = pattern 0 buffer in
    let _ = consumeInContext buffer RPAREN "grouped pattern" in
    { p = SPGroup p; loc }
  | INT, _ -> { p = SPInt token.value; loc }
  | XINT, _ -> { p = SPInt token.value; loc }
  | REAL, _ -> { p = SPReal token.value; loc }
  | FIXED, _ -> { p = SPFixed token.value; loc }
  | STRING, _ -> { p = SPString token.value; loc }
  | TRUE, _ -> { p = SPBool true; loc }
  | FALSE, _ -> { p = SPBool false; loc }
  | _ ->
    let message =
      Error.PointedError
        (token.loc, "Invalid pattern in match expression. Expected a value, wildcard '_', or constructor")
    in
    raise (ParserError message)


and pattern_led (buffer : Stream.stream) (token : 'kind token) (left : pattern) : pattern =
  match token.kind with
  | COMMA -> pair_pattern buffer token left
  | DOT -> pattern_member buffer token left
  | _ ->
    let message = Error.PointedError (token.loc, "Invalid pattern syntax. Expected comma or member access") in
    raise (ParserError message)


and pair_pattern (buffer : Stream.stream) (token : 'kind token) (left : pattern) : pattern =
  let right = pattern (getExpLbp token) buffer in
  let getElems e =
    match e.p with
    | SPTuple elems -> elems
    | _ -> [ e ]
  in
  let elems1 = left |> getElems in
  let elems2 = right |> getElems in
  { p = SPTuple (elems1 @ elems2); loc = left.loc }


and pattern_member (buffer : Stream.stream) (token : 'kind token) (left : pattern) : pattern =
  let right = pattern (getExpLbp token) buffer in
  match right.p with
  | SPMember (({ p = SPId id; _ } as i), n) -> { right with p = SPMember ({ i with p = SPMember (left, id) }, n) }
  | SPId id -> { right with p = SPMember (left, id) }
  | _ ->
    let message =
      Error.PointedError (token.loc, "Invalid pattern member access. Expected an identifier after the dot")
    in
    raise (ParserError message)


and exp_member (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let right = expression (getExpLbp token) buffer in
  match right.e with
  | SEMember (({ e = SEId id; _ } as i), n) -> { right with e = SEMember ({ i with e = SEMember (left, id) }, n) }
  | SEId id -> { right with e = SEMember (left, id) }
  | _ ->
    let message = Error.PointedError (token.loc, "Invalid member access. Expected a field name after the dot") in
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
    { right with e = SECall { call with instance = Some (name, None) } }
  | { e = SEIndex { e = { e = SEId name; _ }; index }; _ }, { e = SECall ({ instance = None; _ } as call); _ } ->
    { right with e = SECall { call with instance = Some (name, Some index) } }
  | ({ e = SEId _; _ } | { e = SEIndex { e = { e = SEId _; _ }; _ }; _ }), _ ->
    let loc = left.loc in
    let error =
      Error.PointedError
        (Loc.getNext loc, "Invalid instance call syntax. Expected a function call after ':' (e.g., name:foo())")
    in
    raise (ParserError error)
  | _, { e = SECall { instance = None; _ }; _ } ->
    let loc = left.loc in
    let error =
      Error.PointedError
        ( Loc.getNext loc
        , "Invalid instance name. Use a simple identifier like 'name:foo()' or indexed like 'name[1]:foo()'" )
    in
    raise (ParserError error)
  | _, { e = SECall { instance = Some _; _ }; _ } ->
    let loc = left.loc in
    let error =
      Error.PointedError (Loc.getNext loc, "Cannot apply instance name to a function that already has an instance")
    in
    raise (ParserError error)
  | _ ->
    let loc = Loc.merge left.loc right.loc in
    let error =
      Error.PointedError
        ( Loc.getNext loc
        , "Invalid expression. Missing operator between expressions? (e.g., 'a + (b)' instead of 'a (b)')" )
    in
    raise (ParserError error)


and call (buffer : Stream.stream) (_token : 'kind token) (left : exp) : exp =
  let error () =
    let message =
      Error.PointedError (left.loc, "Invalid function call. Expected a function name or module.function syntax")
    in
    raise (ParserError message)
  in
  let path = expToPath error left in
  (* Check for type intrinsics: typemax('t), typemin('t), typedefault('t) *)
  let is_type_intrinsic =
    match path with
    | { id; n = None; _ } -> String.equal id "typemax" || String.equal id "typemin" || String.equal id "typedefault"
    | _ -> false
  in
  if is_type_intrinsic then
    (* Parse a single quoted identifier argument for type intrinsics *)
    match Stream.peek buffer with
    | QUOTED_ID ->
      let token = Stream.current buffer in
      let () = Stream.skip buffer in
      let type_param = token.value in
      let () = consumeInContext buffer RPAREN "type intrinsic call" in
      { e = SETypeIntrinsic (path.id, type_param); loc = path.loc }
    | RPAREN ->
      let message =
        Error.PointedError
          (path.loc, "Type intrinsic '" ^ path.id ^ "' requires a type parameter (e.g., " ^ path.id ^ "('t))")
      in
      raise (ParserError message)
    | _ ->
      let message =
        Error.PointedError
          (path.loc, "Type intrinsic '" ^ path.id ^ "' expects a generic type parameter (e.g., " ^ path.id ^ "('t))")
      in
      raise (ParserError message)
  else
    let args =
      match Stream.peek buffer with
      | RPAREN -> []
      | _ -> expressionList buffer
    in
    let _ = consumeInContext buffer RPAREN "function call" in
    { e = SECall { instance = None; path; args }; loc = path.loc }


and exp_index (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let index = expression 0 buffer in
  let _ = consumeInContext buffer RBRACK "array index" in
  { e = SEIndex { e = left; index }; loc = token.loc }


and unaryOp (buffer : Stream.stream) (token : 'kind token) : exp =
  let right = expression 70 buffer in
  { e = SEUnOp (token.value, right); loc = token.loc }


and binary_op (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let right = expression (getExpLbp token) buffer in
  let loc = Loc.merge left.loc right.loc in
  { e = SEOp (token.value, left, right); loc }


and recordValue level (buffer : Stream.stream) =
  let id, loc = id_name buffer in
  let _ = consumeInContext buffer EQUAL "record field assignment" in
  let e = expression level buffer in
  { id; n = None; loc }, e


and recordValues (buffer : Stream.stream) = commaSepList recordValue buffer

and record (buffer : Stream.stream) (token : 'kind token) (left : exp) : exp =
  let error () =
    let message = Error.PointedError (left.loc, "Invalid record constructor. Expected a type name before the braces") in
    raise (ParserError message)
  in
  let path = expToPath error left in
  let elems = recordValues buffer in
  let _ = consumeInContext buffer RBRACE "record constructor" in
  { e = SERecord { path; elems }; loc = token.loc }


and expressionList (buffer : Stream.stream) : exp list = commaSepList expression buffer

and stmtVal (buffer : Stream.stream) : stmt =
  let loc = Stream.location buffer in
  let _ = consumeInContext buffer VAL "value declaration" in
  let lhs = dexp_expression 0 buffer in
  match Stream.peek buffer with
  | EQUAL ->
    let _ = Stream.skip buffer in
    let rhs = expression 0 buffer in
    let _ = consumeInContext buffer SEMI "value declaration" in
    { s = SStmtVal (lhs, Some rhs); loc }
  | _ ->
    let _ = consumeInContext buffer SEMI "value declaration" in
    { s = SStmtVal (lhs, None); loc }


and stmtMem (buffer : Stream.stream) : stmt =
  let loc = Stream.location buffer in
  let _ = consumeInContext buffer MEM "memory declaration" in
  let lhs = dexp_expression 0 buffer in
  match Stream.peek buffer with
  | EQUAL ->
    let _ = Stream.skip buffer in
    let rhs = expression 0 buffer in
    let tags = optional_tag buffer in
    let _ = consumeInContext buffer SEMI "memory declaration" in
    { s = SStmtMem (lhs, Some rhs, tags); loc }
  | _ ->
    let tags = optional_tag buffer in
    let _ = consumeInContext buffer SEMI "memory declaration" in
    { s = SStmtMem (lhs, None, tags); loc }


and stmtReturn (buffer : Stream.stream) : stmt =
  let loc = Stream.location buffer in
  let _ = consumeInContext buffer RET "return statement" in
  let e = expression 0 buffer in
  let _ = consumeInContext buffer SEMI "return statement" in
  { s = SStmtReturn e; loc }


and stmtBind (buffer : Stream.stream) : stmt =
  match lexp_expression 0 buffer with
  | e1 -> (
    let loc = e1.loc in
    match Stream.peek buffer with
    | EQUAL ->
      let _ = consumeInContext buffer EQUAL "assignment" in
      let e2 = expression 0 buffer in
      let _ = consumeInContext buffer SEMI "assignment" in
      { s = SStmtBind (e1, e2); loc }
    | _ ->
      let message =
        Printf.sprintf "Invalid statement. All statements should be in the forms: \"a = b; \" or \"_ = b(); \" "
      in
      raise (ParserError (Stream.makeError buffer message)))


and stmtIf (buffer : Stream.stream) : stmt =
  let _ = consumeInContext buffer IF "if statement" in
  let _ = consumeInContext buffer LPAREN "if condition" in
  let cond = expression 0 buffer in
  let _ = consumeInContext buffer RPAREN "if condition" in
  let tstm = stmtList buffer in
  let loc = cond.loc in
  match Stream.peek buffer with
  | ELSE ->
    let _ = consumeInContext buffer ELSE "if-else statement" in
    let fstm = stmtList buffer in
    { s = SStmtIf (cond, tstm, Some fstm); loc }
  | _ -> { s = SStmtIf (cond, tstm, None); loc }


and stmtMatch (buffer : Stream.stream) : stmt =
  let _ = consumeInContext buffer MATCH "match statement" in
  let _ = consumeInContext buffer LPAREN "match expression" in
  let e = expression 0 buffer in
  let _ = consumeInContext buffer RPAREN "match expression" in
  let _ = consumeInContext buffer LBRACE "match cases" in
  let loc = e.loc in
  let rec loop cases =
    let m = pattern 0 buffer in
    let _ = consumeInContext buffer ARROW "match case" in
    let case = stmtList buffer in
    match Stream.peek buffer with
    | RBRACE -> CCList.rev ((m, case) :: cases)
    | _ -> loop ((m, case) :: cases)
  in
  let cases = loop [] in
  let _ = consumeInContext buffer RBRACE "match cases" in
  { s = SStmtMatch { e; cases }; loc }


and typedArgOpt (buffer : Stream.stream) =
  let _ = expectInContext buffer ID "function parameter" in
  let token = Stream.current buffer in
  let _ = Stream.skip buffer in
  match Stream.peek buffer with
  | COLON ->
    let _ = Stream.skip buffer in
    let e = type_ 20 buffer in
    token.value, Some e, token.loc
  | _ -> token.value, None, token.loc


and typedArg (buffer : Stream.stream) =
  let _ = expectInContext buffer ID "typed function parameter" in
  let token = Stream.current buffer in
  let _ = Stream.skip buffer in
  let _ = consumeInContext buffer COLON "parameter type" in
  let e = type_ 20 buffer in
  token.value, Some e, token.loc


and parseGenericParam (buffer : Stream.stream) : generic_param =
  let _ = expectInContext buffer QUOTED_ID "generic parameter" in
  let token = Stream.current buffer in
  let _ = Stream.skip buffer in
  let param_name = token.value in
  (* Validate parameter name *)
  (if String.length param_name = 0 then
     let msg = "Generic parameter name cannot be empty" in
     raise (ParserError (Error.PointedError (token.loc, msg))));
  (* Check for invalid characters in parameter name *)
  (if not (Str.string_match (Str.regexp "^[a-zA-Z_][a-zA-Z0-9_]*$") param_name 0) then
     let msg =
       Printf.sprintf
         "Generic parameter name '%s' contains invalid characters. Use only letters, numbers, and underscore."
         param_name
     in
     raise (ParserError (Error.PointedError (token.loc, msg))));
  (* Check for reserved names *)
  let reserved_names =
    [ "int"; "real"; "bool"; "string"; "unit"; "if"; "else"; "while"; "for"; "return"; "fun"; "type"; "enum" ]
  in
  (if List.mem param_name reserved_names then
     let msg = Printf.sprintf "Generic parameter name '%s' is a reserved keyword" param_name in
     raise (ParserError (Error.PointedError (token.loc, msg))));
  match Stream.peek buffer with
  | COLON -> (
    let _ = Stream.skip buffer in
    let type_expr = type_ 20 buffer in
    (* Check if this looks like a function type - for now, simple heuristic *)
    match type_expr.t with
    | STComposed ("fun", _) -> GParamFunction (param_name, Some type_expr)
    | _ -> GParamConstant (param_name, type_expr))
  | _ ->
    (* No type annotation - use naming convention to determine type *)
    (* If parameter name starts with 'f' or ends with 'fn'/'func', assume it's a function *)
    if String.length param_name > 0 && param_name.[0] = 'f' then
      GParamFunction (param_name, None)
    else
      GParamType param_name


and argList arg_parser (buffer : Stream.stream) =
  match Stream.peek buffer with
  | ID -> (
    let first = arg_parser buffer in
    match Stream.peek buffer with
    | COMMA ->
      let _ = consumeInContext buffer COMMA "parameter list" in
      first :: argList arg_parser buffer
    | _ -> [ first ])
  | _ -> []


and parseGenericsAndArguments (buffer : Stream.stream) : generic_param list * arg list =
  let rec loop generic_params param_names =
    match Stream.peek buffer with
    | QUOTED_ID -> (
      let generic_param = parseGenericParam buffer in
      let param_name =
        match generic_param with
        | GParamFunction (name, _) -> name
        | GParamType name -> name
        | GParamConstant (name, _) -> name
      in
      (* Check for duplicate generic parameter names *)
      (if List.mem param_name param_names then
         let token = Stream.current buffer in
         let msg = Printf.sprintf "Duplicate generic parameter name '%s'" param_name in
         raise (ParserError (Error.PointedError (token.loc, msg))));
      match Stream.peek buffer with
      | COMMA ->
        let _ = consumeInContext buffer COMMA "parameter list" in
        loop (generic_param :: generic_params) (param_name :: param_names)
      | _ -> List.rev (generic_param :: generic_params), [])
    | ID ->
      (* Start parsing regular arguments *)
      let args = argList typedArgOpt buffer in
      let generic_params_final = List.rev generic_params in
      (* Check for conflicts between generic parameter names and regular argument names *)
      List.iter
        (fun generic_param ->
          let generic_name =
            match generic_param with
            | GParamFunction (name, _) -> name
            | GParamType name -> name
            | GParamConstant (name, _) -> name
          in
          List.iter
            (fun arg ->
              let arg_name, _, arg_loc = arg in
              if arg_name = generic_name then
                let msg = Printf.sprintf "Generic parameter '%s' conflicts with function argument name" generic_name in
                raise (ParserError (Error.PointedError (arg_loc, msg))))
            args)
        generic_params_final;
      generic_params_final, args
    | _ ->
      (* No arguments at all *)
      List.rev generic_params, []
  in
  loop [] []


and stmtExternal (buffer : Stream.stream) : top_stmt =
  let _ = Stream.skip buffer in
  let name, loc = id_name buffer in
  let _ = consumeInContext buffer LPAREN "external function parameter list" in
  let args =
    match Stream.peek buffer with
    | RPAREN -> []
    | _ -> argList typedArg buffer
  in
  let _ = consumeInContext buffer RPAREN "external function parameter list" in
  let _ = consumeInContext buffer COLON "external function return type" in
  let type_ = type_ 0 buffer in
  let link_name, tags =
    match Stream.peek buffer with
    | STRING ->
      let link_name = string buffer in
      let tags = optional_tag buffer in
      Some link_name, tags
    | TAG ->
      let tags = optional_tag buffer in
      None, tags
    | _ ->
      let message = Printf.sprintf "Expecting a string with a link name or a tag" in
      raise (ParserError (Stream.makeError buffer message))
  in
  let _ = consumeInContext buffer SEMI "external function declaration" in
  { top = STopExternal ({ name; args; t = Some type_; tags; loc }, link_name); loc }


(* Extract quoted identifiers from a type for implicit generic type parameters *)
and extract_quoted_identifiers_from_type (t : type_) : string list =
  let rec extract_from_type_d = function
    | STGenericType id -> [ id ] (* Explicit generic type parameter *)
    | STComposed (_, types) -> CCList.flat_map extract_from_type types
    | STUnbound | STSize _ | STId _ -> []
  and extract_from_type (t : type_) : string list = extract_from_type_d t.t in
  extract_from_type t


(* Extract quoted identifiers from function arguments *)
and extract_quoted_identifiers_from_args (args : arg list) : string list =
  CCList.flat_map
    (fun (_, type_opt, _) ->
      match type_opt with
      | Some t -> extract_quoted_identifiers_from_type t
      | None -> [])
    args


(* Create implicit generic type parameters from quoted identifiers *)
and create_implicit_generic_params (quoted_ids : string list) : generic_param list =
  (* Remove duplicates and create GParamType entries *)
  quoted_ids |> CCList.sort_uniq ~cmp:String.compare |> CCList.map (fun param_name -> GParamType param_name)


and stmtFunctionDecl (buffer : Stream.stream) : function_def * Loc.t =
  let _ = Stream.skip buffer in
  let name, loc = id_name buffer in
  let _ = consumeInContext buffer LPAREN "function parameter list" in
  let explicit_generic_params, args =
    match Stream.peek buffer with
    | RPAREN -> [], []
    | _ -> parseGenericsAndArguments buffer
  in
  let _ = consumeInContext buffer RPAREN "function parameter list" in
  let t =
    match Stream.peek buffer with
    | COLON ->
      let _ = Stream.skip buffer in
      Some (type_ 0 buffer)
    | _ -> None
  in
  (* Extract implicit generic type parameters from argument types and return type *)
  let quoted_ids_from_args = extract_quoted_identifiers_from_args args in
  let quoted_ids_from_return =
    match t with
    | Some return_type -> extract_quoted_identifiers_from_type return_type
    | None -> []
  in
  let all_quoted_ids = quoted_ids_from_args @ quoted_ids_from_return in
  let implicit_generic_params = create_implicit_generic_params all_quoted_ids in
  (* Combine explicit and implicit generic parameters *)
  let all_generic_params = explicit_generic_params @ implicit_generic_params in
  let tags = optional_tag buffer in
  let body = stmtList buffer in
  let next =
    match Stream.peek buffer with
    | AND ->
      let def, _ = stmtFunctionDecl buffer in
      Some def
    | _ -> None
  in
  { name; generic_params = all_generic_params; args; t; next; tags; loc; body }, loc


and stmtFunction (buffer : Stream.stream) : top_stmt =
  let def, loc = stmtFunctionDecl buffer in
  { top = STopFunction def; loc }


and stmtType (buffer : Stream.stream) : top_stmt =
  let _ = consumeInContext buffer TYPE "type declaration" in
  let name, loc = id_name buffer in
  match Stream.peek buffer with
  | LBRACE ->
    let _ = Stream.skip buffer in
    let members = type_member_list buffer in
    let _ = consumeInContext buffer RBRACE "type declaration" in
    { top = STopType { name; members }; loc }
  | SEMI ->
    let _ = Stream.skip buffer in
    { top = STopType { name; members = [] }; loc }
  | _ ->
    let got = tokenToString (Stream.current buffer) in
    let message = Printf.sprintf "Expecting a list of value declarations '{ val x:... }' but got %s" got in
    raise (ParserError (Stream.makeError buffer message))


and type_member_list (buffer : Stream.stream) =
  match Stream.peek buffer with
  | RBRACE -> raise (ParserError (Stream.makeError buffer "This type declaration is empty"))
  | VAL ->
    let rec loop acc =
      match Stream.peek buffer with
      | VAL ->
        let decl = type_elem buffer in
        let _ = consumeInContext buffer SEMI "type member" in
        loop (decl :: acc)
      | _ -> CCList.rev acc
    in
    loop []
  | _ ->
    let got = tokenToString (Stream.current buffer) in
    let message = Printf.sprintf "Expecting a list of value declarations '{ val x:... }' but got %s" got in
    raise (ParserError (Stream.makeError buffer message))


and type_elem (buffer : Stream.stream) =
  let _ = consumeInContext buffer VAL "type member declaration" in
  let name, loc = id_name buffer in
  let _ = consumeInContext buffer COLON "type member type" in
  let type_ = type_ 10 buffer in
  let tags = optional_tag buffer in
  name, type_, tags, loc


and stmtEnum (buffer : Stream.stream) : top_stmt =
  let _ = consumeInContext buffer ENUM "enum declaration" in
  let name, loc = id_name buffer in
  match Stream.peek buffer with
  | LBRACE ->
    let _ = Stream.skip buffer in
    let members = enum_member_type buffer in
    let _ = consumeInContext buffer RBRACE "enum declaration" in
    { top = STopEnum { name; members }; loc }
  | _ ->
    let got = tokenToString (Stream.current buffer) in
    let message = Printf.sprintf "Expecting a list of value declarations '{ val x:... }' but got %s" got in
    raise (ParserError (Stream.makeError buffer message))


and enum_member_type (buffer : Stream.stream) =
  match Stream.peek buffer with
  | RBRACE -> raise (ParserError (Stream.makeError buffer "The enumeration declaration is empty"))
  | ID ->
    let rec loop acc =
      match Stream.peek buffer with
      | ID -> (
        let decl = enum_name buffer in
        match Stream.peek buffer with
        | COMMA ->
          let _ = consumeInContext buffer COMMA "enum member list" in
          loop (decl :: acc)
        | RBRACE -> CCList.rev (decl :: acc)
        | _ -> raise (ParserError (Stream.makeError buffer "Expecting more enumeration elements")))
      | _ -> CCList.rev acc
    in
    loop []
  | _ ->
    let got = tokenToString (Stream.current buffer) in
    let message = Printf.sprintf "Expecting a list of enumeration elements but got %s" got in
    raise (ParserError (Stream.makeError buffer message))


and enum_name (buffer : Stream.stream) =
  let name, loc = id_name buffer in
  if String.capitalize_ascii name = name then
    name, loc
  else
    raise (ParserError (Stream.makeError buffer "Enumeration elements should start with uppercase"))


and stmtWhile (buffer : Stream.stream) : stmt =
  let loc = Stream.location buffer in
  let _ = consumeInContext buffer WHILE "while loop" in
  let _ = consumeInContext buffer LPAREN "while condition" in
  let cond = expression 0 buffer in
  let _ = consumeInContext buffer RPAREN "while condition" in
  let tstm = stmtList buffer in
  { s = SStmtWhile (cond, tstm); loc }


and stmtIter (buffer : Stream.stream) : stmt =
  let loc = Stream.location buffer in
  let _ = consumeInContext buffer ITER "iter loop" in
  let _ = consumeInContext buffer LPAREN "iter parameters" in
  let name, id_loc = id_name buffer in
  let _ = consumeInContext buffer COMMA "iter parameters" in
  let value = expression 0 buffer in
  let _ = consumeInContext buffer RPAREN "iter parameters" in
  let body = stmtList buffer in
  { s = SStmtIter { id = name, id_loc; value; body }; loc }


and stmt (buffer : Stream.stream) : stmt =
  try
    match Stream.peek buffer with
    | VAL -> stmtVal buffer
    | MEM -> stmtMem buffer
    | RET -> stmtReturn buffer
    | IF -> stmtIf buffer
    | WHILE -> stmtWhile buffer
    | ITER -> stmtIter buffer
    | MATCH -> stmtMatch buffer
    | _ -> (
      let backup = Stream.backup buffer in
      try stmtBind buffer with
      | ParserError _ as exn -> (
        Stream.restore ~buffer ~backup;
        try
          let e = expression 0 buffer in
          let _ = consumeInContext buffer SEMI "expression statement" in
          match e with
          | { e = SECall _; _ } -> { s = SStmtBind ({ l = SLWild; loc = e.loc }, e); loc = e.loc }
          | _ ->
            let message = Printf.sprintf "The result of this expression must be explicitly discarded e.g. val _ = 1;" in
            raise (ParserError (Stream.makeError buffer message))
        with
        | _ -> raise exn)
      | exn -> raise exn)
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
      { s = SStmtBlock (CCList.rev acc); loc }
    | EOF ->
      let _ = expectInContext buffer RBRACE "statement block" in
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


and stmtConstant (buffer : Stream.stream) : top_stmt =
  let loc = Stream.location buffer in
  let _ = consumeInContext buffer CONSTANT "constant declaration" in
  let lhs = dexp_expression 0 buffer in
  let _ = consumeInContext buffer EQUAL "constant assignment" in
  let rhs = expression 0 buffer in
  let _ = consumeInContext buffer SEMI "constant declaration" in
  { top = STopConstant (lhs, rhs); loc }


and topStmt (buffer : Stream.stream) : top_stmt =
  try
    match Stream.peek buffer with
    | FUN -> stmtFunction buffer
    | TYPE -> stmtType buffer
    | EXTERNAL -> stmtExternal buffer
    | ENUM -> stmtEnum buffer
    | CONSTANT -> stmtConstant buffer
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
  CCList.rev (loop [])


let parseDExp (s : string) : dexp = dexp_expression 0 (Stream.fromString s)

let parseLhsExp (s : string) : lexp = lexp_expression 0 (Stream.fromString s)

let parseExp (s : string) : exp = expression 0 (Stream.fromString s)

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


let parseFunctionDecl (s : string) : function_def =
  let buffer = Stream.fromString s in
  fst (stmtFunctionDecl buffer)


let moduleName file =
  match Filename.extension file with
  | ".vult" -> file |> Filename.basename |> Filename.chop_extension |> String.capitalize_ascii
  | _ ->
    let message = Printf.sprintf "Invalid file extension. File '%s' must have the '.vult' extension" file in
    raise (Error.Errors [ Error.SimpleError message ])


(** Parses a buffer containing a list of statements and returns the results *)
let parseBuffer (file : string) (buffer : Stream.stream) =
  try
    let rec loop acc =
      match Stream.peek buffer with
      | EOF -> CCList.rev acc
      | _ -> loop (topStmt buffer :: acc)
    in
    let stmts = loop [] in
    if Stream.hasErrors buffer then
      raise (Error.Errors (CCList.rev (Stream.getErrors buffer)))
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
  | None ->
    Error.raiseErrorMsg ("Cannot read file '" ^ filename ^ "'. Check if the file exists and you have read permissions")


(** Parses a string containing a list of statements and returns the results *)
let parseString (file : string option) (text : string) =
  let buffer =
    match file with
    | Some f -> Stream.fromString ~file:f text
    | None -> Stream.fromString text
  in
  let file = Option.value file ~default:"live.vult" in
  let result = parseBuffer file buffer in
  result


(** Parses a string containing a list of statements and returns the results *)
let parseTagString (text : string) = tag 0 (Stream.fromString text)

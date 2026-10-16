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

(* Every parsing function takes the stream and returns the parsed value together with the stream
   that follows it. The stream is immutable, so trying an alternative only requires parsing again
   from a previous stream. *)

open Lexer
open Tokens
open Syntax
open Util

type parsed_file = {file: string; name: string; stmts: top_stmt list}

let fail = Stream.fail

(** Contextual wrapper functions for better error messages *)

let addContext (context : string) (f : unit -> 'a) : 'a =
  try f ()
  with Stream.ParserError (Error.PointedError (loc, msg), buffer) ->
    let enhanced_msg = Printf.sprintf "%s in %s" msg context in
    fail buffer (Error.PointedError (loc, enhanced_msg))

let consumeInContext (buffer : Stream.t) (kind : token_enum) (context : string) : Stream.t =
  addContext context (fun () -> Stream.consume buffer kind)

let expectInContext (buffer : Stream.t) (kind : token_enum) (context : string) : unit =
  addContext context (fun () -> Stream.expect buffer kind)

let notExpectedErrorInContext (token : token_enum token) (context : string) (expected : string) : Error.t =
  let token_str = tokenToString token in
  let message = Printf.sprintf "Found %s, but expected %s in %s" token_str expected context in
  Error.PointedError (Loc.getNext token.loc, message)

let rec moveToNextTopStatement (buffer : Stream.t) : Stream.t =
  match Stream.peek buffer with
  | EOF ->
      buffer
  | FUN | TYPE | EXTERNAL | ENUM ->
      buffer
  | _ ->
      moveToNextTopStatement (Stream.skip buffer)

let rec moveToNextStatement (buffer : Stream.t) : Stream.t =
  match Stream.peek buffer with
  | SEMI ->
      Stream.skip buffer
  | EOF ->
      buffer
  | VAL | IF | RET ->
      buffer
  | RBRACE ->
      Stream.skip buffer
  | _ ->
      moveToNextStatement (Stream.skip buffer)

let expToPath error (exp : exp) : path =
  match exp with
  | {e= SEId id; loc} ->
      {id; n= None; loc}
  | {e= SEMember ({e= SEId e; _}, id); loc} ->
      {id; n= Some e; loc}
  | _ ->
      error ()

(** Returns the left binding powers of the token *)
let getExpLbp (token : 'kind token) : int =
  match (token.kind, token.value) with
  | COMMA, _ ->
      20
  | OP, "||" ->
      30
  | OP, "&&" ->
      35
  | OP, "==" ->
      40
  | OP, "<>" ->
      40
  | OP, ">" ->
      40
  | GT, _ ->
      40
  | OP, "<" ->
      40
  | LT, _ ->
      40
  | OP, ">=" ->
      40
  | OP, "<=" ->
      40
  | OP, ">>" ->
      50
  | OP, "<<" ->
      50
  | OP, "+" ->
      55
  | OP, "-" ->
      55
  | OP, "&" ->
      60
  | OP, "^" ->
      45
  | OP, "|" ->
      40
  | OP, "*" ->
      60
  | OP, "/" ->
      60
  | OP, "%" ->
      60
  | COLON, _ ->
      70
  | LPAREN, _ ->
      80
  | LBRACK, _ ->
      80
  | LBRACE, _ ->
      80
  | DOT, _ ->
      90
  | _ ->
      0

let getTypeLbp (token : 'kind token) : int =
  match (token.kind, token.value) with
  | COMMA, _ ->
      20
  | GT, _ ->
      21
  | LT, _ ->
      21
  | COLON, _ ->
      70
  | LPAREN, _ ->
      80
  | LBRACK, _ ->
      80
  | DOT, _ ->
      90
  | _ ->
      0

let getLExpLbp (token : 'kind token) : int =
  match (token.kind, token.value) with
  | COMMA, _ ->
      20
  | LPAREN, _ ->
      80
  | LBRACK, _ ->
      80
  | DOT, _ ->
      90
  | COLON, _ ->
      90
  | _ ->
      0

let string (buffer : Stream.t) : string * Stream.t =
  let () = expectInContext buffer STRING "string literal" in
  let token, buffer = Stream.take buffer in
  (token.value, buffer)

let prattParser (rbp : int) (buffer : Stream.t) (lbp : 'kind token -> int)
    (nud : Stream.t -> 'kind token -> 'exp * Stream.t) (led : Stream.t -> 'kind token -> 'exp -> 'exp * Stream.t) :
    'exp * Stream.t =
  let token, buffer = Stream.take buffer in
  let left, buffer = nud buffer token in
  let rec loop left buffer =
    let token = Stream.current buffer in
    if rbp < lbp token then
      let left, buffer = led (Stream.skip buffer) token left in
      loop left buffer
    else (left, buffer)
  in
  loop left buffer

let commaSepList parser (buffer : Stream.t) =
  let rec loop acc buffer =
    (* power of 20 avoids returning a tuple instead of a list*)
    let e, buffer = parser 21 buffer in
    match Stream.peek buffer with COMMA -> loop (e :: acc) (Stream.skip buffer) | _ -> (CCList.rev (e :: acc), buffer)
  in
  loop [] buffer

let id_name (buffer : Stream.t) : (string * Loc.t) * Stream.t =
  let () = expectInContext buffer ID "identifier" in
  let token, buffer = Stream.take buffer in
  ((token.value, token.loc), buffer)

(* The value of an integer token. A literal that does not fit in an int is reported. *)
let intOfToken (token : _ token) : int =
  match int_of_string_opt token.value with
  | Some n ->
      n
  | None ->
      Error.raiseError ("The integer '" ^ token.value ^ "' is out of range") token.loc

let int_value (buffer : Stream.t) : (int * Loc.t) * Stream.t =
  let () = expectInContext buffer INT "integer value" in
  let token, buffer = Stream.take buffer in
  ((intOfToken token, token.loc), buffer)

let isTypeIntrinsic (name : string) : bool =
  String.equal name "typemax" || String.equal name "typemin" || String.equal name "typedefault"

(** Parses tag expressions *)
let rec tag (rbp : int) (buffer : Stream.t) : Ptags.tag * Stream.t = prattParser rbp buffer getExpLbp tag_nud tag_led

and tagExpressionList (buffer : Stream.t) : Ptags.tag list * Stream.t = commaSepList tag buffer

and tag_nud (buffer : Stream.t) (token : 'kind token) : Ptags.tag * Stream.t =
  let loc = token.loc in
  match (token.kind, token.value) with
  | ID, _ ->
      tag_id buffer token
  | OP, "-" ->
      tag_unary_op buffer token
  | INT, _ ->
      ({g= TagInt (intOfToken token); loc}, buffer)
  | XINT, _ ->
      ({g= TagInt (intOfToken token); loc}, buffer)
  | TRUE, _ ->
      ({g= TagBool true; loc}, buffer)
  | FALSE, _ ->
      ({g= TagBool false; loc}, buffer)
  | REAL, _ ->
      ({g= TagReal (float_of_string token.value); loc}, buffer)
  | STRING, _ ->
      ({g= TagString token.value; loc}, buffer)
  | _ ->
      let message =
        notExpectedErrorInContext token "tag expression" "identifier, number, boolean, string, or function call"
      in
      fail buffer message

(* An identifier, a call without arguments, a type intrinsic or a call with named arguments *)
and tag_id (buffer : Stream.t) (token : 'kind token) : Ptags.tag * Stream.t =
  let name = token.value in
  let loc = token.loc in
  match Stream.peek buffer with
  | LPAREN -> (
      let buffer = Stream.skip buffer in
      match Stream.peek buffer with
      | RPAREN ->
          ({g= TagId name; loc}, Stream.skip buffer)
      | QUOTED_ID when isTypeIntrinsic name ->
          (* Parse type intrinsic: typemax('t) *)
          let type_param_token, buffer = Stream.take buffer in
          let buffer = consumeInContext buffer RPAREN "type intrinsic call" in
          ({g= TagTypeIntrinsic (name, type_param_token.value); loc}, buffer)
      | _ ->
          let args, buffer = tag_pair_list buffer in
          let buffer = consumeInContext buffer RPAREN "tag function call" in
          ({g= TagCall {name; args}; loc}, buffer) )
  | _ ->
      ({g= TagId name; loc}, buffer)

and tag_unary_op (buffer : Stream.t) (token : 'kind token) : Ptags.tag * Stream.t =
  let right, buffer = tag 70 buffer in
  match right.g with
  | TagInt value ->
      ({right with g= TagInt (-value)}, buffer)
  | TagReal value ->
      ({right with g= TagReal (-.value)}, buffer)
  | _ ->
      Error.raiseError "Invalid unary operation. Can only apply '-' to numbers" token.loc

and tag_led (buffer : Stream.t) (token : 'kind token) (_ : Ptags.tag) : Ptags.tag * Stream.t =
  let message = notExpectedErrorInContext token "tag expression" "operator or end of expression" in
  fail buffer message

and tag_pair (bp : int) (buffer : Stream.t) : (string * Ptags.tag * Loc.t) * Stream.t =
  let (id, loc), buffer = id_name buffer in
  let buffer = consumeInContext buffer EQUAL "tag assignment" in
  let value, buffer = tag bp buffer in
  ((id, value, loc), buffer)

and tag_pair_list (buffer : Stream.t) : (string * Ptags.tag * Loc.t) list * Stream.t = commaSepList tag_pair buffer

let optional_tag (buffer : Stream.t) : Ptags.tag list * Stream.t =
  match Stream.peek buffer with
  | TAG ->
      let buffer = consumeInContext buffer TAG "attribute declaration" in
      let attr, buffer = tagExpressionList buffer in
      let buffer = consumeInContext buffer RBRACK "attribute list" in
      (attr, buffer)
  | _ ->
      ([], buffer)

let rec type_ (rbp : int) (buffer : Stream.t) : type_ * Stream.t = prattParser rbp buffer getTypeLbp type_nud type_led

(** Nud function for the Pratt parser *)
and type_nud (buffer : Stream.t) (token : 'kind token) : type_ * Stream.t =
  let loc = token.loc in
  match (token.kind, token.value) with
  | WILD, _ ->
      ({t= STUnbound; loc}, buffer)
  | ID, _ ->
      ({t= STId {id= token.value; n= None; loc}; loc}, buffer)
  | QUOTED_ID, _ ->
      ({t= STGenericType token.value; loc}, buffer)
  | INT, _ ->
      ({t= STSize (intOfToken token); loc}, buffer)
  | XINT, _ ->
      ({t= STSize (intOfToken token); loc}, buffer)
  | _ ->
      let message =
        Error.PointedError
          (token.loc, "Unexpected token in type definition. Expected a type name, wildcard '_', number, or generic type")
      in
      fail buffer message

and type_led (buffer : Stream.t) (token : 'kind token) (left : type_) : type_ * Stream.t =
  match token.kind with
  | DOT ->
      type_member buffer token left
  | LPAREN ->
      type_call RPAREN buffer token left
  | LT ->
      type_call GT buffer token left
  | _ ->
      let message =
        Error.PointedError (token.loc, "Unexpected token in type expression. Expected a member access or function call")
      in
      fail buffer message

and type_member (buffer : Stream.t) (token : 'kind token) (left : type_) : type_ * Stream.t =
  let right, buffer = type_ (getExpLbp token) buffer in
  match (right.t, left.t) with
  | STId rpath, STId {id; n= None; _} ->
      ({right with t= STId {rpath with n= Some id}}, buffer)
  | _ ->
      let message =
        Error.PointedError (token.loc, "Unexpected token in member access. Expected an identifier after the dot")
      in
      fail buffer message

and type_call clossing (buffer : Stream.t) (token : 'kind token) (left : type_) : type_ * Stream.t =
  let path =
    match left.t with
    | STId {id; _} ->
        id
    | _ ->
        let message = Error.PointedError (token.loc, "Expected a type name for generic type parameters") in
        fail buffer message
  in
  let args, buffer = if Stream.peek buffer = clossing then ([], buffer) else type_list buffer in
  let buffer = consumeInContext buffer clossing "type parameter list" in
  ({t= STComposed (path, args); loc= token.loc}, buffer)

and type_list (buffer : Stream.t) : type_ list * Stream.t = commaSepList type_ buffer

let rec dexp_expression (rbp : int) (buffer : Stream.t) : dexp * Stream.t =
  prattParser rbp buffer getLExpLbp dexp_nud dexp_led

and dexp_nud (buffer : Stream.t) (token : 'kind token) : dexp * Stream.t =
  match token.kind with
  | WILD ->
      ({d= SDWild; loc= token.loc}, buffer)
  | ID ->
      ({d= SDId (token.value, None); loc= token.loc}, buffer)
  | LPAREN -> (
    match Stream.peek buffer with
    | RPAREN ->
        let message =
          Error.PointedError
            ( token.loc
            , "Empty parentheses are not allowed in variable declarations. Use 'val x = expression;' or remove the \
               parentheses" )
        in
        fail buffer message
    | _ ->
        let e, buffer = dexp_expression 0 buffer in
        let buffer = consumeInContext buffer RPAREN "grouped declaration" in
        ({d= SDGroup e; loc= token.loc}, buffer) )
  | _ ->
      let message =
        Error.PointedError
          (token.loc, "Invalid variable declaration. Expected a variable name, wildcard '_', or grouped declaration")
      in
      fail buffer message

and dexp_led (buffer : Stream.t) (token : 'kind token) (left : dexp) : dexp * Stream.t =
  match token.kind with
  | COMMA ->
      dpair buffer token left
  | LBRACK ->
      darray buffer token left
  | COLON ->
      dtyped buffer token left
  | _ ->
      let message =
        Error.PointedError
          (token.loc, "Invalid syntax in variable declaration. Expected comma, array index, or type annotation")
      in
      fail buffer message

and dpair (buffer : Stream.t) (token : 'kind token) (left : dexp) : dexp * Stream.t =
  let right, buffer = dexp_expression (getLExpLbp token) buffer in
  let getElems e = match e.d with SDTuple elems -> elems | _ -> [e] in
  let elems1 = left |> getElems in
  let elems2 = right |> getElems in
  ({d= SDTuple (elems1 @ elems2); loc= left.loc}, buffer)

and dtyped (buffer : Stream.t) (token : 'kind token) (left : dexp) : dexp * Stream.t =
  let right, buffer = type_ 0 buffer in
  ({d= SDTyped (left, right); loc= token.loc}, buffer)

and darray (buffer : Stream.t) (token : 'kind token) (left : dexp) : dexp * Stream.t =
  let (size, _), buffer = int_value buffer in
  let buffer = consumeInContext buffer RBRACK "array size declaration" in
  match left with
  | {d= SDId (id, None); _} ->
      ({d= SDId (id, Some size); loc= token.loc}, buffer)
  | _ ->
      let message = Error.PointedError (token.loc, "Array declaration syntax error. Expected 'variable_name[size]'") in
      fail buffer message

let rec lexp_expression (rbp : int) (buffer : Stream.t) : lexp * Stream.t =
  prattParser rbp buffer getLExpLbp lexp_nud lexp_led

and lexp_nud (buffer : Stream.t) (token : 'kind token) : lexp * Stream.t =
  match token.kind with
  | WILD ->
      ({l= SLWild; loc= token.loc}, buffer)
  | ID ->
      ({l= SLId token.value; loc= token.loc}, buffer)
  | LPAREN -> (
    match Stream.peek buffer with
    | RPAREN ->
        let message =
          Error.PointedError
            (token.loc, "Empty parentheses are not allowed on left side of assignment. Use variable names or patterns")
        in
        fail buffer message
    | _ ->
        let e, buffer = lexp_expression 0 buffer in
        let buffer = consumeInContext buffer RPAREN "grouped left-hand side" in
        ({l= SLGroup e; loc= token.loc}, buffer) )
  | _ ->
      let message =
        Error.PointedError
          (token.loc, "Invalid assignment target. Expected a variable name, wildcard '_', or grouped expression")
      in
      fail buffer message

and lexp_led (buffer : Stream.t) (token : 'kind token) (left : lexp) : lexp * Stream.t =
  match token.kind with
  | COMMA ->
      lhs_pair buffer token left
  | DOT ->
      lexp_member buffer token left
  | LBRACK ->
      lexp_index buffer token left
  | _ ->
      let message =
        Error.PointedError (token.loc, "Invalid assignment syntax. Expected comma, member access, or array index")
      in
      fail buffer message

and lexp_member (buffer : Stream.t) (token : 'kind token) (left : lexp) : lexp * Stream.t =
  let right, buffer = lexp_expression (getLExpLbp token) buffer in
  match right.l with
  | SLMember (({l= SLId id; _} as i), n) ->
      ({right with l= SLMember ({i with l= SLMember (left, id)}, n)}, buffer)
  | SLId id ->
      ({right with l= SLMember (left, id)}, buffer)
  | _ ->
      let message =
        Error.PointedError
          (token.loc, "Invalid member access on left side of assignment. Expected a field name after the dot")
      in
      fail buffer message

and lhs_pair (buffer : Stream.t) (token : 'kind token) (left : lexp) : lexp * Stream.t =
  let right, buffer = lexp_expression (getLExpLbp token) buffer in
  let getElems e = match e.l with SLTuple elems -> elems | _ -> [e] in
  let elems1 = left |> getElems in
  let elems2 = right |> getElems in
  ({l= SLTuple (elems1 @ elems2); loc= left.loc}, buffer)

and lexp_index (buffer : Stream.t) (token : 'kind token) (left : lexp) : lexp * Stream.t =
  let index, buffer = expression 0 buffer in
  let buffer = consumeInContext buffer RBRACK "array index in assignment" in
  ({l= SLIndex {e= left; index}; loc= token.loc}, buffer)

and expression (rbp : int) (buffer : Stream.t) : exp * Stream.t = prattParser rbp buffer getExpLbp exp_nud exp_led

(** Nud function for the Pratt parser *)
and exp_nud (buffer : Stream.t) (token : 'kind token) : exp * Stream.t =
  let loc = token.loc in
  match (token.kind, token.value) with
  | OP, "-" ->
      unaryOp buffer token
  | ID, _ ->
      ({e= SEId token.value; loc}, buffer)
  | LPAREN, _ ->
      let e, buffer = expression 0 buffer in
      let buffer = consumeInContext buffer RPAREN "grouped expression" in
      ({e= SEGroup e; loc}, buffer)
  | INT, _ ->
      ({e= SEInt token.value; loc}, buffer)
  | XINT, _ ->
      ({e= SEInt token.value; loc}, buffer)
  | REAL, _ ->
      ({e= SEReal token.value; loc}, buffer)
  | FIXED, _ ->
      ({e= SEFixed token.value; loc}, buffer)
  | STRING, _ ->
      ({e= SEString token.value; loc}, buffer)
  | TRUE, _ ->
      ({e= SEBool true; loc}, buffer)
  | FALSE, _ ->
      ({e= SEBool false; loc}, buffer)
  | IF, _ ->
      let cond, buffer = expression 0 buffer in
      let buffer = consumeInContext buffer THEN "if-then-else expression" in
      let then_, buffer = expression 0 buffer in
      let buffer = consumeInContext buffer ELSE "if-then-else expression" in
      let else_, buffer = expression 0 buffer in
      ({e= SEIf {cond; then_; else_}; loc}, buffer)
  | LBRACE, _ ->
      anonymousRecord buffer token
  | LBRACK, _ -> (
    match Stream.peek buffer with
    | RBRACK ->
        let buffer = consumeInContext buffer RBRACK "empty array" in
        ({e= SEArray []; loc}, buffer)
    | _ ->
        let elems, buffer = expressionList buffer in
        let buffer = consumeInContext buffer RBRACK "array expression" in
        ({e= SEArray elems; loc}, buffer) )
  | _ ->
      let message =
        Error.PointedError (token.loc, "Unexpected token in expression. Expected a value, variable, or operator")
      in
      fail buffer message

and exp_led (buffer : Stream.t) (token : 'kind token) (left : exp) : exp * Stream.t =
  match token.kind with
  | OP ->
      binary_op buffer token left
  | LT ->
      binary_op buffer token left
  | GT ->
      binary_op buffer token left
  | COMMA ->
      pair buffer token left
  | DOT ->
      exp_member buffer token left
  | LPAREN ->
      call buffer token left
  | LBRACE ->
      record buffer token left
  | COLON ->
      named_call buffer token left
  | LBRACK ->
      exp_index buffer token left
  | _ ->
      let message =
        Error.PointedError
          (token.loc, "Invalid expression syntax. Expected an operator, function call, or end of expression")
      in
      fail buffer message

and pattern (rbp : int) (buffer : Stream.t) : pattern * Stream.t =
  prattParser rbp buffer getExpLbp pattern_nud pattern_led

(** Nud function for the Pratt parser *)
and pattern_nud (buffer : Stream.t) (token : 'kind token) : pattern * Stream.t =
  let loc = token.loc in
  match (token.kind, token.value) with
  | WILD, _ ->
      ({p= SPWild; loc}, buffer)
  | ID, _ ->
      ({p= SPId token.value; loc}, buffer)
  | LPAREN, _ ->
      let p, buffer = pattern 0 buffer in
      let buffer = consumeInContext buffer RPAREN "grouped pattern" in
      ({p= SPGroup p; loc}, buffer)
  | INT, _ ->
      ({p= SPInt token.value; loc}, buffer)
  | XINT, _ ->
      ({p= SPInt token.value; loc}, buffer)
  | REAL, _ ->
      ({p= SPReal token.value; loc}, buffer)
  | FIXED, _ ->
      ({p= SPFixed token.value; loc}, buffer)
  | STRING, _ ->
      ({p= SPString token.value; loc}, buffer)
  | TRUE, _ ->
      ({p= SPBool true; loc}, buffer)
  | FALSE, _ ->
      ({p= SPBool false; loc}, buffer)
  | _ ->
      let message =
        Error.PointedError
          (token.loc, "Invalid pattern in match expression. Expected a value, wildcard '_', or constructor")
      in
      fail buffer message

and pattern_led (buffer : Stream.t) (token : 'kind token) (left : pattern) : pattern * Stream.t =
  match token.kind with
  | COMMA ->
      pair_pattern buffer token left
  | DOT ->
      pattern_member buffer token left
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid pattern syntax. Expected comma or member access") in
      fail buffer message

and pair_pattern (buffer : Stream.t) (token : 'kind token) (left : pattern) : pattern * Stream.t =
  let right, buffer = pattern (getExpLbp token) buffer in
  let getElems e = match e.p with SPTuple elems -> elems | _ -> [e] in
  let elems1 = left |> getElems in
  let elems2 = right |> getElems in
  ({p= SPTuple (elems1 @ elems2); loc= left.loc}, buffer)

and pattern_member (buffer : Stream.t) (token : 'kind token) (left : pattern) : pattern * Stream.t =
  let right, buffer = pattern (getExpLbp token) buffer in
  match right.p with
  | SPMember (({p= SPId id; _} as i), n) ->
      ({right with p= SPMember ({i with p= SPMember (left, id)}, n)}, buffer)
  | SPId id ->
      ({right with p= SPMember (left, id)}, buffer)
  | _ ->
      let message =
        Error.PointedError (token.loc, "Invalid pattern member access. Expected an identifier after the dot")
      in
      fail buffer message

and exp_member (buffer : Stream.t) (token : 'kind token) (left : exp) : exp * Stream.t =
  let right, buffer = expression (getExpLbp token) buffer in
  match right.e with
  | SEMember (({e= SEId id; _} as i), n) ->
      ({right with e= SEMember ({i with e= SEMember (left, id)}, n)}, buffer)
  | SEId id ->
      ({right with e= SEMember (left, id)}, buffer)
  | _ ->
      let message = Error.PointedError (token.loc, "Invalid member access. Expected a field name after the dot") in
      fail buffer message

and pair (buffer : Stream.t) (token : 'kind token) (left : exp) : exp * Stream.t =
  let right, buffer = expression (getExpLbp token) buffer in
  let getElems e = match e.e with SETuple elems -> elems | _ -> [e] in
  let elems1 = left |> getElems in
  let elems2 = right |> getElems in
  ({e= SETuple (elems1 @ elems2); loc= Loc.merge left.loc right.loc}, buffer)

and named_call (buffer : Stream.t) (token : 'kind token) (left : exp) : exp * Stream.t =
  let right, buffer = expression (getExpLbp token) buffer in
  match (left, right) with
  | {e= SEId name; _}, {e= SECall ({instance= None; _} as call); _} ->
      ({right with e= SECall {call with instance= Some (name, None)}}, buffer)
  | {e= SEIndex {e= {e= SEId name; _}; index}; _}, {e= SECall ({instance= None; _} as call); _} ->
      ({right with e= SECall {call with instance= Some (name, Some index)}}, buffer)
  | ({e= SEId _; _} | {e= SEIndex {e= {e= SEId _; _}; _}; _}), _ ->
      let loc = left.loc in
      let error =
        Error.PointedError
          (Loc.getNext loc, "Invalid instance call syntax. Expected a function call after ':' (e.g., name:foo())")
      in
      fail buffer error
  | _, {e= SECall {instance= None; _}; _} ->
      let loc = left.loc in
      let error =
        Error.PointedError
          ( Loc.getNext loc
          , "Invalid instance name. Use a simple identifier like 'name:foo()' or indexed like 'name[1]:foo()'" )
      in
      fail buffer error
  | _, {e= SECall {instance= Some _; _}; _} ->
      let loc = left.loc in
      let error =
        Error.PointedError (Loc.getNext loc, "Cannot apply instance name to a function that already has an instance")
      in
      fail buffer error
  | _ ->
      let loc = Loc.merge left.loc right.loc in
      let error =
        Error.PointedError
          ( Loc.getNext loc
          , "Invalid expression. Missing operator between expressions? (e.g., 'a + (b)' instead of 'a (b)')" )
      in
      fail buffer error

and call (buffer : Stream.t) (_token : 'kind token) (left : exp) : exp * Stream.t =
  let error () =
    let message =
      Error.PointedError (left.loc, "Invalid function call. Expected a function name or module.function syntax")
    in
    fail buffer message
  in
  let path = expToPath error left in
  match path with
  | {id; n= None; _} when isTypeIntrinsic id ->
      typeIntrinsicCall buffer path
  | _ ->
      let args, buffer = match Stream.peek buffer with RPAREN -> ([], buffer) | _ -> expressionList buffer in
      let buffer = consumeInContext buffer RPAREN "function call" in
      ({e= SECall {instance= None; path; args}; loc= path.loc}, buffer)

(* Type intrinsics take a single generic type parameter: typemax('t), typemin('t), typedefault('t) *)
and typeIntrinsicCall (buffer : Stream.t) (path : path) : exp * Stream.t =
  match Stream.peek buffer with
  | QUOTED_ID ->
      let token, buffer = Stream.take buffer in
      let buffer = consumeInContext buffer RPAREN "type intrinsic call" in
      ({e= SETypeIntrinsic (path.id, token.value); loc= path.loc}, buffer)
  | RPAREN ->
      let message =
        Error.PointedError
          (path.loc, "Type intrinsic '" ^ path.id ^ "' requires a type parameter (e.g., " ^ path.id ^ "('t))")
      in
      fail buffer message
  | _ ->
      let message =
        Error.PointedError
          (path.loc, "Type intrinsic '" ^ path.id ^ "' expects a generic type parameter (e.g., " ^ path.id ^ "('t))")
      in
      fail buffer message

and exp_index (buffer : Stream.t) (token : 'kind token) (left : exp) : exp * Stream.t =
  let index, buffer = expression 0 buffer in
  let buffer = consumeInContext buffer RBRACK "array index" in
  ({e= SEIndex {e= left; index}; loc= token.loc}, buffer)

and unaryOp (buffer : Stream.t) (token : 'kind token) : exp * Stream.t =
  let right, buffer = expression 70 buffer in
  ({e= SEUnOp (token.value, right); loc= token.loc}, buffer)

and binary_op (buffer : Stream.t) (token : 'kind token) (left : exp) : exp * Stream.t =
  let right, buffer = expression (getExpLbp token) buffer in
  let loc = Loc.merge left.loc right.loc in
  ({e= SEOp (token.value, left, right); loc}, buffer)

and recordValue level (buffer : Stream.t) : (path * exp) * Stream.t =
  let (id, loc), buffer = id_name buffer in
  let buffer = consumeInContext buffer EQUAL "record field assignment" in
  let e, buffer = expression level buffer in
  (({id; n= None; loc}, e), buffer)

and recordValues (buffer : Stream.t) : (path * exp) list * Stream.t = commaSepList recordValue buffer

and record (buffer : Stream.t) (token : 'kind token) (left : exp) : exp * Stream.t =
  let error () =
    let message = Error.PointedError (left.loc, "Invalid record constructor. Expected a type name before the braces") in
    fail buffer message
  in
  let path = expToPath error left in
  let elems, buffer = recordValues buffer in
  let buffer = consumeInContext buffer RBRACE "record constructor" in
  ({e= SERecord {path= Some path; elems}; loc= token.loc}, buffer)

(* A record without the name of its type, which is inferred from its members *)
and anonymousRecord (buffer : Stream.t) (token : 'kind token) : exp * Stream.t =
  match Stream.peek buffer with
  | RBRACE ->
      (* The brace is consumed so that the parser continues after the record *)
      let buffer = consumeInContext buffer RBRACE "record constructor" in
      let message = Error.PointedError (token.loc, "A record needs at least one member") in
      fail buffer message
  | _ ->
      let elems, buffer = recordValues buffer in
      let buffer = consumeInContext buffer RBRACE "record constructor" in
      ({e= SERecord {path= None; elems}; loc= token.loc}, buffer)

and expressionList (buffer : Stream.t) : exp list * Stream.t = commaSepList expression buffer

and stmtVal (buffer : Stream.t) : stmt * Stream.t =
  let start_loc = Stream.location buffer in
  let buffer = consumeInContext buffer VAL "value declaration" in
  let lhs, buffer = dexp_expression 0 buffer in
  let rhs, buffer =
    match Stream.peek buffer with
    | EQUAL ->
        let rhs, buffer = expression 0 (Stream.skip buffer) in
        (Some rhs, buffer)
    | _ ->
        (None, buffer)
  in
  let end_loc = Stream.location buffer in
  let buffer = consumeInContext buffer SEMI "value declaration" in
  ({s= SStmtVal (lhs, rhs); loc= Loc.merge start_loc end_loc}, buffer)

and stmtMem (buffer : Stream.t) : stmt * Stream.t =
  let start_loc = Stream.location buffer in
  let buffer = consumeInContext buffer MEM "memory declaration" in
  let lhs, buffer = dexp_expression 0 buffer in
  let rhs, buffer =
    match Stream.peek buffer with
    | EQUAL ->
        let rhs, buffer = expression 0 (Stream.skip buffer) in
        (Some rhs, buffer)
    | _ ->
        (None, buffer)
  in
  let tags, buffer = optional_tag buffer in
  let end_loc = Stream.location buffer in
  let buffer = consumeInContext buffer SEMI "memory declaration" in
  ({s= SStmtMem (lhs, rhs, tags); loc= Loc.merge start_loc end_loc}, buffer)

and stmtReturn (buffer : Stream.t) : stmt * Stream.t =
  let start_loc = Stream.location buffer in
  let buffer = consumeInContext buffer RET "return statement" in
  let e, buffer = expression 0 buffer in
  let end_loc = Stream.location buffer in
  let buffer = consumeInContext buffer SEMI "return statement" in
  ({s= SStmtReturn e; loc= Loc.merge start_loc end_loc}, buffer)

and stmtBind (buffer : Stream.t) : stmt * Stream.t =
  let start_loc = Stream.location buffer in
  let e1, buffer = lexp_expression 0 buffer in
  match Stream.peek buffer with
  | EQUAL ->
      let buffer = consumeInContext buffer EQUAL "assignment" in
      let e2, buffer = expression 0 buffer in
      let end_loc = Stream.location buffer in
      let buffer = consumeInContext buffer SEMI "assignment" in
      ({s= SStmtBind (e1, e2); loc= Loc.merge start_loc end_loc}, buffer)
  | _ ->
      let message =
        Printf.sprintf "Invalid statement. All statements should be in the forms: \"a = b; \" or \"_ = b(); \" "
      in
      fail buffer (Stream.makeError buffer message)

(* A statement that does not start with a keyword is an assignment or a call whose result is
   discarded. When it is neither, the assignment error is the one reported. *)
and stmtBindOrCall (buffer : Stream.t) : stmt * Stream.t =
  try stmtBind buffer
  with Stream.ParserError (bind_error, bind_failed) -> (
    let parsed =
      try
        let e, buffer = expression 0 buffer in
        let buffer = consumeInContext buffer SEMI "expression statement" in
        Ok (e, buffer)
      with
      | Stream.ParserError (_, failed) ->
          Error failed
      | _ ->
          Error bind_failed
    in
    match parsed with
    | Error failed ->
        fail failed bind_error
    | Ok (({e= SECall _; _} as e), buffer) ->
        ({s= SStmtBind ({l= SLWild; loc= e.loc}, e); loc= e.loc}, buffer)
    | Ok (e, buffer) ->
        let message = "The result of this expression must be explicitly discarded e.g. val _ = 1;" in
        fail buffer (Error.makeError message e.loc) )

and stmtIf (buffer : Stream.t) : stmt * Stream.t =
  let start_loc = Stream.location buffer in
  let buffer = consumeInContext buffer IF "if statement" in
  let buffer = consumeInContext buffer LPAREN "if condition" in
  let cond, buffer = expression 0 buffer in
  let buffer = consumeInContext buffer RPAREN "if condition" in
  let tstm, buffer = stmtList buffer in
  match Stream.peek buffer with
  | ELSE ->
      let buffer = consumeInContext buffer ELSE "if-else statement" in
      let fstm, buffer = stmtList buffer in
      ({s= SStmtIf (cond, tstm, Some fstm); loc= Loc.merge start_loc fstm.loc}, buffer)
  | _ ->
      ({s= SStmtIf (cond, tstm, None); loc= Loc.merge start_loc tstm.loc}, buffer)

and matchCase (buffer : Stream.t) : (pattern * stmt) * Stream.t =
  let m, buffer = pattern 0 buffer in
  let buffer = consumeInContext buffer ARROW "match case" in
  let case, buffer = stmtList buffer in
  ((m, case), buffer)

and stmtMatch (buffer : Stream.t) : stmt * Stream.t =
  let start_loc = Stream.location buffer in
  let buffer = consumeInContext buffer MATCH "match statement" in
  let buffer = consumeInContext buffer LPAREN "match expression" in
  let e, buffer = expression 0 buffer in
  let buffer = consumeInContext buffer RPAREN "match expression" in
  let buffer = consumeInContext buffer LBRACE "match cases" in
  let first_case, buffer = matchCase buffer in
  let rec loop cases buffer =
    match Stream.peek buffer with
    | RBRACE ->
        (CCList.rev cases, buffer)
    | _ ->
        let case, buffer = matchCase buffer in
        loop (case :: cases) buffer
  in
  let other_cases, buffer = loop [] buffer in
  let end_loc = Stream.location buffer in
  let buffer = consumeInContext buffer RBRACE "match cases" in
  ({s= SStmtMatch {e; first_case; other_cases}; loc= Loc.merge start_loc end_loc}, buffer)

and typedArgOpt (buffer : Stream.t) : arg * Stream.t =
  let () = expectInContext buffer ID "function parameter" in
  let token, buffer = Stream.take buffer in
  match Stream.peek buffer with
  | COLON ->
      let e, buffer = type_ 20 (Stream.skip buffer) in
      ((token.value, Some e, token.loc), buffer)
  | _ ->
      ((token.value, None, token.loc), buffer)

and typedArg (buffer : Stream.t) : arg * Stream.t =
  let () = expectInContext buffer ID "typed function parameter" in
  let token, buffer = Stream.take buffer in
  let buffer = consumeInContext buffer COLON "parameter type" in
  let e, buffer = type_ 20 buffer in
  ((token.value, Some e, token.loc), buffer)

and parseGenericParam (buffer : Stream.t) : generic_param * Stream.t =
  let () = expectInContext buffer QUOTED_ID "generic parameter" in
  let token, buffer = Stream.take buffer in
  let param_name = token.value in
  (* Validate parameter name *)
  ( if String.length param_name = 0 then
      let msg = "Generic parameter name cannot be empty" in
      fail buffer (Error.PointedError (token.loc, msg)) ) ;
  (* Check for invalid characters in parameter name *)
  ( if not (Str.string_match (Str.regexp "^[a-zA-Z_][a-zA-Z0-9_]*$") param_name 0) then
      let msg =
        Printf.sprintf
          "Generic parameter name '%s' contains invalid characters. Use only letters, numbers, and underscore."
          param_name
      in
      fail buffer (Error.PointedError (token.loc, msg)) ) ;
  (* Check for reserved names *)
  let reserved_names =
    ["int"; "real"; "bool"; "string"; "unit"; "if"; "else"; "while"; "for"; "return"; "fun"; "type"; "enum"]
  in
  ( if List.mem param_name reserved_names then
      let msg = Printf.sprintf "Generic parameter name '%s' is a reserved keyword" param_name in
      fail buffer (Error.PointedError (token.loc, msg)) ) ;
  match Stream.peek buffer with
  | COLON -> (
      let type_expr, buffer = type_ 20 (Stream.skip buffer) in
      (* Check if this looks like a function type - for now, simple heuristic *)
      match type_expr.t with
      | STComposed ("fun", _) ->
          (GParamFunction (param_name, Some type_expr), buffer)
      | _ ->
          (GParamConstant (param_name, type_expr, token.loc), buffer) )
  | _ ->
      (* No type annotation - use naming convention to determine type *)
      (* If parameter name starts with 'f' or ends with 'fn'/'func', assume it's a function *)
      if String.length param_name > 0 && param_name.[0] = 'f' then (GParamFunction (param_name, None), buffer)
      else
        (* Default to constant parameter with unbound type (will be inferred) *)
        (GParamConstant (param_name, {t= STUnbound; loc= token.loc}, token.loc), buffer)

and argList arg_parser (buffer : Stream.t) : arg list * Stream.t =
  match Stream.peek buffer with
  | ID -> (
      let first, buffer = arg_parser buffer in
      match Stream.peek buffer with
      | COMMA ->
          let buffer = consumeInContext buffer COMMA "parameter list" in
          let rest, buffer = argList arg_parser buffer in
          (first :: rest, buffer)
      | _ ->
          ([first], buffer) )
  | _ ->
      ([], buffer)

and parseGenericsAndArguments (buffer : Stream.t) : (generic_param list * arg list * param_slot list) * Stream.t =
  (* Helper to extract name from generic_param *)
  let getGenericName (param : generic_param) : string =
    match param with GParamFunction (name, _) -> name | GParamType name -> name | GParamConstant (name, _, _) -> name
  in
  let rec loop generic_params generic_names args arg_names params buffer =
    match Stream.peek buffer with
    | QUOTED_ID -> (
        (* Parse specialized parameter *)
        let generic_param, buffer = parseGenericParam buffer in
        let param_name = getGenericName generic_param in
        (* Check for duplicate generic parameter names *)
        let () =
          if List.mem param_name generic_names then
            let token = Stream.current buffer in
            let msg = Printf.sprintf "Duplicate generic parameter name '%s'" param_name in
            fail buffer (Error.PointedError (token.loc, msg))
        in
        (* Check for conflict with existing regular argument names *)
        let () =
          if List.mem param_name arg_names then
            let token = Stream.current buffer in
            let msg = Printf.sprintf "Generic parameter '%s' conflicts with function argument name" param_name in
            fail buffer (Error.PointedError (token.loc, msg))
        in
        let params = SlotGeneric generic_param :: params in
        match Stream.peek buffer with
        | COMMA ->
            let buffer = consumeInContext buffer COMMA "parameter list" in
            loop (generic_param :: generic_params) (param_name :: generic_names) args arg_names params buffer
        | _ ->
            ((List.rev (generic_param :: generic_params), List.rev args, List.rev params), buffer) )
    | ID -> (
        (* Parse regular argument *)
        let arg, buffer = typedArgOpt buffer in
        let arg_name, _, arg_loc = arg in
        (* Check for conflict with existing generic parameter names *)
        let () =
          if List.mem arg_name generic_names then
            let msg = Printf.sprintf "Function argument '%s' conflicts with generic parameter name" arg_name in
            fail buffer (Error.PointedError (arg_loc, msg))
        in
        (* Note: Duplicate argument name checking is handled by the type checker *)
        let params = SlotArg arg :: params in
        match Stream.peek buffer with
        | COMMA ->
            let buffer = consumeInContext buffer COMMA "parameter list" in
            loop generic_params generic_names (arg :: args) (arg_name :: arg_names) params buffer
        | _ ->
            ((List.rev generic_params, List.rev (arg :: args), List.rev params), buffer) )
    | _ ->
        (* No more arguments *)
        ((List.rev generic_params, List.rev args, List.rev params), buffer)
  in
  loop [] [] [] [] [] buffer

and stmtExternal (buffer : Stream.t) : top_stmt * Stream.t =
  let buffer = Stream.skip buffer in
  let (name, loc), buffer = id_name buffer in
  let buffer = consumeInContext buffer LPAREN "external function parameter list" in
  let args, buffer = match Stream.peek buffer with RPAREN -> ([], buffer) | _ -> argList typedArg buffer in
  let buffer = consumeInContext buffer RPAREN "external function parameter list" in
  let buffer = consumeInContext buffer COLON "external function return type" in
  let type_, buffer = type_ 0 buffer in
  let (link_name, tags), buffer =
    match Stream.peek buffer with
    | STRING ->
        let link_name, buffer = string buffer in
        let tags, buffer = optional_tag buffer in
        ((Some link_name, tags), buffer)
    | TAG ->
        let tags, buffer = optional_tag buffer in
        ((None, tags), buffer)
    | _ ->
        let message = Printf.sprintf "Expecting a string with a link name or a tag" in
        fail buffer (Stream.makeError buffer message)
  in
  let buffer = consumeInContext buffer SEMI "external function declaration" in
  ({top= STopExternal ({name; args; t= Some type_; tags; loc}, link_name); loc}, buffer)

(* Extract quoted identifiers from a type for implicit generic type parameters *)
and extract_quoted_identifiers_from_type (t : type_) : string list =
  let rec extract_from_type_d = function
    | STGenericType id ->
        [id] (* Explicit generic type parameter *)
    | STComposed (_, types) ->
        CCList.flat_map extract_from_type types
    | STUnbound | STSize _ | STId _ ->
        []
  and extract_from_type (t : type_) : string list = extract_from_type_d t.t in
  extract_from_type t

(* Extract quoted identifiers from function arguments *)
and extract_quoted_identifiers_from_args (args : arg list) : string list =
  CCList.flat_map
    (fun (_, type_opt, _) -> match type_opt with Some t -> extract_quoted_identifiers_from_type t | None -> [])
    args

(* Create implicit generic type parameters from quoted identifiers *)
and create_implicit_generic_params (quoted_ids : string list) : generic_param list =
  (* Remove duplicates and create GParamType entries *)
  quoted_ids |> CCList.sort_uniq ~cmp:String.compare |> CCList.map (fun param_name -> GParamType param_name)

and stmtFunctionDecl (buffer : Stream.t) : (function_def * Loc.t) * Stream.t =
  let buffer = Stream.skip buffer in
  let (name, loc), buffer = id_name buffer in
  let buffer = consumeInContext buffer LPAREN "function parameter list" in
  let (explicit_generic_params, args, params), buffer =
    match Stream.peek buffer with RPAREN -> (([], [], []), buffer) | _ -> parseGenericsAndArguments buffer
  in
  let buffer = consumeInContext buffer RPAREN "function parameter list" in
  let t, buffer =
    match Stream.peek buffer with
    | COLON ->
        let t, buffer = type_ 0 (Stream.skip buffer) in
        (Some t, buffer)
    | _ ->
        (None, buffer)
  in
  (* Extract implicit generic type parameters from argument types and return type *)
  let quoted_ids_from_args = extract_quoted_identifiers_from_args args in
  let quoted_ids_from_return =
    match t with Some return_type -> extract_quoted_identifiers_from_type return_type | None -> []
  in
  let all_quoted_ids = quoted_ids_from_args @ quoted_ids_from_return in
  let implicit_generic_params = create_implicit_generic_params all_quoted_ids in
  (* Combine explicit and implicit generic parameters *)
  let all_generic_params = explicit_generic_params @ implicit_generic_params in
  let tags, buffer = optional_tag buffer in
  let body, buffer = stmtList buffer in
  let next, buffer =
    match Stream.peek buffer with
    | AND ->
        let (def, _), buffer = stmtFunctionDecl buffer in
        (Some def, buffer)
    | _ ->
        (None, buffer)
  in
  (({name; generic_params= all_generic_params; args; params; t; next; tags; loc; body}, loc), buffer)

and stmtFunction (buffer : Stream.t) : top_stmt * Stream.t =
  let (def, loc), buffer = stmtFunctionDecl buffer in
  ({top= STopFunction def; loc}, buffer)

and stmtType (buffer : Stream.t) : top_stmt * Stream.t =
  let buffer = consumeInContext buffer TYPE "type declaration" in
  let (name, loc), buffer = id_name buffer in
  match Stream.peek buffer with
  | LBRACE ->
      let members, buffer = type_member_list (Stream.skip buffer) in
      let buffer = consumeInContext buffer RBRACE "type declaration" in
      ({top= STopType {name; members}; loc}, buffer)
  | SEMI ->
      ({top= STopType {name; members= []}; loc}, Stream.skip buffer)
  | _ ->
      let got = tokenToString (Stream.current buffer) in
      let message = Printf.sprintf "Expecting a list of value declarations '{ val x:... }' but got %s" got in
      fail buffer (Stream.makeError buffer message)

and type_member_list (buffer : Stream.t) =
  match Stream.peek buffer with
  | RBRACE ->
      fail buffer (Stream.makeError buffer "This type declaration is empty")
  | VAL ->
      let rec loop acc buffer =
        match Stream.peek buffer with
        | VAL ->
            let decl, buffer = type_elem buffer in
            let buffer = consumeInContext buffer SEMI "type member" in
            loop (decl :: acc) buffer
        | _ ->
            (CCList.rev acc, buffer)
      in
      loop [] buffer
  | _ ->
      let got = tokenToString (Stream.current buffer) in
      let message = Printf.sprintf "Expecting a list of value declarations '{ val x:... }' but got %s" got in
      fail buffer (Stream.makeError buffer message)

and type_elem (buffer : Stream.t) =
  let buffer = consumeInContext buffer VAL "type member declaration" in
  let (name, loc), buffer = id_name buffer in
  let buffer = consumeInContext buffer COLON "type member type" in
  let type_, buffer = type_ 10 buffer in
  let tags, buffer = optional_tag buffer in
  ((name, type_, tags, loc), buffer)

and stmtEnum (buffer : Stream.t) : top_stmt * Stream.t =
  let buffer = consumeInContext buffer ENUM "enum declaration" in
  let (name, loc), buffer = id_name buffer in
  match Stream.peek buffer with
  | LBRACE ->
      let members, buffer = enum_member_type (Stream.skip buffer) in
      let buffer = consumeInContext buffer RBRACE "enum declaration" in
      ({top= STopEnum {name; members}; loc}, buffer)
  | _ ->
      let got = tokenToString (Stream.current buffer) in
      let message = Printf.sprintf "Expecting a list of value declarations '{ val x:... }' but got %s" got in
      fail buffer (Stream.makeError buffer message)

and enum_member_type (buffer : Stream.t) =
  match Stream.peek buffer with
  | RBRACE ->
      fail buffer (Stream.makeError buffer "The enumeration declaration is empty")
  | ID ->
      let rec loop acc buffer =
        match Stream.peek buffer with
        | ID -> (
            let decl, buffer = enum_name buffer in
            match Stream.peek buffer with
            | COMMA ->
                let buffer = consumeInContext buffer COMMA "enum member list" in
                loop (decl :: acc) buffer
            | RBRACE ->
                (CCList.rev (decl :: acc), buffer)
            | _ ->
                fail buffer (Stream.makeError buffer "Expecting more enumeration elements") )
        | _ ->
            (CCList.rev acc, buffer)
      in
      loop [] buffer
  | _ ->
      let got = tokenToString (Stream.current buffer) in
      let message = Printf.sprintf "Expecting a list of enumeration elements but got %s" got in
      fail buffer (Stream.makeError buffer message)

and enum_name (buffer : Stream.t) =
  let (name, loc), buffer = id_name buffer in
  if startsUppercase name then ((name, loc), buffer)
  else fail buffer (Stream.makeError buffer "Enumeration elements should start with uppercase")

and stmtWhile (buffer : Stream.t) : stmt * Stream.t =
  let start_loc = Stream.location buffer in
  let buffer = consumeInContext buffer WHILE "while loop" in
  let buffer = consumeInContext buffer LPAREN "while condition" in
  let cond, buffer = expression 0 buffer in
  let buffer = consumeInContext buffer RPAREN "while condition" in
  let tstm, buffer = stmtList buffer in
  ({s= SStmtWhile (cond, tstm); loc= Loc.merge start_loc tstm.loc}, buffer)

and stmtIter (buffer : Stream.t) : stmt * Stream.t =
  let start_loc = Stream.location buffer in
  let buffer = consumeInContext buffer ITER "iter loop" in
  let buffer = consumeInContext buffer LPAREN "iter parameters" in
  let (name, id_loc), buffer = id_name buffer in
  let buffer = consumeInContext buffer COMMA "iter parameters" in
  let value, buffer = expression 0 buffer in
  let buffer = consumeInContext buffer RPAREN "iter parameters" in
  let body, buffer = stmtList buffer in
  ({s= SStmtIter {id= (name, id_loc); value; body}; loc= Loc.merge start_loc body.loc}, buffer)

(* Parses a statement. On a syntax error, the error is recorded in the stream and the parsing
   continues at the next statement. *)
and stmt (buffer : Stream.t) : stmt * Stream.t =
  try
    match Stream.peek buffer with
    | VAL ->
        stmtVal buffer
    | MEM ->
        stmtMem buffer
    | RET ->
        stmtReturn buffer
    | IF ->
        stmtIf buffer
    | WHILE ->
        stmtWhile buffer
    | ITER ->
        stmtIter buffer
    | MATCH ->
        stmtMatch buffer
    | _ ->
        stmtBindOrCall buffer
  with Stream.ParserError (error, failed) ->
    let buffer = Stream.appendError failed error in
    let buffer = moveToNextStatement buffer in
    ({s= SStmtError; loc= Loc.default}, buffer)

and stmtList (buffer : Stream.t) : stmt * Stream.t =
  let start_loc = Stream.location buffer in
  let rec loop acc buffer =
    match Stream.peek buffer with
    | RBRACE ->
        let end_loc = Stream.location buffer in
        let loc = Loc.merge start_loc end_loc in
        ({s= SStmtBlock (CCList.rev acc); loc}, Stream.skip buffer)
    | EOF ->
        let () = expectInContext buffer RBRACE "statement block" in
        ({s= SStmtBlock []; loc= start_loc}, buffer)
    | _ ->
        let s, buffer = stmt buffer in
        loop (s :: acc) buffer
  in
  match Stream.peek buffer with
  | LBRACE ->
      loop [] (Stream.skip buffer)
  | _ ->
      let s, buffer = stmt buffer in
      ({s= SStmtBlock [s]; loc= s.loc}, buffer)

and stmtConstant (buffer : Stream.t) : top_stmt * Stream.t =
  let loc = Stream.location buffer in
  let buffer = consumeInContext buffer CONSTANT "constant declaration" in
  let lhs, buffer = dexp_expression 0 buffer in
  let buffer = consumeInContext buffer EQUAL "constant assignment" in
  let rhs, buffer = expression 0 buffer in
  let buffer = consumeInContext buffer SEMI "constant declaration" in
  ({top= STopConstant (lhs, rhs); loc}, buffer)

(* Parses a top statement. On a syntax error, the error is recorded in the stream and the parsing
   continues at the next top statement. *)
and topStmt (buffer : Stream.t) : top_stmt * Stream.t =
  try
    match Stream.peek buffer with
    | FUN ->
        stmtFunction buffer
    | TYPE ->
        stmtType buffer
    | EXTERNAL ->
        stmtExternal buffer
    | ENUM ->
        stmtEnum buffer
    | CONSTANT ->
        stmtConstant buffer
    | _ ->
        let message = Printf.sprintf "Expecting a function or type declaration" in
        fail buffer (Stream.makeError buffer message)
  with Stream.ParserError (error, failed) ->
    let buffer = Stream.appendError failed error in
    let buffer = moveToNextTopStatement buffer in
    ({top= STopError; loc= Loc.default}, buffer)

(* Runs a parser on a string, ignoring the tokens that follow the parsed value *)
let parseStringWith (parser : Stream.t -> 'a * Stream.t) (s : string) : 'a = fst (parser (Stream.fromString s))

let parseDExp (s : string) : dexp = parseStringWith (dexp_expression 0) s

let parseLhsExp (s : string) : lexp = parseStringWith (lexp_expression 0) s

let parseExp (s : string) : exp = parseStringWith (expression 0) s

let parseId (s : string) : string = fst (parseStringWith id_name s)

(** Parses an type given a string *)
let parseType (s : string) : type_ = parseStringWith (type_ 0) s

(** Parses an statement given a string *)
let parseStmt (s : string) : stmt = parseStringWith stmt s

(** Parses a list of statements given a string *)
let parseStmtList (s : string) : stmt = parseStringWith stmtList s

let parseFunctionDecl (s : string) : function_def = fst (parseStringWith stmtFunctionDecl s)

let moduleName file =
  match Filename.extension file with
  | ".vult" ->
      file |> Filename.basename |> Filename.chop_extension |> String.capitalize_ascii
  | _ ->
      let message = Printf.sprintf "Invalid file extension. File '%s' must have the '.vult' extension" file in
      raise (Error.Errors [Error.SimpleError message])

(** Parses a buffer containing a list of statements and returns the results *)
let parseBuffer (file : string) (buffer : Stream.t) =
  let rec loop acc buffer =
    match Stream.peek buffer with
    | EOF ->
        (CCList.rev acc, buffer)
    | _ ->
        let top, buffer = topStmt buffer in
        loop (top :: acc) buffer
  in
  match loop [] buffer with
  | exception Stream.ParserError (error, _) ->
      raise (Error.Errors [error])
  | stmts, buffer -> (
    match Stream.errors buffer with
    | [] ->
        let name = moduleName file in
        {stmts; file; name}
    | errors ->
        raise (Error.Errors errors) )

(** Parses a file whose contents are obtained with [read] *)
let parseFileWith (read : string -> string option) (filename : string) =
  match read filename with
  | Some contents ->
      let buffer = Stream.fromString ~file:filename contents in
      parseBuffer filename buffer
  | None ->
      Error.raiseErrorMsg ("Cannot read file '" ^ filename ^ "'. Check if the file exists and you have read permissions")

(** Parses a file containing a list of statements and returns the results *)
let parseFile (filename : string) = parseFileWith FileIO.read filename

(** Parses a string containing a list of statements and returns the results *)
let parseString (file : string option) (text : string) =
  let buffer = Stream.fromString ?file text in
  let file = Option.value file ~default:"live.vult" in
  parseBuffer file buffer

(** Parses a string containing a list of statements and returns the results *)
let parseTagString (text : string) : Ptags.tag = parseStringWith (tag 0) text

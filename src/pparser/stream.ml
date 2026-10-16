(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz, Carl Jönsson

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

(* Immutable token stream consumed by the parser. The whole input is tokenized up front and the
   comments are dropped, so advancing returns a new stream and the old one stays valid: the parser
   backtracks by reusing a previous stream. The stream also carries the errors recovered so far. *)

open Tokens
open Util

type token = token_enum Tokens.token

(* [rest] never contains comments and its last token is EOF. Once it runs out, the stream stays at
   EOF so that the parser reports the unexpected end of the file. *)
type t = {current: token; prev: token; rest: token list; errors: Error.t list}

(* A parse failure together with the stream where it happened, which is where the error recovery
   continues from *)
exception ParserError of Error.t * t

let fail (s : t) (error : Error.t) : 'a = raise (ParserError (error, s))

let isComment (token : token) : bool = match token.kind with LINE_COMMENT | BLOCK_COMMENT -> true | _ -> false

(* Tokenizes the whole input. The comments are emitted by the lexer and dropped here. *)
let fromLexbuf (source : Loc.source) (lexbuf : Lexing.lexbuf) : t =
  let rec loop acc =
    let token = Lexer.next_token_config source comment_config lexbuf in
    match token.kind with
    | EOF -> (
      match CCList.rev acc with
      | [] ->
          {current= token; prev= token; rest= []; errors= []}
      | first :: rest ->
          {current= first; prev= first; rest= rest @ [token]; errors= []} )
    | _ when isComment token ->
        loop acc
    | _ ->
        loop (token :: acc)
  in
  loop []

(** Creates a token stream given a string *)
let fromString ?file (str : string) : t =
  let lexbuf = Lexing.from_string str in
  let () = Option.iter (Lexing.set_filename lexbuf) file in
  let source = match file with Some f -> Loc.File f | None -> Loc.Text str in
  fromLexbuf source lexbuf

(** Returns the stream advanced by one token *)
let skip (s : t) : t =
  match s.rest with [] -> {s with prev= s.current} | next :: rest -> {s with current= next; prev= s.current; rest}

(** Returns the current token and the stream after it *)
let take (s : t) : token * t = (s.current, skip s)

(** Returns the current token in the stream *)
let current (s : t) : token = s.current

(** Returns the current location of the stream *)
let location (s : t) : Loc.t = s.current.loc

(** Returns the kind of the current token *)
let peek (s : t) : token_enum = s.current.kind

(** Make an error at the current location *)
let makeError (s : t) (message : string) : Error.t = Error.PointedError (Loc.getNext s.prev.loc, message)

(** Returns the recovered errors in the order they were found *)
let errors (s : t) : Error.t list = CCList.rev s.errors

let appendError (s : t) (error : Error.t) : t = {s with errors= error :: s.errors}

let unexpected (s : t) (kind : token_enum) (got : string) : 'a =
  let expected = Lexer.kindToString kind in
  let message =
    match s.current.kind with
    | EOF ->
        Printf.sprintf "Expecting a %s but the file ended" expected
    | _ ->
        Printf.sprintf "Expecting a %s but got %s" expected got
  in
  fail s (makeError s message)

(** Checks that the current token matches the given kind *)
let expect (s : t) (kind : token_enum) : unit =
  if s.current.kind <> kind then unexpected s kind (Lexer.kindToString s.current.kind)

(** Checks that the current token matches the given kind and skips it *)
let consume (s : t) (kind : token_enum) : t =
  if s.current.kind <> kind then unexpected s kind (Lexer.tokenToString s.current) else skip s

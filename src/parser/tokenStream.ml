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

open ParserTypes

type source =
   | File of string
   | Text of string

(** Parsing exception *)
exception ParserError of Error.t

module type TokenKindSig = sig
   type kind
   val next     : Lexing.lexbuf -> kind token
   val kindStr  : kind -> string
   val tokenStr : kind token -> string
   val isEOF    : kind -> bool
   val getEOF   : kind
end

module TokenStream(S:TokenKindSig) = struct

   (** Type containing the stream of tokens *)
   type stream =
      {
         lexbuf             : Lexing.lexbuf;
         mutable has_errors : bool;
         mutable errors     : Error.t list;
         mutable peeked     : S.kind token;
         mutable prev       : S.kind token;
         source             : source;
      }

   (** Skips one token *)
   let skip (buffer:stream) : unit =
      buffer.prev <- buffer.peeked;
      buffer.peeked <- S.next buffer.lexbuf

   (** Returns the current token in the buffer *)
   let current (buffer:stream) : S.kind token =
      buffer.peeked

   (** Returns the current location of the buffer *)
   let location (buffer:stream) : Loc.t =
      buffer.peeked.loc

   (** Returns the kind of the current token *)
   let peek (buffer:stream) : S.kind =
      (current buffer).kind

   let makeError (buffer:stream) (message:string) : Error.t =
      Error.PointedError(Loc.getNext buffer.prev.loc,message)

   let setErrors (buffer:stream) (value:bool) : unit =
      buffer.has_errors<-value

   let hasErrors (buffer:stream) : bool =
      buffer.has_errors

   let getErrors (buffer:stream) : Error.t list =
      buffer.errors

   let notExpectedError (token:S.kind token) : Error.t =
      let message = Printf.sprintf "Not expecting to find %s" (S.kindStr token.kind) in
      Error.PointedError(Loc.getNext token.loc,message)

   let appendError (buffer:stream) (error:Error.t) =
      buffer.errors <- error::buffer.errors

   (** Checks that the next token matches the given kind and skip it *)
   let consume (buffer:stream) (kind:S.kind) : unit =
      match buffer.peeked with
      | t when t.kind = kind ->
         let _ = buffer.prev <- buffer.peeked in
         buffer.peeked <- S.next buffer.lexbuf
      | t when S.isEOF t.kind ->
         let expected = S.kindStr kind in
         let message = Printf.sprintf "Expecting a %s but the file ended" expected in
         raise (ParserError(makeError buffer message))
      | got_token ->
         let expected = S.kindStr kind in
         let got = S.tokenStr got_token in
         let message =  Printf.sprintf "Expecting a %s but got %s" expected got in
         raise (ParserError(makeError buffer message))

   (** Checks that the next token matches *)
   let expect (buffer:stream) (kind:S.kind) : unit =
      match buffer.peeked with
      | t when t.kind=kind -> ()
      | t when S.isEOF t.kind ->
         let expected = S.kindStr kind in
         let message = Printf.sprintf "Expecting a %s but the file ended" expected in
         raise (ParserError(makeError buffer message))
      | got_token ->
         let expected = S.kindStr kind in
         let got = S.kindStr got_token.kind in
         let message = Printf.sprintf "Expecting a %s but got %s" expected got in
         raise (ParserError(makeError buffer message))

   (** Optionally consumes the given token *)
   let optConsume (buffer:stream) (kind:S.kind) : unit =
      match buffer.peeked with
      | t when t.kind = kind ->
         skip buffer
      | _ -> ()

   (** Returns an empty 'lexed_lines' type *)
   let emptyLexedLines () =
      {
         current_line = Buffer.create 100;
         all_lines    = [];
      }

   (** Creates a token stream given a string *)
   let fromString (str:string) : stream =
      let lexbuf = Lexing.from_string str in
      let lines = emptyLexedLines () in
      let first =  S.next lexbuf in
      { lexbuf = lexbuf; peeked = first; prev = first ; has_errors = false; errors= []; source = Text(str) }

   (** Creates a token stream given a channel *)
   let fromChannel (chan:in_channel) (file:string) : stream =
      let lexbuf = Lexing.from_channel chan in
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file };
      let first =  S.next lexbuf in
      { lexbuf = lexbuf; peeked = first; prev = first ; has_errors = false; errors = []; source = File(file) }


end

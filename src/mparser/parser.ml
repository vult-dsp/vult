module I = Grammar.MenhirInterpreter

let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> assert false


let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> I.current_state_number (env checkpoint)


let rec loop lexbuf (checkpoint : (Bast.state -> Bast.state * Pparser.Syntax.stmts) I.checkpoint) :
    Bast.state -> Bast.state * Pparser.Syntax.stmts =
  match checkpoint with
  | I.InputNeeded _env ->
    let token = Lexer.token false lexbuf in
    let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
    let checkpoint = I.offer checkpoint (token, startp, endp) in
    loop lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
    let checkpoint = I.resume checkpoint in
    loop lexbuf checkpoint
  | I.HandlingError _env ->
    let state = state checkpoint in
    let msg = Grammar_msg.message state in
    let msg = CCString.replace ~sub:"%token" ~by:(Lexing.lexeme lexbuf) msg in
    (* Stop parsing and return a function that produces the error *)
    fun state ->
      Util.Error.raiseError msg (Bast.mk_loc state (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf))
  | I.Accepted v -> v
  | I.Rejected -> assert false


let parseBuffer lexbuf =
  try loop lexbuf (Grammar.Incremental.program lexbuf.lex_curr_p) with
  | Lexer.LexError msg ->
    let pos = lexbuf.lex_curr_p in
    Printf.eprintf "Lexical error at %s:%d:%d: %s\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg;
    exit 1


let parseFile (file : string) =
  let p =
    match Util.FileIO.read file with
    | Some contents ->
      let lexbuf = Lexing.from_string contents in
      parseBuffer lexbuf
    | None -> Util.Error.raiseErrorMsg ("Could not open the file " ^ file)
  in
  let stmts = snd (p file) in
  let name = Pparser.Parse.moduleName file in
  Pparser.Parse.{ file; name; stmts }


let parseString (file : string option) (text : string) =
  let lexbuf = Lexing.from_string text in
  let file = Option.value file ~default:"live.vult" in
  let p = parseBuffer lexbuf in
  let stmts = snd (p file) in
  let name = Pparser.Parse.moduleName file in
  Pparser.Parse.{ file; name; stmts }

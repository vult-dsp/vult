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

(* Vult Language Server - Modular LSP implementation for Vult DSP language *)

open Util

(** Workspace management for LSP server *)
module Workspace = struct
  type t =
    { root_uri : string option
    ; include_paths : string list
    ; documents : (string, string) Hashtbl.t (* uri -> content *)
    }

  let create ?root_uri () = { root_uri; include_paths = []; documents = Hashtbl.create 16 }

  let set_include_paths workspace paths = { workspace with include_paths = paths }

  let add_document workspace uri content =
    Hashtbl.replace workspace.documents uri content;
    workspace


  let get_document workspace uri = Hashtbl.find_opt workspace.documents uri

  let remove_document workspace uri =
    Hashtbl.remove workspace.documents uri;
    workspace


  let get_all_documents workspace = Hashtbl.fold (fun uri content acc -> (uri, content) :: acc) workspace.documents []

  (** Convert file:// URI to local path *)
  let uri_to_path uri =
    if String.starts_with ~prefix:"file://" uri then
      String.sub uri 7 (String.length uri - 7)
    else
      uri


  (** Get include paths, adding workspace root if available *)
  let get_effective_include_paths workspace =
    match workspace.root_uri with
    | Some root ->
      let root_path = uri_to_path root in
      root_path :: workspace.include_paths
    | None -> workspace.include_paths
end

(** Common types and utilities *)
module Common = struct
  (** Find identifier at a specific position using the tokenizer *)
  let find_identifier_at_position (content : string) (line : int) (character : int) : string option =
    try
      let target_line = line + 1 in
      (* Lexer uses 1-based line numbers *)
      let lexbuf = Lexing.from_string content in
      let rec loop () =
        match Mparser.Lexer.token true lexbuf with
        | EOF -> None
        | token ->
          let start_pos = lexbuf.lex_start_p in
          let end_pos = lexbuf.lex_curr_p in
          let token_line = start_pos.Lexing.pos_lnum in
          let token_start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
          let token_end_char = end_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
          (* Check if the cursor position is within this token *)
          if token_line = target_line && character >= token_start_char && character < token_end_char then
            (* Extract identifier from ID tokens *)
            match token with
            | Mparser.Grammar.ID id -> Some id
            | _ -> None
          else
            loop ()
      in
      loop ()
    with
    | _ -> None


  (** Extract the word being typed at cursor position using tokenizer with fallback *)
  let get_word_at_position (content : string) (line : int) (character : int) : string =
    try
      (* First try to find a complete identifier at cursor position using tokenizer *)
      match find_identifier_at_position content line character with
      | Some id -> id
      | None ->
        (* No complete token found, fall back to string processing for partial words *)
        (* This handles cases like typing "SA" when "SAMPLE_RATE" exists *)
        let lines = String.split_on_char '\n' content in
        if line >= 0 && line < CCList.length lines then
          let line_text = CCList.nth lines line in
          let line_length = String.length line_text in
          if character >= 0 && character <= line_length then
            (* Find start of current word (go backwards from cursor) *)
            let rec find_word_start pos =
              if pos <= 0 then
                0
              else if pos >= line_length then
                line_length
              else
                let c = line_text.[pos - 1] in
                if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' then
                  find_word_start (pos - 1)
                else
                  pos
            in
            let word_start = find_word_start character in
            let word_length = character - word_start in
            if word_length > 0 then
              String.sub line_text word_start word_length
            else
              ""
          else
            ""
        else
          ""
    with
    | _ -> ""


  (** Find function call at a specific position using the tokenizer *)
  let find_function_call_at_position (content : string) (line : int) (character : int) : string option =
    try
      let target_line = line + 1 in
      (* Lexer uses 1-based line numbers *)
      let lexbuf = Lexing.from_string content in
      (* Collect all tokens with their positions *)
      let tokens = ref [] in
      let rec collect_tokens () =
        match Mparser.Lexer.token true lexbuf with
        | EOF -> CCList.rev !tokens
        | token ->
          let start_pos = lexbuf.lex_start_p in
          let end_pos = lexbuf.lex_curr_p in
          tokens := (token, start_pos, end_pos) :: !tokens;
          collect_tokens ()
      in
      let all_tokens = collect_tokens () in
      (* Find the identifier token at the cursor position *)
      let rec find_target_identifier = function
        | [] -> None
        | (token, start_pos, end_pos) :: rest ->
          let token_line = start_pos.Lexing.pos_lnum in
          let token_start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
          let token_end_char = end_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
          if token_line = target_line && character >= token_start_char && character < token_end_char then
            match token with
            | Mparser.Grammar.ID id -> Some (id, end_pos, rest)
            | _ -> None
          else
            find_target_identifier rest
      in
      match find_target_identifier all_tokens with
      | Some (identifier, id_end_pos, remaining_tokens) -> (
        (* Look for the next non-whitespace token to see if it's LPAREN *)
        let rec find_next_significant_token = function
          | [] -> None
          | (token, start_pos, _) :: rest ->
            (* Only consider tokens that come after the identifier *)
            if start_pos.Lexing.pos_cnum > id_end_pos.Lexing.pos_cnum then
              match token with
              | Mparser.Grammar.LPAREN -> Some true
              | Mparser.Grammar.ID _ | Mparser.Grammar.REAL _ | Mparser.Grammar.INT _ -> Some false
              | _ -> find_next_significant_token rest
            else
              find_next_significant_token rest
        in
        match find_next_significant_token remaining_tokens with
        | Some true -> Some identifier
        | _ -> None)
      | None -> None
    with
    | _ ->
      (* Fallback to identifier detection without function call verification *)
      find_identifier_at_position content line character
end

(** Diagnostics module *)
module Diagnostics = struct
  (** LSP Diagnostic severity constants *)
  let diagnostic_severity_error = 1

  let diagnostic_severity_warning = 2

  let diagnostic_severity_information = 3

  let diagnostic_severity_hint = 4

  (** Convert Vult Error.t to LSP Diagnostic JSON *)
  let error_to_diagnostic_json (error : Error.t) : Yojson.Safe.t =
    match error with
    | Error.PointedError (location, msg) ->
      let line = max 0 (Loc.line location - 1) in
      (* LSP uses 0-based line numbers *)
      let start_char = max 0 (Loc.startColumn location) in
      let end_char = max 0 (Loc.endColumn location) in
      `Assoc
        [ ( "range"
          , `Assoc
              [ "start", `Assoc [ "line", `Int line; "character", `Int start_char ]
              ; "end", `Assoc [ "line", `Int line; "character", `Int end_char ]
              ] )
        ; "severity", `Int diagnostic_severity_error
        ; "message", `String msg
        ]
    | Error.SimpleError msg ->
      `Assoc
        [ ( "range"
          , `Assoc
              [ "start", `Assoc [ "line", `Int 0; "character", `Int 0 ]
              ; "end", `Assoc [ "line", `Int 0; "character", `Int 0 ]
              ] )
        ; "severity", `Int diagnostic_severity_error
        ; "message", `String msg
        ]


  (** Run Vult compiler diagnostics on file content *)
  let get_diagnostics ?(includes = []) (content : string) (filename : string) : Yojson.Safe.t list =
    try
      (* Use Args.Code to pass content directly without temp files *)
      let args =
        { Args.default_arguments with
          files = [ Args.Code (filename, content) ]
        ; includes
        ; check = true
        ; use_menhir = true
        }
      in
      let result =
        try
          let parsed, _ = Driver.Loader.loadFiles args args.files in
          let _ = Core.Inference.infer args parsed in
          []
        with
        | Error.Errors errors -> CCList.map error_to_diagnostic_json errors
        | _ -> []
      in
      result
    with
    | _ -> []


  (** Enhanced diagnostics using workspace context *)
  let get_diagnostics_with_workspace (workspace : Workspace.t) (content : string) (filename : string) :
      Yojson.Safe.t list =
    let includes = Workspace.get_effective_include_paths workspace in
    let all_files =
      (* Add the current file *)
      let current_file = Args.Code (filename, content) in
      (* Add all other open documents as potential dependencies *)
      let other_files =
        Workspace.get_all_documents workspace
        |> CCList.filter_map (fun (uri, doc_content) ->
               let path = Workspace.uri_to_path uri in
               if path <> filename then
                 Some (Args.Code (path, doc_content))
               else
                 None)
      in
      current_file :: other_files
    in
    try
      let args = { Args.default_arguments with files = all_files; includes; check = true } in
      let result =
        try
          let parsed, _ = Driver.Loader.loadFiles args args.files in
          let _ = Core.Inference.infer args parsed in
          []
        with
        | Error.Errors errors ->
          (* Filter errors to only those from the target file *)
          CCList.filter_map
            (fun error ->
              match error with
              | Error.PointedError (location, _) ->
                if Loc.file location = filename then
                  Some (error_to_diagnostic_json error)
                else
                  None
              | Error.SimpleError _ ->
                (* Simple errors don't have location info, include them *)
                Some (error_to_diagnostic_json error))
            errors
        | _ -> []
      in
      result
    with
    | _ -> []
end

(** Completion module *)
module Completion = struct
  (** Basic keyword completions *)
  let vult_keywords =
    [ "fun"
    ; "val"
    ; "mem"
    ; "type"
    ; "external"
    ; "return"
    ; "if"
    ; "then"
    ; "else"
    ; "while"
    ; "iter"
    ; "match"
    ; "constant"
    ; "enum"
    ; "true"
    ; "false"
    ; "and"
    ]


  (** Completion item information *)
  type completion_item =
    { label : string
    ; kind : int (* LSP CompletionItemKind *)
    ; detail : string option
    ; documentation : string option
    }

  (** LSP Completion kinds as integers *)
  let completion_kind_function = 3

  let completion_kind_struct = 7

  let completion_kind_enum = 13

  let completion_kind_keyword = 14

  let completion_kind_enum_member = 20

  let completion_kind_constant = 21

  (** Extract completion items from parsed symbols *)
  let extract_completion_items (parsed_stmts : Pparser.Syntax.top_stmt list) : completion_item list =
    let rec extract_function_completions (func_def : Pparser.Syntax.function_def) =
      let current_completion =
        let detail =
          match func_def.t with
          | Some return_type ->
            let type_str = Pparser.Syntax.Print.type_ return_type |> Pla.print in
            Some (Printf.sprintf "fun %s(...) : %s" func_def.name type_str)
          | None -> Some (Printf.sprintf "fun %s(...)" func_def.name)
        in
        { label = func_def.name; kind = completion_kind_function; detail; documentation = None }
      in
      match func_def.next with
      | Some next_func -> current_completion :: extract_function_completions next_func
      | None -> [ current_completion ]
    in
    let extract_external_completion (ext_def : Pparser.Syntax.ext_def) =
      let detail =
        match ext_def.t with
        | Some return_type ->
          let type_str = Pparser.Syntax.Print.type_ return_type |> Pla.print in
          Some (Printf.sprintf "external %s(...) : %s" ext_def.name type_str)
        | None -> Some (Printf.sprintf "external %s(...)" ext_def.name)
      in
      { label = ext_def.name; kind = completion_kind_function; detail; documentation = Some "External function" }
    in
    let extract_type_completion name _members_list =
      { label = name
      ; kind = completion_kind_struct
      ; detail = Some ("type " ^ name)
      ; documentation = Some "User-defined type"
      }
    in
    let extract_enum_completion name members_list =
      let enum_item =
        { label = name
        ; kind = completion_kind_enum
        ; detail = Some ("enum " ^ name)
        ; documentation = Some "Enumeration type"
        }
      in
      let member_items =
        CCList.map
          (fun (member_name, _) ->
            { label = member_name
            ; kind = completion_kind_enum_member
            ; detail = Some (name ^ "::" ^ member_name)
            ; documentation = Some "Enumeration value"
            })
          members_list
      in
      enum_item :: member_items
    in
    let extract_constant_completion (dexp : Pparser.Syntax.dexp) =
      match dexp.d with
      | SDId (id, _) ->
        Some
          { label = id
          ; kind = completion_kind_constant
          ; detail = Some ("constant " ^ id)
          ; documentation = Some "Constant value"
          }
      | _ -> None (* Skip constants without simple identifiers *)
    in
    let extract_from_stmt (stmt : Pparser.Syntax.top_stmt) =
      match stmt.top with
      | STopFunction func_def -> extract_function_completions func_def
      | STopExternal (ext_def, _) -> [ extract_external_completion ext_def ]
      | STopType { name; members } -> [ extract_type_completion name members ]
      | STopEnum { name; members } -> extract_enum_completion name members
      | STopConstant (dexp, _) -> (
        match extract_constant_completion dexp with
        | Some completion -> [ completion ]
        | None -> [])
      | STopError -> []
    in
    CCList.flat_map extract_from_stmt parsed_stmts


  (** Get all completions from pre-parsed statements *)
  let get_all_completions (parsed_stmts : Pparser.Syntax.top_stmt list) : completion_item list =
    (* Keyword completions *)
    let keyword_completions =
      CCList.map
        (fun keyword ->
          { label = keyword; kind = completion_kind_keyword; detail = Some "keyword"; documentation = None })
        vult_keywords
    in
    (* Symbol-based completions *)
    let symbol_completions = extract_completion_items parsed_stmts in
    keyword_completions @ symbol_completions


  (** Get all completions - let editor handle filtering *)
  let get_completions (parsed_stmts : Pparser.Syntax.top_stmt list) : completion_item list =
    (* Return all available completions - let the editor handle prefix filtering *)
    get_all_completions parsed_stmts
end

(** Document Symbols module *)
module DocumentSymbols = struct
  (** LSP Symbol kinds as integers *)
  let symbol_kind_function = 12

  let symbol_kind_constant = 14

  let symbol_kind_enum = 10

  let symbol_kind_enum_member = 22

  let symbol_kind_struct = 23

  (** Convert Loc.t to LSP Range JSON *)
  let loc_to_range_json (loc : Loc.t) =
    let line = max 0 (Loc.line loc - 1) in
    (* LSP uses 0-based line numbers *)
    let start_char = max 0 (Loc.startColumn loc) in
    let end_char = max 0 (Loc.endColumn loc) in
    `Assoc
      [ "start", `Assoc [ "line", `Int line; "character", `Int start_char ]
      ; "end", `Assoc [ "line", `Int line; "character", `Int end_char ]
      ]


  (** Create a document symbol JSON *)
  let create_symbol_json name kind loc =
    let range_json = loc_to_range_json loc in
    `Assoc
      [ "name", `String name
      ; "kind", `Int kind
      ; "range", range_json
      ; "selectionRange", range_json (* Use same range for selection *)
      ]


  (** Extract symbols from parsed Vult AST - returns JSON objects *)
  let extract_symbols_from_ast (parsed_stmts : Pparser.Syntax.top_stmt list) =
    let rec extract_function_symbols (func_def : Pparser.Syntax.function_def) =
      let current_symbol = create_symbol_json func_def.name symbol_kind_function func_def.loc in
      match func_def.next with
      | Some next_func -> current_symbol :: extract_function_symbols next_func
      | None -> [ current_symbol ]
    in
    let extract_external_symbol (ext_def : Pparser.Syntax.ext_def) =
      create_symbol_json ext_def.name symbol_kind_function ext_def.loc
    in
    let extract_type_symbol name _members_list loc = create_symbol_json name symbol_kind_struct loc in
    let extract_enum_symbol name members_list loc =
      (* Create symbol for the enum type itself *)
      let enum_type_symbol = create_symbol_json name symbol_kind_enum loc in
      (* Create symbols for each enum member *)
      let member_symbols =
        CCList.map
          (fun (member_name, member_loc) -> create_symbol_json member_name symbol_kind_enum_member member_loc)
          members_list
      in
      enum_type_symbol :: member_symbols
    in
    let extract_constant_symbol (dexp : Pparser.Syntax.dexp) loc =
      match dexp.d with
      | SDId (id, _) -> [ create_symbol_json id symbol_kind_constant loc ]
      | _ -> [] (* Skip constants without simple identifiers *)
    in
    let extract_from_stmt (stmt : Pparser.Syntax.top_stmt) =
      match stmt.top with
      | STopFunction func_def -> extract_function_symbols func_def
      | STopExternal (ext_def, _) -> [ extract_external_symbol ext_def ]
      | STopType { name; members } -> [ extract_type_symbol name members stmt.loc ]
      | STopEnum { name; members } -> extract_enum_symbol name members stmt.loc
      | STopConstant (dexp, _) -> extract_constant_symbol dexp stmt.loc
      | STopError -> []
    in
    CCList.flat_map extract_from_stmt parsed_stmts


  (** Get document symbols from pre-parsed statements *)
  let get_document_symbols (parsed_stmts : Pparser.Syntax.top_stmt list) = extract_symbols_from_ast parsed_stmts
end

(** Go-to-Definition module *)
module GoToDefinition = struct
  (** Definition information for go-to-definition *)
  type definition =
    { name : string
    ; location : Loc.t
    }

  (** Extract all definitions (functions, enum values, and constants) from parsed AST in a single pass *)
  let extract_all_definitions (parsed_stmts : Pparser.Syntax.top_stmt list) : definition list =
    let rec extract_function_defs (func_def : Pparser.Syntax.function_def) : definition list =
      let current_def = { name = func_def.name; location = func_def.loc } in
      match func_def.next with
      | Some next_func -> current_def :: extract_function_defs next_func
      | None -> [ current_def ]
    in
    let extract_from_stmt (stmt : Pparser.Syntax.top_stmt) : definition list =
      match stmt.top with
      | STopFunction func_def -> extract_function_defs func_def
      | STopExternal (ext_def, _) -> [ { name = ext_def.name; location = ext_def.loc } ]
      | STopEnum { name = _; members } ->
        CCList.map (fun (member_name, member_loc) -> { name = member_name; location = member_loc }) members
      | STopConstant (dexp, _) -> (
        (* Extract the constant name from the dexp *)
        match dexp.d with
        | SDId (id, _) -> [ { name = id; location = stmt.loc } ]
        | _ -> [] (* Skip constants without simple identifiers *))
      | STopType { name; _ } -> [ { name; location = stmt.loc } ]
      | STopError -> []
    in
    CCList.flat_map extract_from_stmt parsed_stmts


  (** Find definition by name from a list of definitions *)
  let find_definition_by_name (name : string) (definitions : definition list) : Loc.t option =
    try
      let def = CCList.find (fun def -> def.name = name) definitions in
      Some def.location
    with
    | Not_found -> None


  (** Get definition location from workspace using cached parsing - supports both functions and enum values *)
  let get_definition_location_workspace_cached
      (get_parsed_statements_fn : string -> string -> string -> Pparser.Syntax.top_stmt list) (workspace : Workspace.t)
      (content : string) (line : int) (character : int) : Loc.t option =
    try
      (* First try to find a function call at the cursor position *)
      let identifier =
        match Common.find_function_call_at_position content line character with
        | Some name -> Some name
        | None -> Common.find_identifier_at_position content line character
      in
      match identifier with
      | Some name ->
        (* Search through all documents in the workspace using cached parsing *)
        let all_documents = Workspace.get_all_documents workspace in
        let rec search_files = function
          | [] -> None
          | (uri, doc_content) :: rest -> (
            let filename = Workspace.uri_to_path uri in
            try
              let parsed_stmts = get_parsed_statements_fn uri doc_content filename in
              let all_definitions = extract_all_definitions parsed_stmts in
              match find_definition_by_name name all_definitions with
              | Some location -> Some location
              | None -> search_files rest
            with
            | _ -> search_files rest)
        in
        search_files all_documents
      | None -> None
    with
    | _ -> None
end

(** Semantic Tokens module *)
module SemanticTokens = struct
  (** Basic syntax highlighting using Vult lexer *)
  type vult_token_type =
    | VKeyword
    | VNumber
    | VString
    | VComment
    | VIdentifier
    | VFunction
    | VOperator
    | VPunctuation
    | VType

  (** Convert token type to LSP semantic token type number *)
  let token_type_to_lsp_number = function
    | VIdentifier -> 0 (* variable *)
    | VComment -> 1 (* comment *)
    | VKeyword -> 2 (* keyword *)
    | VType -> 3 (* type *)
    | VNumber -> 4 (* number *)
    | VString -> 5 (* string *)
    | VOperator -> 6 (* operator *)
    | VPunctuation -> 7 (* punctuation *)
    | VFunction -> 8 (* function *)


  (** Classify tokens based on their content *)
  let classify_token _token_text t =
    let open Mparser.Grammar in
    match t with
    | BLOCK_COMMENT _ -> VComment
    | LINE_COMMENT _ -> VComment
    | TYPE
     |FUN
     |VAL
     |MEM
     |EXTERNAL
     |RETURN
     |IF
     |THEN
     |ELSE
     |WHILE
     |ITER
     |MATCH
     |CONSTANT
     |ENUM
     |TRUE
     |FALSE
     |AND -> VKeyword
    | INT _ | REAL _ | FIXED _ | XINT _ -> VNumber
    | STRING _ -> VString
    | ID _s -> VIdentifier
    | OP_LEVEL_0 _ | OP_LEVEL_1 _ | OP_LEVEL_2 _ | OP_LEVEL_3 _ | MINUS | LAND | LOR -> VOperator
    | LPAREN
     |RPAREN
     |LBRACE
     |RBRACE
     |LBRACKET
     |RBRACKET
     |TAG
     |ARROW
     |COLON
     |SEMICOLON
     |COMMA
     |DOT
     |ASSIGN
     |WILDCARD
     |EOF -> VPunctuation


  (** Check if an identifier is followed by '(' indicating a function call *)
  let is_function_call (content : string) (line : int) (start_char : int) (length : int) : bool =
    try
      let lines = String.split_on_char '\n' content in
      if line >= 0 && line < CCList.length lines then
        let line_text = CCList.nth lines line in
        let end_pos = start_char + length in
        let rec skip_whitespace pos =
          if pos < String.length line_text && (line_text.[pos] = ' ' || line_text.[pos] = '\t') then
            skip_whitespace (pos + 1)
          else
            pos
        in
        let pos_after_whitespace = skip_whitespace end_pos in
        pos_after_whitespace < String.length line_text && line_text.[pos_after_whitespace] = '('
      else
        false
    with
    | _ -> false


  (** Split a multi-line token into per-line tokens *)
  let split_multiline_token (content : string) (token_type : vult_token_type) (start_line : int) (start_char : int)
      (length : int) : (vult_token_type * int * int * int) list =
    let lines = String.split_on_char '\n' content in
    try
      (* Calculate which lines this token spans *)
      let start_line_text = CCList.nth lines start_line in
      let chars_on_first_line = String.length start_line_text - start_char in
      if length <= chars_on_first_line then
        (* Single line token *)
        [ token_type, start_line, start_char, length ]
      else
        (* Multi-line token - need to split *)
        let result = ref [] in
        let current_line = ref start_line in
        let remaining_chars = ref length in
        (* Use iterative loop instead of recursive to avoid stack overflow *)
        while !remaining_chars > 0 && !current_line < CCList.length lines do
          let line_text = CCList.nth lines !current_line in
          let line_start =
            if !current_line = start_line then
              start_char
            else
              0
          in
          let available_chars = String.length line_text - line_start in
          let chars_to_take = min !remaining_chars available_chars in
          if chars_to_take > 0 then (
            let new_token = token_type, !current_line, line_start, chars_to_take in
            result := new_token :: !result;
            (* Account for the newline character when moving to next line *)
            let chars_consumed =
              chars_to_take
              +
              if !current_line < CCList.length lines - 1 then
                1
              else
                0
            in
            current_line := !current_line + 1;
            remaining_chars := !remaining_chars - chars_consumed)
          else
            (* Skip empty lines *)
            current_line := !current_line + 1;
          remaining_chars := !remaining_chars - 1
        done;
        CCList.rev !result
    with
    | _ -> [ token_type, start_line, start_char, length ]


  (* Fallback to single token *)

  (** Tokenize Vult code for syntax highlighting *)
  let tokenize_vult_code (content : string) : (vult_token_type * int * int * int) list =
    try
      let lexbuf = Lexing.from_string content in
      let tokens = ref [] in
      (* Use iterative loop instead of recursive to avoid stack overflow in JavaScript *)
      let continue = ref true in
      while !continue do
        match Mparser.Lexer.token true lexbuf with
        | EOF -> continue := false
        | token ->
          let start_pos = lexbuf.lex_start_p in
          let end_pos = lexbuf.lex_curr_p in
          let line = start_pos.Lexing.pos_lnum - 1 in
          (* LSP uses 0-based line numbers *)
          let start_char = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol in
          let length = end_pos.Lexing.pos_cnum - start_pos.Lexing.pos_cnum in
          let token_text = Lexing.lexeme lexbuf in
          let token_type = classify_token token_text token in
          (* Enhance classification for function calls *)
          let final_token_type =
            match token_type with
            | VIdentifier when is_function_call content line start_char length -> VFunction
            | _ -> token_type
          in
          (* Handle multi-line tokens (block comments) *)
          let line_tokens =
            match token with
            | Mparser.Grammar.BLOCK_COMMENT _ ->
              (* Extract the actual comment content from the source file *)
              let actual_token_content =
                try
                  let lines = String.split_on_char '\n' content in
                  let start_line_text = CCList.nth lines line in
                  let remaining_on_first_line = String.length start_line_text - start_char in
                  if length <= remaining_on_first_line then
                    (* Single line - extract from current line *)
                    String.sub start_line_text start_char length
                  else
                    (* Multi-line - need to extract across lines *)
                    let buffer = Buffer.create length in
                    let current_line = ref line in
                    let remaining_chars = ref length in
                    (* Use iterative loop instead of recursive to avoid stack overflow *)
                    while !remaining_chars > 0 && !current_line < CCList.length lines do
                      let line_text = CCList.nth lines !current_line in
                      let line_start =
                        if !current_line = line then
                          start_char
                        else
                          0
                      in
                      let available_chars = String.length line_text - line_start in
                      let chars_to_take = min !remaining_chars available_chars in
                      if chars_to_take > 0 then
                        Buffer.add_substring buffer line_text line_start chars_to_take;
                      if !current_line < CCList.length lines - 1 && !remaining_chars > chars_to_take then
                        Buffer.add_char buffer '\n';
                      current_line := !current_line + 1;
                      remaining_chars := !remaining_chars - chars_to_take - 1
                    done;
                    Buffer.contents buffer
                with
                | _ -> token_text
              in
              if String.contains actual_token_content '\n' then
                split_multiline_token content final_token_type line start_char length
              else
                [ final_token_type, line, start_char, length ]
            | _ -> [ final_token_type, line, start_char, length ]
          in
          tokens := CCList.append (CCList.rev line_tokens) !tokens
      done;
      CCList.rev !tokens
    with
    | _ -> [] (* Return empty list on lexer errors *)


  (** Generate LSP semantic tokens from Vult code *)
  let get_semantic_tokens (content : string) : int list =
    let tokens = tokenize_vult_code content in
    (* Use iterative encoding instead of recursive to avoid stack overflow in JavaScript *)
    let result = ref [] in
    let prev_line = ref 0 in
    let prev_char = ref 0 in
    CCList.iter
      (fun (token_type, line, char, length) ->
        let delta_line = line - !prev_line in
        let delta_char =
          if delta_line = 0 then
            char - !prev_char
          else
            char
        in
        let token_type_num = token_type_to_lsp_number token_type in
        let token_modifiers = 0 in
        (* No modifiers for now *)
        (* LSP format: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers] *)
        result := token_modifiers :: token_type_num :: length :: delta_char :: delta_line :: !result;
        prev_line := line;
        prev_char := char)
      tokens;
    CCList.rev !result
end

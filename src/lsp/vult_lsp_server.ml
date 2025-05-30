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

(* Vult Language Server - Direct JSON/LSP implementation *)
(* Handles LSP protocol directly with yojson, no complex GADT dependencies *)

open Yojson.Safe.Util
open Util

(** Parsed file cache entry *)
type parsed_cache_entry =
  { content_hash : string
  ; parsed_files : Pparser.Parse.parsed_file list
  ; parse_time : float
  }

(** Enhanced server state with workspace support and parse caching *)
module State = struct
  type t =
    { workspace : Vult_lsp.Workspace.t
    ; initialized : bool
    ; parsed_cache : (string, parsed_cache_entry) Hashtbl.t (* uri -> cached parse result *)
    }

  let create ?root_uri () =
    { workspace = Vult_lsp.Workspace.create ?root_uri (); initialized = false; parsed_cache = Hashtbl.create 16 }


  let set_document state uri content =
    let new_workspace = Vult_lsp.Workspace.add_document state.workspace uri content in
    (* Don't clear cache here - let get_parsed_files handle cache update safely *)
    { state with workspace = new_workspace }


  let get_document state uri = Vult_lsp.Workspace.get_document state.workspace uri

  let remove_document state uri =
    let new_workspace = Vult_lsp.Workspace.remove_document state.workspace uri in
    (* Remove cache for this URI since document is closed *)
    Hashtbl.remove state.parsed_cache uri;
    { state with workspace = new_workspace }


  let set_initialized state = { state with initialized = true }

  let set_include_paths state paths =
    let new_workspace = Vult_lsp.Workspace.set_include_paths state.workspace paths in
    (* Clear all caches since include paths affect parsing *)
    Hashtbl.clear state.parsed_cache;
    { state with workspace = new_workspace }


  (** Get cached parsed files or parse if needed *)
  let get_parsed_files state uri content filename =
    let content_hash = Digest.string content in
    let cache_key = uri in
    match Hashtbl.find_opt state.parsed_cache cache_key with
    | Some cached when cached.content_hash = content_hash ->
      Printf.eprintf "   📋 Using cached parse result for %s\n" filename;
      flush stderr;
      cached.parsed_files
    | Some cached -> (
      Printf.eprintf "   🔄 Parsing %s (content changed)\n" filename;
      flush stderr;
      (* Content changed - try to parse new content *)
      try
        let includes = Vult_lsp.Workspace.get_effective_include_paths state.workspace in
        let args = { Args.default_arguments with files = [ Args.Code (filename, content) ]; includes; check = false } in
        let parsed_files, _ = Driver.Loader.loadFiles args args.files in
        (* Parsing succeeded - update cache with new result *)
        let cache_entry =
          { content_hash
          ; parsed_files
          ; parse_time =
              (try Unix.time () with
              | _ -> 0.0)
          }
        in
        Hashtbl.replace state.parsed_cache cache_key cache_entry;
        Printf.eprintf "   ✅ Parse successful, cache updated for %s\n" filename;
        flush stderr;
        parsed_files
      with
      | exn ->
        Printf.eprintf
          "   ❌ Parse error for %s: %s - keeping previous cached result\n"
          filename
          (Printexc.to_string exn);
        flush stderr;
        (* Parsing failed - keep using the old cached result *)
        cached.parsed_files)
    | None -> (
      Printf.eprintf "   🔄 Parsing %s (cache miss)\n" filename;
      flush stderr;
      (* No cache entry - try to parse *)
      try
        let includes = Vult_lsp.Workspace.get_effective_include_paths state.workspace in
        let args = { Args.default_arguments with files = [ Args.Code (filename, content) ]; includes; check = false } in
        let parsed_files, _ = Driver.Loader.loadFiles args args.files in
        (* Parsing succeeded - create cache entry *)
        let cache_entry =
          { content_hash
          ; parsed_files
          ; parse_time =
              (try Unix.time () with
              | _ -> 0.0)
          }
        in
        Hashtbl.replace state.parsed_cache cache_key cache_entry;
        Printf.eprintf "   ✅ Parse successful, cache created for %s\n" filename;
        flush stderr;
        parsed_files
      with
      | exn ->
        Printf.eprintf "   ❌ Initial parse error for %s: %s\n" filename (Printexc.to_string exn);
        flush stderr;
        (* No previous cache and parsing failed - return empty *)
        [])


  (** Get all statements from cached parse result *)
  let get_parsed_statements state uri content filename =
    let parsed_files = get_parsed_files state uri content filename in
    List.flatten @@ List.map (fun (p : Pparser.Parse.parsed_file) -> p.stmts) parsed_files
end

(** LSP Protocol Constants *)
module Protocol = struct
  (** Server capabilities JSON *)
  let server_capabilities_json =
    `Assoc
      [ "textDocumentSync", `Int 1
      ; (* Full synchronization *)
        "completionProvider", `Assoc [ "triggerCharacters", `List [ `String "." ] ]
      ; "hoverProvider", `Bool true
      ; ( "semanticTokensProvider"
        , `Assoc
            [ ( "legend"
              , `Assoc
                  [ ( "tokenTypes"
                    , `List
                        [ `String "variable"
                        ; (* 0 - VIdentifier *)
                          `String "comment"
                        ; (* 1 - VComment *)
                          `String "keyword"
                        ; (* 2 - VKeyword *)
                          `String "type"
                        ; (* 3 - VType *)
                          `String "number"
                        ; (* 4 - VNumber *)
                          `String "string"
                        ; (* 5 - VString *)
                          `String "operator"
                        ; (* 6 - VOperator *)
                          `String "punctuation"
                        ; (* 7 - VPunctuation *)
                          `String "function" (* 8 - VFunction *)
                        ] )
                  ; "tokenModifiers", `List []
                  ] )
            ; "full", `Bool true
            ] )
      ; "documentSymbolProvider", `Bool true
      ; "definitionProvider", `Bool true
      ]
end

(** Request Handlers Module *)
module RequestHandlers = struct
  (** Handle initialize request *)
  let handle_initialize id params =
    (* Extract workspace root from initialize params *)
    let root_uri =
      try Some (params |> member "rootUri" |> to_string) with
      | _ -> (
        try Some (params |> member "workspaceFolders" |> to_list |> List.hd |> member "uri" |> to_string) with
        | _ -> None)
    in
    Printf.eprintf "✅ Initialize request handled\n";
    (match root_uri with
    | Some uri -> Printf.eprintf "   Workspace root: %s\n" uri
    | None -> Printf.eprintf "   No workspace root specified\n");
    flush stderr;
    `Assoc
      [ "jsonrpc", `String "2.0"; "id", id; "result", `Assoc [ "capabilities", Protocol.server_capabilities_json ] ]


  (** Handle completion request *)
  let handle_completion state id params =
    Printf.eprintf "✅ Completion request handled\n";
    flush stderr;
    let text_document = params |> member "textDocument" in
    let uri = text_document |> member "uri" |> to_string in
    let position = params |> member "position" in
    let line = position |> member "line" |> to_int in
    let character = position |> member "character" |> to_int in
    let filename = Vult_lsp.Workspace.uri_to_path uri in
    (* Get content from document store *)
    let content =
      match State.get_document state uri with
      | Some doc_content -> doc_content
      | None ->
        Printf.eprintf "   Warning: Document not found in store for completion\n";
        flush stderr;
        ""
    in
    (* Get intelligent completions based on symbols in the workspace using cached parsing *)
    let parsed_stmts = State.get_parsed_statements state uri content filename in
    let completions = Vult_lsp.Completion.get_completions parsed_stmts in
    (* Extract prefix for debugging *)
    let prefix = Vult_lsp.Common.get_word_at_position content line character in
    Printf.eprintf "   Generated %d completions (editor will filter by prefix '%s')\n" (List.length completions) prefix;
    flush stderr;
    (* Convert completion_item list to LSP JSON format *)
    let completion_items =
      List.map
        (fun (completion : Vult_lsp.Completion.completion_item) ->
          let detail_str =
            match completion.detail with
            | Some d -> d
            | None -> ""
          in
          let doc_str =
            match completion.documentation with
            | Some d -> d
            | None -> ""
          in
          `Assoc
            [ "label", `String completion.label
            ; "kind", `Int completion.kind
            ; "detail", `String detail_str
            ; "documentation", `String doc_str
            ])
        completions
    in
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", id
      ; "result", `Assoc [ "isIncomplete", `Bool false; "items", `List completion_items ]
      ]


  (** Handle hover request *)
  let handle_hover id params =
    Printf.eprintf "✅ Hover request handled\n";
    flush stderr;
    let position = params |> member "position" in
    let line = position |> member "line" |> to_int in
    let character = position |> member "character" |> to_int in
    (* Simple hover info for now *)
    let hover_text = Printf.sprintf "Vult position: line %d, character %d" line character in
    `Assoc
      [ "jsonrpc", `String "2.0"
      ; "id", id
      ; "result", `Assoc [ "contents", `Assoc [ "kind", `String "markdown"; "value", `String hover_text ] ]
      ]


  (** Handle shutdown request *)
  let handle_shutdown id =
    Printf.eprintf "✅ Shutdown request handled\n";
    flush stderr;
    `Assoc [ "jsonrpc", `String "2.0"; "id", id; "result", `Null ]


  (** Handle semantic tokens request *)
  let handle_semantic_tokens state id params =
    Printf.eprintf "✅ Semantic tokens request handled\n";
    flush stderr;
    let text_document = params |> member "textDocument" in
    let uri = text_document |> member "uri" |> to_string in
    (* Get content from document store, or use fallback *)
    let content =
      match State.get_document state uri with
      | Some doc_content -> doc_content
      | None ->
        Printf.eprintf "   Warning: Document not found in store, using fallback\n";
        flush stderr;
        "fun test(x: real) : real {\n  return sin(x * 2.0);\n}"
    in
    (* Get semantic tokens from Vult lexer *)
    let tokens = Vult_lsp.SemanticTokens.get_semantic_tokens content in
    Printf.eprintf "   Generated %d semantic token values\n" (List.length tokens);
    flush stderr;
    `Assoc
      [ "jsonrpc", `String "2.0"; "id", id; "result", `Assoc [ "data", `List (CCList.map (fun i -> `Int i) tokens) ] ]


  (** Handle textDocument/documentSymbol request *)
  let handle_document_symbol state id params =
    Printf.eprintf "📋 Document symbol request handled\n";
    flush stderr;
    let text_document = params |> member "textDocument" in
    let uri = text_document |> member "uri" |> to_string in
    let filename = Vult_lsp.Workspace.uri_to_path uri in
    (* Get content from document store *)
    let content =
      match State.get_document state uri with
      | Some doc_content -> doc_content
      | None ->
        Printf.eprintf "   Warning: Document not found in store for symbols\n";
        flush stderr;
        ""
    in
    (* Extract symbols from parsed AST using cached parsing *)
    let parsed_stmts = State.get_parsed_statements state uri content filename in
    let symbols = Vult_lsp.DocumentSymbols.get_document_symbols parsed_stmts in
    Printf.eprintf "   Found %d symbols\n" (List.length symbols);
    flush stderr;
    (* Symbols are already JSON objects *)
    `Assoc [ "jsonrpc", `String "2.0"; "id", id; "result", `List symbols ]


  (** Handle textDocument/definition request *)
  let handle_definition state id params =
    Printf.eprintf "🎯 Definition request handled\n";
    flush stderr;
    let text_document = params |> member "textDocument" in
    let uri = text_document |> member "uri" |> to_string in
    let position = params |> member "position" in
    let line = position |> member "line" |> to_int in
    let character = position |> member "character" |> to_int in
    let _filename = Vult_lsp.Workspace.uri_to_path uri in
    (* Get content from document store *)
    let content =
      match State.get_document state uri with
      | Some doc_content -> doc_content
      | None ->
        Printf.eprintf "   Warning: Document not found in store for definition\n";
        flush stderr;
        ""
    in
    (* Find function definition location using workspace-wide search with caching *)
    match
      Vult_lsp.GoToDefinition.get_definition_location_workspace_cached
        (State.get_parsed_statements state)
        state.workspace
        content
        line
        character
    with
    | Some location ->
      let def_line = max 0 (Loc.line location - 1) in
      (* LSP uses 0-based line numbers *)
      let def_char = max 0 (Loc.startColumn location) in
      let def_filename = Loc.file location in
      let def_uri =
        if String.starts_with ~prefix:"/" def_filename then
          "file://" ^ def_filename
        else
          "file://"
          ^
          try Filename.concat (Sys.getcwd ()) def_filename with
          | _ -> def_filename
      in
      Printf.eprintf "   Found definition at %s line %d, character %d\n" def_filename def_line def_char;
      flush stderr;
      let location_json =
        `Assoc
          [ "uri", `String def_uri
          ; ( "range"
            , `Assoc
                [ "start", `Assoc [ "line", `Int def_line; "character", `Int def_char ]
                ; "end", `Assoc [ "line", `Int def_line; "character", `Int (max 0 (Loc.endColumn location)) ]
                ] )
          ]
      in
      `Assoc [ "jsonrpc", `String "2.0"; "id", id; "result", location_json ]
    | None ->
      Printf.eprintf "   No definition found\n";
      flush stderr;
      `Assoc [ "jsonrpc", `String "2.0"; "id", id; "result", `Null ]
end

(** Notification Handlers Module *)
module NotificationHandlers = struct
  (** Handle textDocument/didOpen notification *)
  let handle_did_open state params =
    let text_document = params |> member "textDocument" in
    let uri = text_document |> member "uri" |> to_string in
    let content = text_document |> member "text" |> to_string in
    let new_state = State.set_document state uri content in
    Printf.eprintf "✅ Document opened: %s\n" uri;
    flush stderr;
    (* Generate diagnostics using workspace context *)
    let filename = Vult_lsp.Workspace.uri_to_path uri in
    let diagnostics = Vult_lsp.Diagnostics.get_diagnostics_with_workspace state.workspace content filename in
    Printf.eprintf "   Generated %d diagnostics\n" (List.length diagnostics);
    flush stderr;
    (* Diagnostics are already in JSON format *)
    let diagnostics_json = diagnostics in
    let notification =
      `Assoc
        [ "jsonrpc", `String "2.0"
        ; "method", `String "textDocument/publishDiagnostics"
        ; "params", `Assoc [ "uri", `String uri; "diagnostics", `List diagnostics_json ]
        ]
    in
    new_state, [ notification ]


  (** Handle textDocument/didChange notification *)
  let handle_did_change state params =
    let text_document = params |> member "textDocument" in
    let uri = text_document |> member "uri" |> to_string in
    let content_changes = params |> member "contentChanges" |> to_list in
    match content_changes with
    | [] -> state, []
    | change :: _ ->
      let content = change |> member "text" |> to_string in
      let new_state = State.set_document state uri content in
      Printf.eprintf "✅ Document changed: %s\n" uri;
      flush stderr;
      (* Generate updated diagnostics using workspace context *)
      let filename = Vult_lsp.Workspace.uri_to_path uri in
      let diagnostics = Vult_lsp.Diagnostics.get_diagnostics_with_workspace new_state.workspace content filename in
      Printf.eprintf "   Generated %d diagnostics\n" (List.length diagnostics);
      flush stderr;
      (* Diagnostics are already in JSON format *)
      let diagnostics_json = diagnostics in
      let notification =
        `Assoc
          [ "jsonrpc", `String "2.0"
          ; "method", `String "textDocument/publishDiagnostics"
          ; "params", `Assoc [ "uri", `String uri; "diagnostics", `List diagnostics_json ]
          ]
      in
      new_state, [ notification ]


  (** Handle textDocument/didClose notification *)
  let handle_did_close state params =
    let text_document = params |> member "textDocument" in
    let uri = text_document |> member "uri" |> to_string in
    let new_state = State.remove_document state uri in
    Printf.eprintf "✅ Document closed: %s\n" uri;
    flush stderr;
    new_state, []
end

(** LSP Communication Module *)
module Communication = struct
  (** Read LSP header and content *)
  let read_lsp_message ic =
    let rec read_headers acc =
      let line = input_line ic in
      let line = String.trim line in
      if line = "" then
        acc
      else
        match String.split_on_char ':' line with
        | [ key; value ] ->
          let key = String.trim key in
          let value = String.trim value in
          read_headers ((key, value) :: acc)
        | _ -> read_headers acc
    in
    let headers = read_headers [] in
    let content_length =
      match List.assoc_opt "Content-Length" headers with
      | Some len -> int_of_string len
      | None -> failwith "Missing Content-Length header"
    in
    let buffer = Bytes.create content_length in
    really_input ic buffer 0 content_length;
    Bytes.to_string buffer


  (** Write LSP response *)
  let write_lsp_response oc json =
    let content = Yojson.Safe.to_string json in
    let content_length = String.length content in
    Printf.fprintf oc "Content-Length: %d\r\n\r\n%s" content_length content;
    flush oc
end

(** Main server loop *)
let run () =
  let ic = stdin in
  let oc = stdout in
  Printf.eprintf "🚀 Vult Language Server starting...\n";
  Printf.eprintf "📡 Using direct JSON/LSP implementation\n";
  Printf.eprintf "⚡ Diagnostics engine ready\n";
  flush stderr;
  let state = ref (State.create ()) in
  let rec loop () =
    try
      (* Read LSP message *)
      let message_content = Communication.read_lsp_message ic in
      Printf.eprintf "📥 Received message: %s\n" (String.sub message_content 0 (min 100 (String.length message_content)));
      flush stderr;
      (* Parse JSON *)
      let json = Yojson.Safe.from_string message_content in
      (* Check if it's a request or notification *)
      (if json |> member "id" <> `Null then (
         (* It's a request *)
         let id = json |> member "id" in
         let method_name = json |> member "method" |> to_string in
         let params = json |> member "params" in
         Printf.eprintf "📥 Request: %s\n" method_name;
         flush stderr;
         let response =
           match method_name with
           | "initialize" -> RequestHandlers.handle_initialize id params
           | "textDocument/completion" -> RequestHandlers.handle_completion !state id params
           | "textDocument/hover" -> RequestHandlers.handle_hover id params
           | "textDocument/semanticTokens/full" -> RequestHandlers.handle_semantic_tokens !state id params
           | "textDocument/documentSymbol" -> RequestHandlers.handle_document_symbol !state id params
           | "textDocument/definition" -> RequestHandlers.handle_definition !state id params
           | "shutdown" -> RequestHandlers.handle_shutdown id
           | _ ->
             Printf.eprintf "❓ Unhandled request: %s\n" method_name;
             flush stderr;
             `Assoc
               [ "jsonrpc", `String "2.0"
               ; "id", id
               ; ( "error"
                 , `Assoc
                     [ "code", `Int (-32601)
                     ; (* Method not found *)
                       "message", `String ("Method not found: " ^ method_name)
                     ] )
               ]
         in
         Communication.write_lsp_response oc response;
         (* Handle state updates after initialize request *)
         if method_name = "initialize" then
           let root_uri =
             try Some (params |> member "rootUri" |> to_string) with
             | _ -> (
               try Some (params |> member "workspaceFolders" |> to_list |> List.hd |> member "uri" |> to_string) with
               | _ -> None)
           in
           state := State.create ?root_uri () |> State.set_initialized)
       else
         (* It's a notification *)
         let method_name = json |> member "method" |> to_string in
         let params = json |> member "params" in
         Printf.eprintf "📥 Notification: %s\n" method_name;
         flush stderr;
         let new_state, notifications =
           match method_name with
           | "textDocument/didOpen" -> NotificationHandlers.handle_did_open !state params
           | "textDocument/didChange" -> NotificationHandlers.handle_did_change !state params
           | "textDocument/didClose" -> NotificationHandlers.handle_did_close !state params
           | "exit" ->
             Printf.eprintf "👋 Exit notification received\n";
             flush stderr;
             exit 0
           | _ ->
             Printf.eprintf "❓ Unhandled notification: %s\n" method_name;
             flush stderr;
             !state, []
         in
         state := new_state;
         (* Send any resulting notifications *)
         List.iter (Communication.write_lsp_response oc) notifications);
      (* Continue loop *)
      loop ()
    with
    | End_of_file ->
      Printf.eprintf "👋 Client disconnected\n";
      flush stderr;
      exit 0
    | exn ->
      Printf.eprintf "❌ Message loop error: %s\n" (Printexc.to_string exn);
      flush stderr;
      loop ()
  in
  loop ()

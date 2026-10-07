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

(* Vult Language Server - JSON-RPC transport and dispatch of the LSP messages.

   The server state is immutable: every handler returns the new state together with its reply.
   Diagnostics are published for the changed document and for every open document importing it.

   Diagnostic messages are written to stderr only when VULT_LSP_VERBOSE is set; errors always are. *)

open Yojson.Safe.Util
module L = Vult_lsp
module Map = Util.Maps.Map

let verbose = Option.is_some (Sys.getenv_opt "VULT_LSP_VERBOSE")

let log fmt = Printf.ksprintf (fun msg -> if verbose then prerr_endline msg) fmt

let log_error fmt = Printf.ksprintf prerr_endline fmt

(** Lifecycle of the server as defined by the protocol *)
type phase = AwaitingInitialize | Running | ShutdownRequested

module State = struct
  type t =
    { phase: phase
    ; workspace: L.Workspace.t
    ; analyses: L.Analysis.t Map.t (* path of an open document -> its last analysis *) }

  let empty : t = {phase= AwaitingInitialize; workspace= L.Workspace.create (); analyses= Map.empty}

  (** Analysis of an open document, redone when any of the files it read has changed *)
  let analysis (state : t) (path : string) : t * L.Analysis.t option =
    match (L.Workspace.find_document state.workspace path, Map.find_opt path state.analyses) with
    | None, _ ->
        (state, None)
    | Some _, Some analysis when L.Analysis.is_current state.workspace analysis ->
        (state, Some analysis)
    | Some _, previous ->
        let analysis = L.Analysis.analyze ?previous state.workspace path in
        ({state with analyses= Map.add path analysis state.analyses}, Some analysis)

  (** Open documents affected by a change to [path]: the document itself and those that read it *)
  let affected_documents (state : t) (path : string) : string list =
    let is_affected document =
      String.equal document path
      ||
      match Map.find_opt document state.analyses with
      | Some analysis ->
          L.Analysis.depends_on state.workspace analysis path
      | None ->
          false
    in
    CCList.filter is_affected (L.Workspace.open_paths state.workspace)
end

(** JSON-RPC messages *)
module Message = struct
  type t =
    | Request of {id: Yojson.Safe.t; method_: string; params: Yojson.Safe.t}
    | Notification of {method_: string; params: Yojson.Safe.t}
    | Response (* a reply to a request from the server; the server sends none, so they are ignored *)

  let classify (json : Yojson.Safe.t) : t option =
    match json with
    | `Assoc _ -> (
        let params = member "params" json in
        match (member "id" json, member "method" json) with
        | `Null, `String method_ ->
            Some (Notification {method_; params})
        | id, `String method_ ->
            Some (Request {id; method_; params})
        | `Null, _ ->
            None
        | _, _ ->
            Some Response )
    | _ ->
        None

  type error_code =
    | ParseError
    | InvalidRequest
    | MethodNotFound
    | InvalidParams
    | InternalError
    | ServerNotInitialized

  let error_code_to_int (code : error_code) : int =
    match code with
    | ParseError ->
        -32700
    | InvalidRequest ->
        -32600
    | MethodNotFound ->
        -32601
    | InvalidParams ->
        -32602
    | InternalError ->
        -32603
    | ServerNotInitialized ->
        -32002

  let response (id : Yojson.Safe.t) (result : Yojson.Safe.t) : Yojson.Safe.t =
    `Assoc [("jsonrpc", `String "2.0"); ("id", id); ("result", result)]

  let error (id : Yojson.Safe.t) (code : error_code) (message : string) : Yojson.Safe.t =
    `Assoc
      [ ("jsonrpc", `String "2.0")
      ; ("id", id)
      ; ("error", `Assoc [("code", `Int (error_code_to_int code)); ("message", `String message)]) ]

  let notification (method_ : string) (params : Yojson.Safe.t) : Yojson.Safe.t =
    `Assoc [("jsonrpc", `String "2.0"); ("method", `String method_); ("params", params)]
end

(** Server capabilities JSON *)
let server_capabilities_json : Yojson.Safe.t =
  `Assoc
    [ ("textDocumentSync", `Int 1 (* full document on every change *))
    ; ("completionProvider", `Assoc [("triggerCharacters", `List [`String "."])])
    ; ("hoverProvider", `Bool true)
    ; ( "semanticTokensProvider"
      , `Assoc
          [ ( "legend"
            , `Assoc
                [ ("tokenTypes", `List (CCList.map (fun name -> `String name) L.SemanticTokens.legend))
                ; ("tokenModifiers", `List []) ] )
          ; ("full", `Bool true) ] )
    ; ("documentSymbolProvider", `Bool true)
    ; ("definitionProvider", `Bool true) ]

(** Reading the parameters of requests and notifications *)
module Params = struct
  let uri (params : Yojson.Safe.t) : string = params |> member "textDocument" |> member "uri" |> to_string

  let path (params : Yojson.Safe.t) : string = L.Workspace.path_of_uri (uri params)

  (** The 0-based line and UTF-16 column of a position *)
  let position (params : Yojson.Safe.t) : int * int =
    let position = member "position" params in
    (position |> member "line" |> to_int, position |> member "character" |> to_int)

  let content (state : State.t) (path : string) : string =
    match L.Workspace.find_document state.workspace path with Some document -> document.content | None -> ""

  (** The document content and the 0-based line and byte column of the position *)
  let content_and_position (state : State.t) (params : Yojson.Safe.t) : string * int * int =
    let content = content state (path params) in
    let line, character = position params in
    (content, line, L.Text.byte_of_utf16 (L.Text.of_string content) line character)

  (** Workspace folder given in the initialize request *)
  let root (params : Yojson.Safe.t) : string option =
    match (member "rootUri" params, member "workspaceFolders" params) with
    | `String uri, _ ->
        Some (L.Workspace.path_of_uri uri)
    | _, `List (folder :: _) -> (
      match member "uri" folder with `String uri -> Some (L.Workspace.path_of_uri uri) | _ -> None )
    | _ ->
        None
end

(** Request handlers return the new state and the result of the request *)
module RequestHandlers = struct
  let initialize (state : State.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t =
    let root = Params.root params in
    log "Initialize, workspace root: %s" (Option.value root ~default:"none") ;
    let state = {state with phase= Running; workspace= L.Workspace.create ?root ()} in
    (state, `Assoc [("capabilities", server_capabilities_json)])

  let completion (state : State.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t =
    let state, analysis = State.analysis state (Params.path params) in
    let files = match analysis with Some analysis -> analysis.files | None -> [] in
    let stmts = CCList.flat_map (fun (file : Pparser.Parse.parsed_file) -> file.stmts) files in
    let item (completion : L.Completion.completion_item) =
      let optional name value = Option.to_list (Option.map (fun v -> (name, `String v)) value) in
      `Assoc
        ( [("label", `String completion.label); ("kind", `Int completion.kind)]
        @ optional "detail" completion.detail
        @ optional "documentation" completion.documentation )
    in
    let items = CCList.map item (L.Completion.get_completions stmts) in
    (state, `Assoc [("isIncomplete", `Bool false); ("items", `List items)])

  let hover (state : State.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t =
    let _, line, column = Params.content_and_position state params in
    let state, analysis = State.analysis state (Params.path params) in
    let stmts = match analysis with Some analysis -> L.Analysis.own_typed_statements analysis | None -> [] in
    (* Vult lines are 1-based *)
    match L.Hover.get_hover_info stmts (line + 1) column with
    | Some text ->
        let value = Printf.sprintf "```vult\n%s\n```" text in
        (state, `Assoc [("contents", `Assoc [("kind", `String "markdown"); ("value", `String value)])])
    | None ->
        (state, `Null)

  let semantic_tokens (state : State.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t =
    let tokens = L.SemanticTokens.get_semantic_tokens (Params.content state (Params.path params)) in
    (state, `Assoc [("data", `List (CCList.map (fun i -> `Int i) tokens))])

  let document_symbol (state : State.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t =
    let path = Params.path params in
    let state, analysis = State.analysis state path in
    let own_files = match analysis with Some analysis -> L.Analysis.own_files analysis | None -> [] in
    let stmts = CCList.flat_map (fun (file : Pparser.Parse.parsed_file) -> file.stmts) own_files in
    let text = L.Text.of_string (Params.content state path) in
    (state, `List (L.DocumentSymbols.get_document_symbols text stmts))

  let definition (state : State.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t =
    let content, line, column = Params.content_and_position state params in
    let state, analysis = State.analysis state (Params.path params) in
    let target =
      match (analysis, L.Common.identifier_at content line column) with
      | Some analysis, Some id ->
          L.GoToDefinition.find analysis id
      | _ ->
          None
    in
    match target with
    | Some (file, loc) ->
        let text = L.Text.of_string (Option.value (L.Workspace.read_source state.workspace file) ~default:"") in
        let range = L.Range.of_loc text loc in
        let uri =
          match L.Workspace.find_document state.workspace (L.Workspace.resolve_path state.workspace file) with
          | Some document ->
              document.uri
          | None ->
              L.Uri.of_path file
        in
        (state, `Assoc [("uri", `String uri); ("range", L.Range.to_json range)])
    | None ->
        (state, `Null)

  let find (method_ : string) : (State.t -> Yojson.Safe.t -> State.t * Yojson.Safe.t) option =
    match method_ with
    | "textDocument/completion" ->
        Some completion
    | "textDocument/hover" ->
        Some hover
    | "textDocument/semanticTokens/full" ->
        Some semantic_tokens
    | "textDocument/documentSymbol" ->
        Some document_symbol
    | "textDocument/definition" ->
        Some definition
    | _ ->
        None
end

(** Notification handlers return the new state and the notifications to send *)
module NotificationHandlers = struct
  (** Publishes the diagnostics of the given open documents *)
  let publish_diagnostics (state : State.t) (paths : string list) : State.t * Yojson.Safe.t list =
    let publish (state, notifications) path =
      match (L.Workspace.find_document state.State.workspace path, State.analysis state path) with
      | Some document, (state, Some analysis) ->
          let diagnostics = L.Diagnostics.of_analysis analysis document.content in
          log "%d diagnostics for %s" (CCList.length diagnostics) path ;
          let params = `Assoc [("uri", `String document.uri); ("diagnostics", `List diagnostics)] in
          (state, Message.notification "textDocument/publishDiagnostics" params :: notifications)
      | _, (state, _) ->
          (state, notifications)
    in
    let state, notifications = CCList.fold_left publish (state, []) paths in
    (state, CCList.rev notifications)

  (** Updates the content of a document and publishes the diagnostics of the documents it affects *)
  let set_document (state : State.t) (uri : string) (content : string) : State.t * Yojson.Safe.t list =
    let path = L.Workspace.path_of_uri uri in
    let state = {state with workspace= L.Workspace.open_document state.workspace uri content} in
    publish_diagnostics state (State.affected_documents state path)

  let did_open (state : State.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t list =
    let document = member "textDocument" params in
    set_document state (document |> member "uri" |> to_string) (document |> member "text" |> to_string)

  (** With full synchronization the last change holds the whole document *)
  let did_change (state : State.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t list =
    match CCList.last_opt (params |> member "contentChanges" |> to_list) with
    | Some change ->
        set_document state (Params.uri params) (change |> member "text" |> to_string)
    | None ->
        (state, [])

  (** Clears the diagnostics of the closed document. The documents importing it now read it from disk. *)
  let did_close (state : State.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t list =
    let uri = Params.uri params in
    let path = L.Workspace.path_of_uri uri in
    let dependents = CCList.filter (fun p -> not (String.equal p path)) (State.affected_documents state path) in
    let state =
      {state with workspace= L.Workspace.close_document state.workspace uri; analyses= Map.remove path state.analyses}
    in
    let state, notifications = publish_diagnostics state dependents in
    let cleared =
      Message.notification "textDocument/publishDiagnostics" (`Assoc [("uri", `String uri); ("diagnostics", `List [])])
    in
    (state, cleared :: notifications)

  let find (method_ : string) : (State.t -> Yojson.Safe.t -> State.t * Yojson.Safe.t list) option =
    match method_ with
    | "textDocument/didOpen" ->
        Some did_open
    | "textDocument/didChange" ->
        Some did_change
    | "textDocument/didClose" ->
        Some did_close
    | _ ->
        None
end

(** What the server does after handling a message *)
type outcome = Continue of State.t * Yojson.Safe.t list | Exit of int

(** The protocol asks for exit code 0 only when the exit was preceded by a shutdown request *)
let exit_code (phase : phase) : int = match phase with ShutdownRequested -> 0 | AwaitingInitialize | Running -> 1

(** Runs a request handler. A failing handler is answered with an error instead of leaving the request unanswered. *)
let run_request handler (state : State.t) (id : Yojson.Safe.t) (params : Yojson.Safe.t) : State.t * Yojson.Safe.t =
  match handler state params with
  | state, result ->
      (state, Message.response id result)
  | exception Type_error (msg, _) ->
      (state, Message.error id InvalidParams msg)
  | exception exn ->
      let msg = Printexc.to_string exn in
      log_error "Request failed: %s" msg ;
      (state, Message.error id InternalError msg)

let handle_request (state : State.t) (id : Yojson.Safe.t) (method_ : string) (params : Yojson.Safe.t) :
    State.t * Yojson.Safe.t =
  log "Request: %s" method_ ;
  match (state.phase, method_) with
  | AwaitingInitialize, "initialize" ->
      run_request RequestHandlers.initialize state id params
  | AwaitingInitialize, _ ->
      (state, Message.error id ServerNotInitialized "The server has not been initialized")
  | ShutdownRequested, _ ->
      (state, Message.error id InvalidRequest "The server is shutting down")
  | Running, "initialize" ->
      (state, Message.error id InvalidRequest "The server is already initialized")
  | Running, "shutdown" ->
      ({state with phase= ShutdownRequested}, Message.response id `Null)
  | Running, _ -> (
    match RequestHandlers.find method_ with
    | Some handler ->
        run_request handler state id params
    | None ->
        (state, Message.error id MethodNotFound ("Method not found: " ^ method_)) )

let handle_notification (state : State.t) (method_ : string) (params : Yojson.Safe.t) : outcome =
  log "Notification: %s" method_ ;
  match (state.phase, method_) with
  | _, "exit" ->
      Exit (exit_code state.phase)
  | Running, _ -> (
    match NotificationHandlers.find method_ with
    | Some handler -> (
      match handler state params with
      | state, notifications ->
          Continue (state, notifications)
      | exception exn ->
          log_error "Notification %s failed: %s" method_ (Printexc.to_string exn) ;
          Continue (state, []) )
    | None ->
        Continue (state, []) )
  | (AwaitingInitialize | ShutdownRequested), _ ->
      Continue (state, [])

let handle_message (state : State.t) (content : string) : outcome =
  match Yojson.Safe.from_string content with
  | exception Yojson.Json_error msg ->
      Continue (state, [Message.error `Null ParseError msg])
  | json -> (
    match Message.classify json with
    | Some (Request {id; method_; params}) ->
        let state, reply = handle_request state id method_ params in
        Continue (state, [reply])
    | Some (Notification {method_; params}) ->
        handle_notification state method_ params
    | Some Response ->
        Continue (state, [])
    | None ->
        Continue (state, [Message.error `Null InvalidRequest "Invalid JSON-RPC message"]) )

(** LSP Communication Module *)
module Communication = struct
  (** Read LSP header and content. A message without a valid Content-Length cannot be read. *)
  let read_lsp_message ic : (string, string) result =
    let rec read_headers acc =
      let line = String.trim (input_line ic) in
      if line = "" then acc
      else
        match String.split_on_char ':' line with
        | [key; value] ->
            read_headers ((String.trim key, String.trim value) :: acc)
        | _ ->
            read_headers acc
    in
    let headers = read_headers [] in
    match CCList.assoc_opt ~eq:String.equal "Content-Length" headers with
    | None ->
        Error "Missing Content-Length header"
    | Some len -> (
      match int_of_string_opt len with
      | Some content_length when content_length >= 0 ->
          Ok (really_input_string ic content_length)
      | _ ->
          Error ("Invalid Content-Length header: " ^ len) )

  (** Write LSP response *)
  let write_lsp_response oc json =
    let content = Yojson.Safe.to_string json in
    Printf.fprintf oc "Content-Length: %d\r\n\r\n%s" (String.length content) content ;
    flush oc
end

(** Main server loop *)
let run () =
  log "Vult Language Server starting" ;
  let rec loop (state : State.t) =
    match Communication.read_lsp_message stdin with
    | exception End_of_file ->
        log "Client disconnected" ;
        exit (exit_code state.phase)
    | Error msg ->
        log_error "Invalid message: %s" msg ; loop state
    | Ok content -> (
      match handle_message state content with
      | Exit code ->
          exit code
      | Continue (state, messages) ->
          CCList.iter (Communication.write_lsp_response stdout) messages ;
          loop state )
  in
  loop State.empty

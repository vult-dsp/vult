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

(* Vult Language Server - editor features for the Vult language, independent of the LSP transport.

   Documents are analyzed together with the modules they import. The contents of open documents
   take precedence over the files on disk, and every analysis records the digests of the files it
   read so that it can be reused until one of them changes.

   The compiler measures columns in bytes, while LSP measures them in UTF-16 code units; [Text]
   converts between both. *)

open Util

(** File URIs as sent by the editor *)
module Uri = struct
  let hex_value (c : char) : int option =
    match c with
    | '0' .. '9' ->
        Some (Char.code c - Char.code '0')
    | 'a' .. 'f' ->
        Some (Char.code c - Char.code 'a' + 10)
    | 'A' .. 'F' ->
        Some (Char.code c - Char.code 'A' + 10)
    | _ ->
        None

  (** Decodes the %XX escapes. Malformed escapes are kept as they are. *)
  let percent_decode (s : string) : string =
    let n = String.length s in
    let buffer = Buffer.create n in
    let rec loop i =
      if i < n then
        let escaped = if s.[i] = '%' && i + 2 < n then (hex_value s.[i + 1], hex_value s.[i + 2]) else (None, None) in
        match escaped with
        | Some high, Some low ->
            Buffer.add_char buffer (Char.chr ((high * 16) + low)) ;
            loop (i + 3)
        | _ ->
            Buffer.add_char buffer s.[i] ;
            loop (i + 1)
    in
    loop 0 ; Buffer.contents buffer

  let is_unreserved (c : char) : bool =
    match c with 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' | '/' -> true | _ -> false

  let percent_encode (s : string) : string =
    let buffer = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        if is_unreserved c then Buffer.add_char buffer c
        else Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code c)) )
      s ;
    Buffer.contents buffer

  (** True for paths like "C:/dir" *)
  let starts_with_drive (path : string) : bool =
    String.length path >= 2 && path.[1] = ':' && match path.[0] with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

  (** Converts a file URI to a path. Other URIs are returned unchanged. *)
  let to_path (uri : string) : string =
    match CCString.chop_prefix ~pre:"file://" uri with
    | None ->
        uri
    | Some encoded ->
        let path = percent_decode encoded in
        (* "file:///C:/dir" denotes the Windows path "C:/dir" *)
        let without_slash = CCString.drop 1 path in
        if CCString.prefix ~pre:"/" path && starts_with_drive without_slash then without_slash else path

  (** Converts a path to a file URI. Relative paths are resolved against the current directory. *)
  let of_path (path : string) : string =
    let path = if Filename.is_relative path then Filename.concat (FileIO.cwd ()) path else path in
    let path = if Sys.win32 then String.map (fun c -> if c = '\\' then '/' else c) path else path in
    let path = if starts_with_drive path then "/" ^ path else path in
    "file://" ^ percent_encode path
end

(** File paths *)
module Path = struct
  (** Resolves the "." and ".." segments, so that a file is always named the same way *)
  let normalize (path : string) : string =
    let absolute = CCString.prefix ~pre:"/" path in
    let step (acc : string list) (segment : string) =
      match (segment, acc) with
      | ("" | "."), _ ->
          acc
      | "..", previous :: rest when not (String.equal previous "..") ->
          rest
      | "..", [] when absolute ->
          acc
      | _ ->
          segment :: acc
    in
    let segments = CCList.rev (CCList.fold_left step [] (String.split_on_char '/' path)) in
    let joined = String.concat "/" segments in
    if absolute then "/" ^ joined else joined
end

(** Text of a document, split in lines, used to convert byte columns into UTF-16 columns *)
module Text = struct
  type t = string array

  let of_string (content : string) : t = Array.of_list (String.split_on_char '\n' content)

  let line (text : t) (n : int) : string = if n >= 0 && n < Array.length text then text.(n) else ""

  (** UTF-16 code units of the character starting at byte [i] and the number of bytes it uses *)
  let decode (s : string) (i : int) : int * int =
    let decoded = String.get_utf_8_uchar s i in
    let units =
      if Uchar.utf_decode_is_valid decoded then Uchar.utf_16_byte_length (Uchar.utf_decode_uchar decoded) / 2 else 1
    in
    (units, Uchar.utf_decode_length decoded)

  (** Converts a byte column into a UTF-16 column. Columns past the end of the line are kept past the end. *)
  let utf16_of_byte (text : t) (line_number : int) (byte_column : int) : int =
    let s = line text line_number in
    let limit = min byte_column (String.length s) in
    let rec loop i units =
      if i >= limit then units + (byte_column - i)
      else
        let char_units, bytes = decode s i in
        loop (i + bytes) (units + char_units)
    in
    loop 0 0

  (** Converts a UTF-16 column into a byte column *)
  let byte_of_utf16 (text : t) (line_number : int) (utf16_column : int) : int =
    let s = line text line_number in
    let rec loop i units =
      if units >= utf16_column || i >= String.length s then i + max 0 (utf16_column - units)
      else
        let char_units, bytes = decode s i in
        loop (i + bytes) (units + char_units)
    in
    loop 0 0
end

(** LSP ranges: 0-based lines and UTF-16 columns *)
module Range = struct
  type position = {line: int; character: int}

  type t = {start: position; end_: position}

  let origin : t = {start= {line= 0; character= 0}; end_= {line= 0; character= 0}}

  let of_lexing_position (text : Text.t) (p : Lexing.position) : position =
    let line = max 0 (p.pos_lnum - 1) in
    let byte_column = max 0 (p.pos_cnum - p.pos_bol) in
    {line; character= Text.utf16_of_byte text line byte_column}

  (** Range of a location, given the text of the file the location belongs to *)
  let of_loc (text : Text.t) (loc : Loc.t) : t =
    {start= of_lexing_position text loc.start_pos; end_= of_lexing_position text loc.end_pos}

  let position_to_json (p : position) : Yojson.Safe.t = `Assoc [("line", `Int p.line); ("character", `Int p.character)]

  let to_json (range : t) : Yojson.Safe.t =
    `Assoc [("start", position_to_json range.start); ("end", position_to_json range.end_)]
end

(** Tokens of a document *)
module Common = struct
  type vult_token = Pparser.Tokens.token_enum Pparser.Tokens.token

  (** Tokens of the content, comments included. A lexer error ends the list, so that a document being
      edited is still partially tokenized. *)
  let tokenize (content : string) : vult_token list =
    let lexbuf = Lexing.from_string content in
    let rec loop acc =
      match Pparser.Lexer.next_token_config (Loc.File "") Pparser.Tokens.comment_config lexbuf with
      | {kind= Pparser.Tokens.EOF; _} ->
          CCList.rev acc
      | token ->
          loop (token :: acc)
      | exception _ ->
          CCList.rev acc
    in
    loop []

  (** True if the token covers the given 0-based line and byte column *)
  let token_contains (token : vult_token) (line : int) (column : int) : bool =
    let start_pos = token.loc.start_pos in
    let end_pos = token.loc.end_pos in
    start_pos.pos_lnum = line + 1
    && end_pos.pos_lnum = line + 1
    && column >= start_pos.pos_cnum - start_pos.pos_bol
    && column < end_pos.pos_cnum - end_pos.pos_bol

  (** An identifier, with the module that qualifies it as in [Module.name] *)
  type identifier = {qualifier: string option; name: string}

  (** Finds the identifier at the given 0-based line and byte column *)
  let identifier_at (content : string) (line : int) (column : int) : identifier option =
    let rec find (tokens : vult_token list) =
      match tokens with
      | [] ->
          None
      | ({kind= ID; _} as qualifier) :: {kind= DOT; _} :: ({kind= ID; _} as id) :: _ when token_contains id line column
        ->
          Some {qualifier= Some qualifier.value; name= id.value}
      | ({kind= ID; _} as id) :: _ when token_contains id line column ->
          Some {qualifier= None; name= id.value}
      | _ :: rest ->
          find rest
    in
    find (tokenize content)
end

(** Documents open in the editor *)
module Workspace = struct
  type document = {uri: string; content: string}

  type t =
    { root: string option (* path of the workspace folder, used as include path *)
    ; documents: document Maps.Map.t (* normalized path -> document *) }

  let create ?root () : t = {root; documents= Maps.Map.empty}

  let path_of_uri (uri : string) : string = Path.normalize (Uri.to_path uri)

  let open_document (workspace : t) (uri : string) (content : string) : t =
    {workspace with documents= Maps.Map.add (path_of_uri uri) {uri; content} workspace.documents}

  let close_document (workspace : t) (uri : string) : t =
    {workspace with documents= Maps.Map.remove (path_of_uri uri) workspace.documents}

  let find_document (workspace : t) (path : string) : document option = Maps.Map.find_opt path workspace.documents

  (** Path of a file, using the name of the open document when it differs only in case: the loader looks for
      lowercase file names first, which on case-insensitive file systems also finds capitalized files. *)
  let resolve_path (workspace : t) (path : string) : string =
    let path = Path.normalize path in
    let same_ignoring_case other = String.equal (String.lowercase_ascii other) (String.lowercase_ascii path) in
    if Maps.Map.mem path workspace.documents then path
    else
      match CCList.find_opt same_ignoring_case (CCList.map fst (Maps.Map.bindings workspace.documents)) with
      | Some document_path ->
          document_path
      | None ->
          path

  let open_paths (workspace : t) : string list = CCList.map fst (Maps.Map.bindings workspace.documents)

  (** Contents of a file, taken from the editor when the file is open *)
  let read_source (workspace : t) (path : string) : string option =
    match find_document workspace (resolve_path workspace path) with
    | Some document ->
        Some document.content
    | None ->
        FileIO.read path
end

(** Configuration file support for vultconfig.json *)
module Config = struct
  type t =
    { file: string option (* the vultconfig.json that applies, if any *)
    ; include_paths: string list (* absolute include paths *) }

  let file_name = "vultconfig.json"

  (** Find vultconfig.json by searching upward from start_dir *)
  let rec find_file (dir : string) : string option =
    let candidate = Filename.concat dir file_name in
    if FileIO.exists candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else find_file parent

  (** Reads the "include" list of the configuration. Entries that are not strings are ignored. *)
  let parse_includes (content : string) : string list =
    match Yojson.Safe.from_string content with
    | `Assoc fields -> (
      match CCList.assoc_opt ~eq:String.equal "include" fields with
      | Some (`List entries) ->
          CCList.filter_map (function `String path -> Some path | _ -> None) entries
      | _ ->
          [] )
    | _ ->
        []
    | exception Yojson.Json_error _ ->
        []

  (** Configuration for a file. Relative include paths are resolved against the configuration's directory. *)
  let for_file (read : string -> string option) (file_path : string) : t =
    match find_file (Filename.dirname file_path) with
    | None ->
        {file= None; include_paths= []}
    | Some config_file ->
        let config_dir = Filename.dirname config_file in
        let resolve path = if Filename.is_relative path then Filename.concat config_dir path else path in
        let includes = Option.fold ~none:[] ~some:parse_includes (read config_file) in
        {file= Some config_file; include_paths= CCList.map resolve includes}
end

(** Loads a document with the modules it imports and typechecks them *)
module Analysis = struct
  (** A file read by the analysis and the digest of its contents ([None] if it could not be read) *)
  type source = {path: string; digest: Digest.t option}

  type t =
    { path: string (* the analyzed document *)
    ; sources: source list
    ; files: Pparser.Parse.parsed_file list
          (* the document and its imports. When parsing fails, the files of the previous analysis are kept *)
    ; errors: Error.t list
    ; warnings: Error.warning list
    ; typed: Core.Typed.top_stmt list option Lazy.t (* elaborated program, [None] if it has errors *) }

  let digest (content : string option) : Digest.t option = Option.map Digest.string content

  let read_source (workspace : Workspace.t) (path : string) : source =
    {path= Workspace.resolve_path workspace path; digest= digest (Workspace.read_source workspace path)}

  (** True if none of the files read by the analysis has changed *)
  let is_current (workspace : Workspace.t) (analysis : t) : bool =
    CCList.for_all
      (fun (source : source) -> Option.equal Digest.equal (read_source workspace source.path).digest source.digest)
      analysis.sources

  (** True if the analysis read the file at [path], which may have been opened since *)
  let depends_on (workspace : Workspace.t) (analysis : t) (path : string) : bool =
    CCList.exists
      (fun (source : source) -> String.equal (Workspace.resolve_path workspace source.path) path)
      analysis.sources

  let internal_error (exn : exn) : Error.t = Error.SimpleError ("Internal compiler error: " ^ Printexc.to_string exn)

  (** The errors and the warnings of the type inference. The bodies of generic functions are checked
      when they are specialized, so their warnings are only reported by the compiler. *)
  let typecheck (args : Args.args) (files : Pparser.Parse.parsed_file list) : Error.t list * Error.warning list =
    match Core.Typechecking.typecheck args files with
    | env, _ ->
        ([], Core.Env.getWarnings env)
    | exception Error.Errors errors ->
        (errors, [])
    | exception exn ->
        ([internal_error exn], [])

  let elaborate (args : Args.args) (files : Pparser.Parse.parsed_file list) : Core.Typed.top_stmt list option =
    match Core.Typechecking.typecheck_and_elaborate args files with _, stmts -> Some stmts | exception _ -> None

  (** Analyzes the document at [path]. [previous] provides the files to keep when parsing fails. *)
  let analyze ?(previous : t option) (workspace : Workspace.t) (path : string) : t =
    let config = Config.for_file (Workspace.read_source workspace) path in
    let includes = CCList.uniq ~eq:String.equal (Option.to_list workspace.root @ config.include_paths) in
    let args = {Args.default_arguments with files= [Args.File path]; includes} in
    (* records the files read by the loader *)
    let reads = ref [] in
    let read file =
      let content = Workspace.read_source workspace file in
      reads := {path= Workspace.resolve_path workspace file; digest= digest content} :: !reads ;
      content
    in
    let loaded =
      match Driver.Loader.loadFiles ~read args args.files with
      | files, _ ->
          Ok files
      | exception Error.Errors errors ->
          Error errors
      | exception exn ->
          Error [internal_error exn]
    in
    let config_sources = CCList.map (read_source workspace) (Option.to_list config.file) in
    let sources = config_sources @ CCList.rev !reads in
    match loaded with
    | Ok files ->
        let errors, warnings = typecheck args files in
        {path; sources; files; errors; warnings; typed= lazy (elaborate args files)}
    | Error errors ->
        let files = match previous with Some analysis -> analysis.files | None -> [] in
        {path; sources; files; errors; warnings= []; typed= lazy None}

  (** The parsed file of the analyzed document *)
  let own_files (analysis : t) : Pparser.Parse.parsed_file list =
    CCList.filter
      (fun (file : Pparser.Parse.parsed_file) -> String.equal (Path.normalize file.file) analysis.path)
      analysis.files

  (** The elaborated statements that belong to the analyzed document *)
  let own_typed_statements (analysis : t) : Core.Typed.top_stmt list =
    match Lazy.force analysis.typed with
    | None ->
        []
    | Some stmts ->
        CCList.filter
          (fun (stmt : Core.Typed.top_stmt) -> String.equal (Path.normalize (Loc.file stmt.loc)) analysis.path)
          stmts
end

(** Diagnostics module *)
module Diagnostics = struct
  let severity_error = 1

  let severity_warning = 2

  (* Editors render the code with this tag faded out *)
  let tag_unnecessary = 1

  let make (range : Range.t) (message : string) : Yojson.Safe.t =
    `Assoc [("range", Range.to_json range); ("severity", `Int severity_error); ("message", `String message)]

  (** Locations without a file name belong to the document being checked *)
  let belongs_to (path : string) (loc : Loc.t) : bool =
    let file = Loc.file loc in
    String.equal file "" || String.equal (Path.normalize file) path

  (** Converts an error into a diagnostic of the document at [path]. Errors in imported files are
      reported at the start of the document, since the import statement has no location. *)
  let of_error (path : string) (text : Text.t) (error : Error.t) : Yojson.Safe.t =
    match error with
    | Error.PointedError (loc, msg) when belongs_to path loc ->
        make (Range.of_loc text loc) msg
    | Error.PointedError (loc, msg) ->
        make Range.origin (Printf.sprintf "Error in imported file %s:%d: %s" (Loc.file loc) (Loc.line loc) msg)
    | Error.SimpleError msg ->
        make Range.origin msg

  (** Converts the warnings of the document at [path]. Warnings in imported files are not shown. *)
  let of_warnings (path : string) (text : Text.t) (warnings : Error.warning list) : Yojson.Safe.t list =
    let of_warning (warning : Error.warning) =
      `Assoc
        [ ("range", Range.to_json (Range.of_loc text warning.loc))
        ; ("severity", `Int severity_warning)
        ; ("tags", `List [`Int tag_unnecessary])
        ; ("message", `String warning.msg) ]
    in
    warnings |> CCList.filter (fun (warning : Error.warning) -> belongs_to path warning.loc) |> CCList.map of_warning

  let of_analysis (analysis : Analysis.t) (content : string) : Yojson.Safe.t list =
    let text = Text.of_string content in
    CCList.map (of_error analysis.path text) analysis.errors @ of_warnings analysis.path text analysis.warnings

  (** Diagnostics of a document that is not part of a workspace *)
  let get_diagnostics (content : string) (filename : string) : Yojson.Safe.t list =
    let workspace = Workspace.open_document (Workspace.create ()) filename content in
    let path = Workspace.path_of_uri filename in
    of_analysis (Analysis.analyze workspace path) content
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
    ; "and" ]

  (** Completion item information *)
  type completion_item =
    {label: string; kind: int (* LSP CompletionItemKind *); detail: string option; documentation: string option}

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
          | None ->
              Some (Printf.sprintf "fun %s(...)" func_def.name)
        in
        {label= func_def.name; kind= completion_kind_function; detail; documentation= None}
      in
      match func_def.next with
      | Some next_func ->
          current_completion :: extract_function_completions next_func
      | None ->
          [current_completion]
    in
    let extract_external_completion (ext_def : Pparser.Syntax.ext_def) =
      let detail =
        match ext_def.t with
        | Some return_type ->
            let type_str = Pparser.Syntax.Print.type_ return_type |> Pla.print in
            Some (Printf.sprintf "external %s(...) : %s" ext_def.name type_str)
        | None ->
            Some (Printf.sprintf "external %s(...)" ext_def.name)
      in
      {label= ext_def.name; kind= completion_kind_function; detail; documentation= Some "External function"}
    in
    let extract_type_completion name _members_list =
      {label= name; kind= completion_kind_struct; detail= Some ("type " ^ name); documentation= Some "User-defined type"}
    in
    let extract_enum_completion name members_list =
      let enum_item =
        {label= name; kind= completion_kind_enum; detail= Some ("enum " ^ name); documentation= Some "Enumeration type"}
      in
      let member_items =
        CCList.map
          (fun (member_name, _) ->
            { label= member_name
            ; kind= completion_kind_enum_member
            ; detail= Some (name ^ "::" ^ member_name)
            ; documentation= Some "Enumeration value" } )
          members_list
      in
      enum_item :: member_items
    in
    let extract_constant_completion (dexp : Pparser.Syntax.dexp) =
      match dexp.d with
      | SDId (id, _) ->
          Some
            { label= id
            ; kind= completion_kind_constant
            ; detail= Some ("constant " ^ id)
            ; documentation= Some "Constant value" }
      | _ ->
          None (* Skip constants without simple identifiers *)
    in
    let extract_from_stmt (stmt : Pparser.Syntax.top_stmt) =
      match stmt.top with
      | STopFunction func_def ->
          extract_function_completions func_def
      | STopExternal (ext_def, _) ->
          [extract_external_completion ext_def]
      | STopType {name; members} ->
          [extract_type_completion name members]
      | STopEnum {name; members} ->
          extract_enum_completion name members
      | STopConstant (dexp, _) -> (
        match extract_constant_completion dexp with Some completion -> [completion] | None -> [] )
      | STopError ->
          []
    in
    CCList.flat_map extract_from_stmt parsed_stmts

  (** Get all completions from pre-parsed statements *)
  let get_all_completions (parsed_stmts : Pparser.Syntax.top_stmt list) : completion_item list =
    (* Keyword completions *)
    let keyword_completions =
      CCList.map
        (fun keyword -> {label= keyword; kind= completion_kind_keyword; detail= Some "keyword"; documentation= None})
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

  (** [loc] covers the whole definition and [name_loc] the name, which must be inside [loc] *)
  let create_symbol_json (text : Text.t) (name : string) (kind : int) ~(loc : Loc.t) ~(name_loc : Loc.t) =
    `Assoc
      [ ("name", `String name)
      ; ("kind", `Int kind)
      ; ("range", Range.to_json (Range.of_loc text loc))
      ; ("selectionRange", Range.to_json (Range.of_loc text name_loc)) ]

  (** Extract symbols from parsed Vult AST - returns JSON objects *)
  let get_document_symbols (text : Text.t) (parsed_stmts : Pparser.Syntax.top_stmt list) : Yojson.Safe.t list =
    let symbol name kind ~loc ~name_loc = create_symbol_json text name kind ~loc ~name_loc in
    let rec function_symbols (def : Pparser.Syntax.function_def) =
      let current = symbol def.name symbol_kind_function ~loc:(Loc.merge def.loc def.body.loc) ~name_loc:def.loc in
      match def.next with Some next -> current :: function_symbols next | None -> [current]
    in
    let extract_from_stmt (stmt : Pparser.Syntax.top_stmt) =
      match stmt.top with
      | STopFunction def ->
          function_symbols def
      | STopExternal (def, _) ->
          [symbol def.name symbol_kind_function ~loc:def.loc ~name_loc:def.loc]
      | STopType {name; members} ->
          let loc = Loc.mergeList stmt.loc (stmt.loc :: CCList.map (fun (_, _, _, loc) -> loc) members) in
          [symbol name symbol_kind_struct ~loc ~name_loc:stmt.loc]
      | STopEnum {name; members} ->
          let loc = Loc.mergeList stmt.loc (stmt.loc :: CCList.map snd members) in
          let member_symbol (member, member_loc) =
            symbol member symbol_kind_enum_member ~loc:member_loc ~name_loc:member_loc
          in
          symbol name symbol_kind_enum ~loc ~name_loc:stmt.loc :: CCList.map member_symbol members
      | STopConstant ({d= SDId (id, _); _}, value) ->
          [symbol id symbol_kind_constant ~loc:(Loc.merge stmt.loc value.loc) ~name_loc:stmt.loc]
      | STopConstant _ | STopError ->
          []
    in
    CCList.flat_map extract_from_stmt parsed_stmts
end

(** Go-to-Definition module *)
module GoToDefinition = struct
  (** Location of a top-level definition with the given name *)
  let find_in_file (name : string) (file : Pparser.Parse.parsed_file) : Loc.t option =
    let rec in_function (def : Pparser.Syntax.function_def) =
      if String.equal def.name name then Some def.loc else Option.bind def.next in_function
    in
    let in_stmt (stmt : Pparser.Syntax.top_stmt) =
      match stmt.top with
      | STopFunction def ->
          in_function def
      | STopExternal (def, _) when String.equal def.name name ->
          Some def.loc
      | STopEnum {members; _} ->
          CCList.assoc_opt ~eq:String.equal name members
      | STopConstant ({d= SDId (id, _); _}, _) when String.equal id name ->
          Some stmt.loc
      | STopType {name= type_name; _} when String.equal type_name name ->
          Some stmt.loc
      | STopExternal _ | STopConstant _ | STopType _ | STopError ->
          None
    in
    CCList.find_map in_stmt file.stmts

  (** Location of the start of a file *)
  let file_start (file : Pparser.Parse.parsed_file) : Loc.t =
    let position = {Lexing.pos_fname= file.file; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} in
    {Loc.start_pos= position; end_pos= position; source= Loc.File file.file}

  (** Finds where an identifier is defined, searching the document first and then its imports.
      A qualified identifier is searched only in its module, and a module name leads to its file.
      Returns the path of the file and the location. *)
  let find (analysis : Analysis.t) (id : Common.identifier) : (string * Loc.t) option =
    let in_files files =
      CCList.find_map
        (fun (file : Pparser.Parse.parsed_file) -> Option.map (fun loc -> (file.file, loc)) (find_in_file id.name file))
        files
    in
    let module_file name =
      CCList.find_opt (fun (file : Pparser.Parse.parsed_file) -> String.equal file.name name) analysis.files
    in
    match id.qualifier with
    | Some qualifier ->
        in_files (Option.to_list (module_file qualifier))
    | None -> (
        let own = Analysis.own_files analysis in
        let imported = CCList.filter (fun file -> not (CCList.memq file own)) analysis.files in
        match in_files (own @ imported) with
        | Some _ as found ->
            found
        | None ->
            Option.map (fun (file : Pparser.Parse.parsed_file) -> (file.file, file_start file)) (module_file id.name) )
end

(** Hover module - provides type information on hover *)
module Hover = struct
  (** Hover result types *)
  type hover_result =
    | HoverExp of Core.Typed.exp (* Variable/expression *)
    | HoverArg of Core.Typed.arg (* Function argument *)
    | HoverFunction of Core.Typed.function_def (* Function name in definition *)
    | HoverNotFound (* Keywords, whitespace, etc. *)

  (** Check if loc1 is strictly smaller than loc2 (contained within but not equal) *)
  let loc_strictly_smaller (loc1 : Loc.t) (loc2 : Loc.t) : bool =
    let s1_line = Loc.startLine loc1 in
    let s1_col = Loc.startColumn loc1 in
    let e1_line = Loc.endLine loc1 in
    let e1_col = Loc.endColumn loc1 in
    let s2_line = Loc.startLine loc2 in
    let s2_col = Loc.startColumn loc2 in
    let e2_line = Loc.endLine loc2 in
    let e2_col = Loc.endColumn loc2 in
    (* loc1 must start at or after loc2's start AND end at or before loc2's end *)
    (* AND at least one boundary must be strictly inside *)
    let starts_at_or_after = s1_line > s2_line || (s1_line = s2_line && s1_col >= s2_col) in
    let ends_at_or_before = e1_line < e2_line || (e1_line = e2_line && e1_col <= e2_col) in
    let strictly_inside =
      s1_line > s2_line
      || (s1_line = s2_line && s1_col > s2_col)
      || e1_line < e2_line
      || (e1_line = e2_line && e1_col < e2_col)
    in
    starts_at_or_after && ends_at_or_before && strictly_inside

  (** Check if a position is within a location.
      LSP uses 0-based lines, Vult Loc.t uses 1-based lines.
      Column is 0-based in both. *)
  let position_in_loc (loc : Loc.t) (line : int) (col : int) : bool =
    let start_line = Loc.startLine loc in
    let end_line = Loc.endLine loc in
    let start_col = Loc.startColumn loc in
    let end_col = Loc.endColumn loc in
    (* Check if position is within the location bounds *)
    if line < start_line || line > end_line then false
    else if line = start_line && line = end_line then col >= start_col && col < end_col
    else if line = start_line then col >= start_col
    else if line = end_line then col < end_col
    else true

  (** Format a type for display *)
  let format_type (t : Core.Typed.type_) : string = Core.Typed.print_type_ t |> Pla.print

  (** Format function signature for display *)
  let format_function_signature (def : Core.Typed.function_def) : string =
    let name = Core.Typed.print_path def.name |> Pla.print in
    let args =
      CCList.map
        (fun (arg : Core.Typed.arg) ->
          let t = format_type arg.t in
          Printf.sprintf "%s : %s" arg.name t )
        def.args
      |> String.concat ", "
    in
    let ret = format_type (snd def.t) in
    Printf.sprintf "fun %s(%s) : %s" name args ret

  (** Check if we're hovering over a function argument definition.
      Skip internal/generated arguments (those starting with '_' like _ctx). *)
  let find_in_args (args : Core.Typed.arg list) (line : int) (col : int) : hover_result option =
    CCList.find_map
      (fun (arg : Core.Typed.arg) ->
        (* Skip internal arguments like _ctx *)
        if String.length arg.name > 0 && arg.name.[0] = '_' then None
        else if position_in_loc arg.loc line col then Some (HoverArg arg)
        else None )
      args

  (** Find hover info in an expression - recurse into children to find the innermost match *)
  let rec find_in_exp (e : Core.Typed.exp) (line : int) (col : int) : hover_result option =
    if not (position_in_loc e.loc line col) then None
    else
      (* Try to find a more specific match in children first *)
      let child_result =
        match e.e with
        | Core.Typed.EIndex {e= sub_e; index} ->
            CCList.find_map (fun exp -> find_in_exp exp line col) [sub_e; index]
        | Core.Typed.EArray elems ->
            CCList.find_map (fun exp -> find_in_exp exp line col) elems
        | Core.Typed.ECall {args; _} ->
            CCList.find_map (fun exp -> find_in_exp exp line col) args
        | Core.Typed.EUnOp (_, sub_e) ->
            find_in_exp sub_e line col
        | Core.Typed.EOp (_, e1, e2) ->
            CCList.find_map (fun exp -> find_in_exp exp line col) [e1; e2]
        | Core.Typed.EIf {cond; then_; else_} ->
            CCList.find_map (fun exp -> find_in_exp exp line col) [cond; then_; else_]
        | Core.Typed.ETuple elems ->
            CCList.find_map (fun exp -> find_in_exp exp line col) elems
        | Core.Typed.EMember (sub_e, _) ->
            (* Only recurse if sub-expression has a strictly smaller location.
             For mem variables, _ctx and _ctx.member have the same location,
             so we should return the member access (with the field type), not _ctx (with record type). *)
            if loc_strictly_smaller sub_e.loc e.loc then find_in_exp sub_e line col else None
        | Core.Typed.ERecord {elems; _} ->
            CCList.find_map (fun (_, exp) -> find_in_exp exp line col) elems
        | Core.Typed.EGenCall {params; _} ->
            CCList.find_map (fun exp -> find_in_exp exp line col) (Core.Typed.callValues params)
        | Core.Typed.EUnit
        | Core.Typed.EBool _
        | Core.Typed.EInt _
        | Core.Typed.EReal _
        | Core.Typed.EFixed _
        | Core.Typed.EString _
        | Core.Typed.EId _
        | Core.Typed.EConst _
        | Core.Typed.ETypeIntrinsic _
        | Core.Typed.EEnum _
        | Core.Typed.EGenCompanionCall _ ->
            None
      in
      (* Return child match if found, otherwise return this expression *)
      match child_result with
      | Some _ ->
          child_result
      | None ->
          Some (HoverExp e)

  (** Find hover info in a left-value expression - recurse into children to find the innermost match *)
  let rec find_in_lexp (le : Core.Typed.lexp) (line : int) (col : int) : hover_result option =
    if not (position_in_loc le.loc line col) then None
    else
      (* Try to find a more specific match in children first *)
      let child_result =
        match le.l with
        | Core.Typed.LWild | Core.Typed.LId _ ->
            None
        | Core.Typed.LMember (sub_le, _) ->
            (* Only recurse if sub-expression has a strictly smaller location *)
            if loc_strictly_smaller sub_le.loc le.loc then find_in_lexp sub_le line col else None
        | Core.Typed.LIndex {e= sub_le; index} -> (
          match find_in_lexp sub_le line col with Some _ as result -> result | None -> find_in_exp index line col )
        | Core.Typed.LTuple lexps ->
            CCList.find_map (fun le -> find_in_lexp le line col) lexps
      in
      (* Return child match if found, otherwise return this lexp's type *)
      match child_result with
      | Some _ ->
          child_result
      | None ->
          let exp : Core.Typed.exp = {e= Core.Typed.EUnit; loc= le.loc; t= le.t} in
          Some (HoverExp exp)

  (** Find hover info in a declaration expression - recurse into children to find the innermost match *)
  let rec find_in_dexp (de : Core.Typed.dexp) (line : int) (col : int) : hover_result option =
    if not (position_in_loc de.loc line col) then None
    else
      (* Try to find a more specific match in children first *)
      let child_result =
        match de.d with
        | Core.Typed.DWild | Core.Typed.DId _ ->
            None
        | Core.Typed.DTuple dexps ->
            CCList.find_map (fun de -> find_in_dexp de line col) dexps
      in
      (* Return child match if found, otherwise return this dexp's type *)
      match child_result with
      | Some _ ->
          child_result
      | None ->
          let exp : Core.Typed.exp = {e= Core.Typed.EUnit; loc= de.loc; t= de.t} in
          Some (HoverExp exp)

  (** Find hover info in a statement.
      Note: Statement locations in the typed AST may not include the full statement,
      so we search children directly without relying on statement bounds. *)
  let rec find_in_stmt (s : Core.Typed.stmt) (line : int) (col : int) : hover_result option =
    match s.s with
    | Core.Typed.StmtVal dexp ->
        find_in_dexp dexp line col
    | Core.Typed.StmtMem (dexp, _) ->
        find_in_dexp dexp line col
    | Core.Typed.StmtBind (lexp, exp) -> (
      match find_in_lexp lexp line col with Some _ as result -> result | None -> find_in_exp exp line col )
    | Core.Typed.StmtReturn exp ->
        find_in_exp exp line col
    | Core.Typed.StmtBlock stmts ->
        CCList.find_map (fun stmt -> find_in_stmt stmt line col) stmts
    | Core.Typed.StmtIf (cond, then_stmt, else_opt) -> (
      match find_in_exp cond line col with
      | Some _ as result ->
          result
      | None -> (
        match find_in_stmt then_stmt line col with
        | Some _ as result ->
            result
        | None -> (
          match else_opt with Some else_stmt -> find_in_stmt else_stmt line col | None -> None ) ) )
    | Core.Typed.StmtWhile (cond, body) -> (
      match find_in_exp cond line col with Some _ as result -> result | None -> find_in_stmt body line col )

  (** Find hover info in a function definition.
      Note: def.loc only covers the function name, not the entire function,
      so we search all children directly without relying on def.loc bounds. *)
  let rec find_in_function_def (def : Core.Typed.function_def) (body : Core.Typed.stmt) (line : int) (col : int) :
      hover_result option =
    (* First check if cursor is on an argument *)
    match find_in_args def.args line col with
    | Some _ as result ->
        result
    | None -> (
      (* Check function body *)
      match find_in_stmt body line col with
      | Some _ as result ->
          result
      | None -> (
        (* Check nested functions *)
        match def.next with
        | Some (next_def, next_body) ->
            find_in_function_def next_def next_body line col
        | None ->
            (* Check if on function name *)
            if position_in_loc def.loc line col then Some (HoverFunction def) else None ) )

  (** Find hover info at a position in the typed AST.
      Note: top_stmt.loc only covers the keyword/name, not the entire definition,
      so we search all children directly without relying on stmt.loc bounds. *)
  let find_at_position (stmts : Core.Typed.top_stmt list) (line : int) (col : int) : hover_result =
    let result =
      CCList.find_map
        (fun (stmt : Core.Typed.top_stmt) ->
          match stmt.top with
          | Core.Typed.TopFunction (def, body) ->
              find_in_function_def def body line col
          | Core.Typed.TopExternal (def, _) -> (
            match find_in_args def.args line col with
            | Some _ as result ->
                result
            | None ->
                if position_in_loc def.loc line col then Some (HoverFunction def) else None )
          | Core.Typed.TopConstant (_path, _dim, t, exp, _) ->
              if position_in_loc exp.loc line col then Some (HoverExp exp)
              else if position_in_loc stmt.loc line col then
                let const_exp : Core.Typed.exp = {e= Core.Typed.EUnit; loc= stmt.loc; t} in
                Some (HoverExp const_exp)
              else None
          | Core.Typed.TopType _ | Core.Typed.TopAlias _ | Core.Typed.TopEnum _ | Core.Typed.TopGenericPlaceholder _ ->
              None )
        stmts
    in
    match result with Some r -> r | None -> HoverNotFound

  (** Get hover text for a hover result *)
  let get_hover_text (result : hover_result) : string option =
    match result with
    | HoverExp exp ->
        Some (format_type exp.t)
    | HoverArg arg ->
        Some (Printf.sprintf "%s : %s" arg.name (format_type arg.t))
    | HoverFunction def ->
        Some (format_function_signature def)
    | HoverNotFound ->
        None

  (** Main entry point: get hover info at a position *)
  let get_hover_info (typed_stmts : Core.Typed.top_stmt list) (line : int) (col : int) : string option =
    let result = find_at_position typed_stmts line col in
    get_hover_text result
end

(** Semantic Tokens module *)
module SemanticTokens = struct
  type token_type = Variable | Comment | Keyword | Number | String | Operator | Punctuation | Function

  (** Index of the token type in [legend] *)
  let index (t : token_type) : int =
    match t with
    | Variable ->
        0
    | Comment ->
        1
    | Keyword ->
        2
    | Number ->
        3
    | String ->
        4
    | Operator ->
        5
    | Punctuation ->
        6
    | Function ->
        7

  (** LSP names of the token types, in the order given by [index] *)
  let legend = ["variable"; "comment"; "keyword"; "number"; "string"; "operator"; "punctuation"; "function"]

  let classify_token (t : Pparser.Tokens.token_enum) : token_type =
    let open Pparser.Tokens in
    match t with
    | BLOCK_COMMENT | LINE_COMMENT ->
        Comment
    | TYPE
    | FUN
    | VAL
    | MEM
    | EXTERNAL
    | RET
    | IF
    | THEN
    | ELSE
    | WHILE
    | ITER
    | MATCH
    | CONSTANT
    | ENUM
    | TRUE
    | FALSE
    | AND ->
        Keyword
    | INT | REAL | FIXED | XINT ->
        Number
    | STRING ->
        String
    | ID | QUOTED_ID ->
        Variable
    | OP ->
        Operator
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | LBRACK
    | RBRACK
    | TAG
    | ARROW
    | COLON
    | SEMI
    | COMMA
    | DOT
    | EQUAL
    | WILD
    | WHITESPACE
    | NEWLINE
    | AT
    | LT
    | GT
    | EOF ->
        Punctuation

  (** Check if an identifier is followed by '(' indicating a function call *)
  let is_function_call (text : Text.t) (line : int) (end_byte : int) : bool =
    let line_text = Text.line text line in
    let rec skip_whitespace pos =
      if pos < String.length line_text && (line_text.[pos] = ' ' || line_text.[pos] = '\t') then
        skip_whitespace (pos + 1)
      else pos
    in
    let pos = skip_whitespace end_byte in
    pos < String.length line_text && line_text.[pos] = '('

  (** A token on a single line: type, line, UTF-16 start column and UTF-16 length *)
  type line_token = token_type * int * int * int

  (** Split a token that spans several lines into one token per line. Tokens covering no characters (for example the
      empty remainder of a line comment that ends with a newline) are dropped. *)
  let split_by_lines (text : Text.t) (token_type : token_type) (start_line : int) (start_byte : int) (end_line : int)
      (end_byte : int) : line_token list =
    let rec collect line acc =
      if line > end_line then CCList.rev acc
      else
        let first = if line = start_line then start_byte else 0 in
        let last = if line = end_line then end_byte else String.length (Text.line text line) in
        let first_utf16 = Text.utf16_of_byte text line first in
        let length = Text.utf16_of_byte text line last - first_utf16 in
        let acc = if length > 0 then (token_type, line, first_utf16, length) :: acc else acc in
        collect (line + 1) acc
    in
    collect start_line []

  (** Tokens of the content for syntax highlighting, one per line *)
  let tokenize_vult_code (content : string) : line_token list =
    let text = Text.of_string content in
    let line_tokens (token : Common.vult_token) =
      let start_pos = token.loc.start_pos in
      let end_pos = token.loc.end_pos in
      (* LSP uses 0-based line numbers *)
      let start_line = start_pos.pos_lnum - 1 in
      let end_line = end_pos.pos_lnum - 1 in
      let start_byte = start_pos.pos_cnum - start_pos.pos_bol in
      let end_byte = end_pos.pos_cnum - end_pos.pos_bol in
      let token_type =
        match classify_token token.kind with
        | Variable when start_line = end_line && is_function_call text start_line end_byte ->
            Function
        | token_type ->
            token_type
      in
      split_by_lines text token_type start_line start_byte end_line end_byte
    in
    CCList.flat_map line_tokens (Common.tokenize content)

  (** Generate LSP semantic tokens from Vult code: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers] for
      every token *)
  let get_semantic_tokens (content : string) : int list =
    let encode (prev_line, prev_char, acc) (token_type, line, char, length) =
      let delta_line = line - prev_line in
      let delta_char = if delta_line = 0 then char - prev_char else char in
      (line, char, 0 :: index token_type :: length :: delta_char :: delta_line :: acc)
    in
    let _, _, encoded = CCList.fold_left encode (0, 0, []) (tokenize_vult_code content) in
    CCList.rev encoded
end

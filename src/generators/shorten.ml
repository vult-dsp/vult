(*
   The MIT License (MIT)

   Copyright (c) 2014-2026 Leonardo Laguna Ruiz

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

(* Implements the -shorten flag: replaces the names of the functions, local variables, types and
   struct members with short generated ones, so the generated code does not reveal the original
   names.

   Only names that the outside world never refers to are replaced. The kept names are:
   - the root functions and the functions taking a root context type as first argument, which
     are the interface the templates and the user code call,
   - the root context types, the types aliased to or from them, their members, which the
     templates read (e.g. [process_ret_0]), and their [<type>_init] and [<type>_alloc] companions,
   - the external functions, which link to native code,
   - the parameters of the kept functions and the context argument [_ctx], which the templates
     inspect.

   The generators derive the companion names from the type name, so a renamed type [t0] gets the
   companions [t0_init] and [t0_alloc]. *)

open Core.Prog
open Util.Maps

type env = {functions: string Map.t; variables: string Map.t; members: string Map.t; types: string Map.t}

let contextType (def : function_def) : string option =
  match def.args with {t= {t= TStruct {path; _}; _}; _} :: _ -> Some path | _ -> None

let rootContextTypes (prog : prog) : Set.t =
  CCList.fold_left
    (fun acc (s : top_stmt) ->
      match s.top with
      | TopFunction (def, _) when def.info.is_root -> (
        match contextType def with Some path -> Set.add path acc | None -> acc )
      | _ ->
          acc )
    Set.empty prog

let aliases (prog : prog) : (string * string) list =
  CCList.filter_map
    (fun (s : top_stmt) -> match s.top with TopAlias {path; alias_of} -> Some (path, alias_of) | _ -> None)
    prog

(* The root context types together with every type aliased to or from them, since a root can
   name its context through an alias. *)
let rec closeOverAliases (aliases : (string * string) list) (types : Set.t) : Set.t =
  let related (path, alias_of) = Set.mem path types || Set.mem alias_of types in
  let linked = CCList.filter related aliases in
  let types' = CCList.fold_left (fun acc (path, alias_of) -> Set.add path (Set.add alias_of acc)) types linked in
  if Set.cardinal types' = Set.cardinal types then types else closeOverAliases aliases types'

(* Generated names that are reserved in a target language. The keyword replacements are applied
   before this pass, so they do not cover the generated names. *)
let reserved = Set.of_list ["f16"; "f32"; "f64"; "f80"; "f128"]

type names =
  { top_level: Set.t  (** the names defined at the top level *)
  ; taken: Set.t
        (** names a generated one must avoid: the top level ones, the called ones, which include
            builtins without definition, and the parameters, since a kept one would shadow a
            function with the same name *)
  }

let collectNames (prog : prog) : names =
  let topLevelName (s : top_stmt) =
    match s.top with
    | TopFunction ({name; _}, _) | TopExternal ({name; _}, _) | TopConstant (name, _, _, _, _) ->
        name
    | TopType {path; _} | TopAlias {path; _} ->
        path
  in
  let top_level = Set.of_list (CCList.map topLevelName prog) in
  let taken = ref (Set.union top_level reserved) in
  let add name = taken := Set.add name !taken in
  let exp =
    Core.Mapper.make (fun _env state (e : exp) ->
        (match e.e with ECall {path; _} -> add path | _ -> ()) ;
        (state, e) )
  in
  let param = Core.Mapper.make (fun _env state (p : param) -> add p.name ; (state, p)) in
  let _ = Core.Mapper.prog {Core.Mapper.identity with exp; param} () (Core.Mapper.defaultState ()) prog in
  {top_level; taken= !taken}

(* Returns the first name [prefix ^ n], counting from [next], that is free. *)
let rec freshName (is_free : string -> bool) (prefix : string) (next : int) : string * int =
  let name = prefix ^ string_of_int next in
  if is_free name then (name, next + 1) else freshName is_free prefix (next + 1)

let assignNamesWhere (is_free : string -> bool) (prefix : string) (names : string list) : string Map.t =
  let assign (map, next) name =
    if Map.mem name map then (map, next)
    else
      let new_name, next = freshName is_free prefix next in
      (Map.add name new_name map, next)
  in
  fst (CCList.fold_left assign (Map.empty, 0) names)

let assignNames (taken : Set.t) (prefix : string) (names : string list) : string Map.t =
  assignNamesWhere (fun name -> not (Set.mem name taken)) prefix names

let companionSuffixes = ["_init"; "_alloc"]

let typeRenames (prog : prog) (kept_types : Set.t) (taken : Set.t) : string Map.t =
  let renamed =
    CCList.filter_map
      (fun (s : top_stmt) ->
        match s.top with
        | (TopType {path; _} | TopAlias {path; _}) when not (Set.mem path kept_types) ->
            Some path
        | TopType _ | TopAlias _ | TopFunction _ | TopExternal _ | TopConstant _ ->
            None )
      prog
  in
  let is_free name = CCList.for_all (fun suffix -> not (Set.mem (name ^ suffix) taken)) ("" :: companionSuffixes) in
  assignNamesWhere is_free "t" renamed

(* The name of the type a function is a companion of, if it is one. *)
let companionType (types : Set.t) (name : string) : (string * string) option =
  CCList.find_map
    (fun suffix ->
      match CCString.chop_suffix ~suf:suffix name with
      | Some type_name when Set.mem type_name types ->
          Some (type_name, suffix)
      | _ ->
          None )
    companionSuffixes

type function_naming = Keep | Companion of string | Generate

let functionNaming ~(kept_types : Set.t) ~(all_types : Set.t) (types : string Map.t) (def : function_def) :
    function_naming =
  let takes_kept_context = match contextType def with Some path -> Set.mem path kept_types | None -> false in
  if def.info.is_root || takes_kept_context then Keep
  else
    match companionType all_types def.name with
    | Some (type_name, suffix) -> (
      match Map.find_opt type_name types with Some new_type -> Companion (new_type ^ suffix) | None -> Keep )
    | None ->
        Generate

let functionRenames (prog : prog) (kept_types : Set.t) (types : string Map.t) (taken : Set.t) : string Map.t =
  let all_types = Set.union kept_types (Set.of_list (Map.keys types |> CCList.of_iter)) in
  let classify (companions, generated) (s : top_stmt) =
    match s.top with
    | TopFunction (def, _) -> (
      match functionNaming ~kept_types ~all_types types def with
      | Keep ->
          (companions, generated)
      | Companion new_name ->
          (Map.add def.name new_name companions, generated)
      | Generate ->
          (companions, def.name :: generated) )
    | TopType _ | TopAlias _ | TopExternal _ | TopConstant _ ->
        (companions, generated)
  in
  let companions, generated = CCList.fold_left classify (Map.empty, []) prog in
  let generated = assignNames taken "f" (CCList.rev generated) in
  Map.union (fun _ a _ -> Some a) companions generated

let declaredVariables (body : stmt) : string list =
  let names = ref [] in
  let dexp =
    Core.Mapper.make (fun _env state (d : dexp) ->
        let (DId (name, _)) = d.d in
        names := name :: !names ;
        (state, d) )
  in
  let _ = Core.Mapper.stmt {Core.Mapper.identity with dexp} () (Core.Mapper.defaultState ()) body in
  CCList.rev !names

(* The local variables of a function and, when its name is replaced too, its parameters. Names that
   coincide with a top level one are left alone, since a use of them could refer to it. *)
let variableRenames (names : names) ~(rename_params : bool) (def : function_def) (body : stmt) : string Map.t =
  let params = if rename_params then CCList.map (fun (p : param) -> p.name) def.args else [] in
  let candidates = params @ declaredVariables body in
  let renamable name = name <> "_ctx" && not (Set.mem name names.top_level) in
  let candidates = CCList.filter renamable candidates in
  assignNames names.taken "v" candidates

let memberNames (members : member list) : string list = CCList.map (fun (name, _, _, _) -> name) members

(* Member names are renamed the same way in every struct, so an access can be renamed without
   knowing the type it reads from. A name used by a kept struct is therefore kept everywhere. *)
let memberRenames (prog : prog) (kept_types : Set.t) (taken : Set.t) : string Map.t =
  let kept, others =
    CCList.fold_left
      (fun (kept, others) (s : top_stmt) ->
        match s.top with
        | TopType {path; members} when Set.mem path kept_types ->
            (Set.add_list kept (memberNames members), others)
        | TopType {members; _} ->
            (kept, others @ memberNames members)
        | TopFunction _ | TopExternal _ | TopAlias _ | TopConstant _ ->
            (kept, others) )
      (Set.empty, []) prog
  in
  let renamed = CCList.filter (fun name -> not (Set.mem name kept)) others in
  let taken = Set.union taken (Set.add_list kept others) in
  assignNames taken "m" renamed

let renameWith (map : string Map.t) (name : string) : string =
  match Map.find_opt name map with Some new_name -> new_name | None -> name

let mapper : (env, unit) Core.Mapper.mapper =
  let exp =
    Core.Mapper.make (fun env state (e : exp) ->
        match e.e with
        | EId name ->
            (state, {e with e= EId (renameWith env.variables name)})
        | ECall {path; args} ->
            (state, {e with e= ECall {path= renameWith env.functions path; args}})
        | EMember (e1, name) ->
            (state, {e with e= EMember (e1, renameWith env.members name)})
        | ERecord {path; elems} ->
            let elems = CCList.map (fun (name, e) -> (renameWith env.members name, e)) elems in
            (state, {e with e= ERecord {path= renameWith env.types path; elems}})
        | _ ->
            (state, e) )
  in
  let lexp =
    Core.Mapper.make (fun env state (l : lexp) ->
        match l.l with
        | LId name ->
            (state, {l with l= LId (renameWith env.variables name)})
        | LMember (l1, name) ->
            (state, {l with l= LMember (l1, renameWith env.members name)})
        | _ ->
            (state, l) )
  in
  let member =
    Core.Mapper.make (fun env state ((name, t, tags, loc) : member) ->
        (state, (renameWith env.members name, t, tags, loc)) )
  in
  let struct_descr =
    Core.Mapper.make (fun env state (descr : struct_descr) ->
        (state, {descr with path= renameWith env.types descr.path}) )
  in
  let dexp =
    Core.Mapper.make (fun env state (d : dexp) ->
        let (DId (name, dim)) = d.d in
        (state, {d with d= DId (renameWith env.variables name, dim)}) )
  in
  let param = Core.Mapper.make (fun env state (p : param) -> (state, {p with name= renameWith env.variables p.name})) in
  let function_def =
    Core.Mapper.make (fun env state (def : function_def) -> (state, {def with name= renameWith env.functions def.name}))
  in
  {Core.Mapper.identity with exp; lexp; dexp; param; member; struct_descr; function_def}

let renameTopStmt (names : names) (env : env) (s : top_stmt) : top_stmt list =
  match s.top with
  | TopAlias {path; alias_of} ->
      (* The mapper does not visit the names of an alias *)
      [{s with top= TopAlias {path= renameWith env.types path; alias_of= renameWith env.types alias_of}}]
  | TopFunction (def, body) ->
      let variables = variableRenames names ~rename_params:(Map.mem def.name env.functions) def body in
      snd (Core.Mapper.top_stmt mapper {env with variables} (Core.Mapper.defaultState ()) s)
  | TopExternal _ | TopType _ | TopConstant _ ->
      snd (Core.Mapper.top_stmt mapper env (Core.Mapper.defaultState ()) s)

let hasRoots (prog : prog) : bool =
  CCList.exists (fun (s : top_stmt) -> match s.top with TopFunction (def, _) -> def.info.is_root | _ -> false) prog

(* Replaces the internal names of the program with short generated ones. Without roots every
   function would be renamed, leaving nothing callable, so at least one root is required. *)
let run (prog : prog) : prog =
  if not (hasRoots prog) then
    Util.Error.raiseErrorMsg "The flag -shorten requires at least one root function, given with the flag -root."
  else
    let names = collectNames prog in
    let kept_types = closeOverAliases (aliases prog) (rootContextTypes prog) in
    let types = typeRenames prog kept_types names.taken in
    let functions = functionRenames prog kept_types types names.taken in
    let members = memberRenames prog kept_types names.taken in
    let env = {functions; variables= Map.empty; members; types} in
    CCList.concat_map (renameTopStmt names env) prog

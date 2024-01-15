open Prog
module TypeTable = CCMap.Make (String)
module TypeSet = CCSet.Make (String)

let buffer_name = "CustomBuffer"
let typdescr_name = "CustomTypeDescr"
let push_block_header = "push_block_header"
let push_array = "push_array"
let update_size = "update_size"
let push_int = "push_int"
let push_float = "push_float"
let push_string = "push_string"
let search_field_name = "search_field_name"

let rec getTypeDep (t : type_) =
  match t with
  | { t = TStruct { path; _ }; _ } -> [ path ]
  | { t = TArray (_, t); _ } -> getTypeDep t
  | { t = TTuple elems; _ } -> List.flatten (List.map getTypeDep elems)
  | _ -> []


let saveAnyMember members = List.exists (fun (_, _, tags, _) -> Pparser.Ptags.has tags "save") members

let typeDependencyTable (prog : prog) =
  let rec loop table visited prog =
    match prog with
    | [] -> table
    | { top = TopAlias { path; alias_of }; _ } :: t ->
      if TypeSet.mem path visited then
        loop table visited t
      else (
        let visited = TypeSet.add path visited in
        let table = TypeTable.add path (None, false, [ alias_of ]) table in
        loop table visited t)
    | { top = TopType ({ path; members; _ } as h); _ } :: t ->
      if TypeSet.mem path visited then
        loop table visited t
      else (
        let deps =
          members
          |> List.map (fun (_, (t : type_), _, _) -> getTypeDep t)
          |> List.flatten
          |> TypeSet.of_list
          |> TypeSet.to_list
        in
        let save = saveAnyMember members in
        let visited = TypeSet.add path visited in
        let table = TypeTable.add path (Some h, save, deps) table in
        loop table visited t)
    | _ :: t -> loop table visited t
  in
  loop TypeTable.empty TypeSet.empty prog


(* check if the type or any of it's dependencies has the "save" tag *)
let rec shoulSave table path =
  match TypeTable.find_opt path table with
  | None -> false
  | Some (_, save, deps) ->
    if save then
      save
    else
      List.exists (shoulSave table) deps


let shouldSaveType table t = getTypeDep t |> List.exists (shoulSave table)

let propagateSaveTag (table : (struct_descr option * bool * string list) TypeTable.t) =
  let tick = ref 0 in
  let nextTick save =
    if save then (
      let i = !tick in
      incr tick;
      i)
    else
      -1
  in
  TypeTable.map
    (fun ((s : struct_descr option), save, deps) ->
      match s with
      (* type alias, check if the alias type needs to be saved *)
      | None ->
        let save = List.exists (shoulSave table) deps in
        None, save, deps, nextTick save
      | Some s ->
        let save, members_rev =
          List.fold_left
            (fun (save, members) (name, t, tags, loc) ->
              match Pparser.Ptags.getArguments tags "save" with
              | None ->
                (* the member has no "save" tag, if the type of the member has it, add the tag to the member *)
                if shouldSaveType table t then (
                  let tag =
                    Pparser.Ptags.
                      { g = TagCall { name = "save"; args = [ "name", { g = TagString name; loc }, loc ] }; loc }
                  in
                  true, (name, t, tag :: tags, loc) :: members)
                else
                  save, (name, t, tags, loc) :: members
              | Some [] ->
                (* the member has the tag but it's empty, fill name automatically *)
                let tags = Pparser.Ptags.setArgument tags "save" "name" { g = TagString name; loc } in
                true, (name, t, tags, loc) :: members
              | Some _ ->
                (* the member has the save tag, mark the type with "save" *)
                let save = true in
                save, (name, t, tags, loc) :: members)
            (save, [])
            s.members
        in
        let members = List.rev members_rev in
        Some { s with members }, save, deps, nextTick save)
    table


let getTypeNameString (t : type_) =
  match t with
  | { t = TStruct { path; _ }; _ } -> path
  | _ -> failwith "This is not a struct"


let rec getAllStructTypes (t : type_) =
  match t with
  | { t = TStruct { path; members }; _ } -> path :: CCList.flat_map (fun (_, t, _, _) -> getAllStructTypes t) members
  | { t = TArray (_, t); _ } -> getAllStructTypes t
  | { t = TTuple elems; _ } -> CCList.flat_map (fun t -> getAllStructTypes t) elems
  | _ -> []


let getTypeNameStringExp (t : type_) =
  let string_type = { t = TString; loc = t.loc } in
  { e = EString (getTypeNameString t); t = string_type; loc = t.loc }


let collectFullySavedTypes (stmts : top_stmt list) =
  let rec loop set =
    let new_set =
      List.fold_left
        (fun set stmt ->
          match stmt with
          | { top = TopType { members; _ }; _ } ->
            TypeSet.add_list
              set
              (List.flatten
               @@ List.filter_map
                    (fun (_, (t : type_), tags, _) ->
                      if Pparser.Ptags.has tags "save" then
                        Some (getAllStructTypes t)
                      else
                        None)
                    members)
          | _ -> set)
        set
        stmts
    in
    if TypeSet.cardinal set <> TypeSet.cardinal new_set then loop new_set else new_set
  in
  loop TypeSet.empty


let tagAllMembers fully_saved (stmts : top_stmt list) =
  List.map
    (fun stmt ->
      match stmt with
      | { top = TopType { path; members }; _ } when TypeSet.mem path fully_saved ->
        let members =
          List.map
            (fun (name, t, tags, loc) ->
              if Pparser.Ptags.has tags "save" then
                name, t, tags, loc
              else (
                let tag = Pparser.Ptags.{ g = TagId "save"; loc } in
                name, t, tag :: tags, loc))
            members
        in
        { stmt with top = TopType { path; members } }
      | _ -> stmt)
    stmts


let serializerForType (t : type_) =
  match t with
  | { t = TInt | TFix16 | TBool; _ } -> push_int
  | { t = TReal; _ } -> push_float
  | { t = TString; _ } -> push_string
  | { t = TVoid _; _ } -> failwith "serializerForType: void"
  | { t = TArray _; _ } -> failwith "serializerForType: array"
  | { t = TTuple _; _ } -> failwith "serializerForType: tuple"
  | { t = TStruct { path; _ }; _ } -> path ^ "_serialize_data"


let callPush push this_type member t loc =
  let buffer_type = { t = TStruct { path = buffer_name; members = [] }; loc } in
  let index = C.eid "index" C.int_t in
  let buffer = C.eid "buffer" buffer_type in
  let ectx = C.eid "_ctx" this_type in
  Some
    { s =
        StmtBind
          ( { l = LId "index"; loc; t = C.int_t }
          , { e = ECall { path = push; args = [ buffer; index; { e = EMember (ectx, member); t; loc } ] }
            ; t = C.void_t
            ; loc
            } )
    ; loc
    }


let createSerializer table (stmt : top_stmt) =
  match stmt with
  | { top = TopType { path; _ }; loc } ->
    let s, save, _, _ = TypeTable.find path table in
    if save && s <> None then (
      let s = Option.get s in
      let name = s.path ^ "_serialize_data" in
      let this_type = { t = TStruct s; loc } in
      let buffer_type = { t = TStruct { path = buffer_name; members = [] }; loc } in
      let start = C.eid "start" C.int_t in
      let index = C.eid "index" C.int_t in
      let lindex = C.lid "index" C.int_t in
      let buffer = C.eid "buffer" buffer_type in
      let start_decl = C.sdecl "start" C.int_t in
      let start_bind = C.sbind (C.lid "start" C.int_t) index in
      (* push the object header *)
      let push_block_header = C.sbind lindex (C.ecall push_block_header [ buffer; index ] C.void_t) in
      (* call serialization for each member *)
      let member_stmts =
        let tick = ref 0 in
        CCList.filter_map
          (fun (name, (t : type_), tags, loc) ->
            if Pparser.Ptags.has tags "save" then (
              match t with
              | { t = TInt | TFix16; _ } -> callPush push_int this_type name t loc
              | { t = TReal; _ } -> callPush push_float this_type name t loc
              | { t = TString; _ } -> callPush push_string this_type name t loc
              | { t = TBool; _ } -> callPush push_int this_type name t loc
              | { t = TStruct { path; _ }; _ } -> callPush (path ^ "_serialize_data") this_type name t loc
              | { t = TVoid _; _ } -> None
              | { t = TArray (Some size, at); _ } ->
                let n = !tick in
                incr tick;
                let iter_name = "i_" ^ string_of_int n in
                let iter_exp = C.eid iter_name C.int_t in
                let iter_lexp = C.lid iter_name C.int_t in
                let iter_decl = C.sdecl iter_name C.int_t in
                let start_name = "start_" ^ string_of_int n in
                let start_exp = C.eid start_name C.int_t in
                let start_lexp = C.lid start_name C.int_t in
                let start_decl = C.sdecl start_name C.int_t in
                let start_bind = C.sbind start_lexp index in
                let iter_incr = C.sbind iter_lexp (C.eadd iter_exp (C.eint 1)) in
                let update_array =
                  C.sbind_wild (C.ecall "update_size" [ buffer; start_exp; C.esub index start_exp ] C.void_t)
                in
                let push_array = C.sbind lindex (C.ecall push_array [ buffer; index; C.eint size ] C.void_t) in
                let call =
                  C.sbind
                    lindex
                    (C.ecall
                       (serializerForType at)
                       [ buffer; index; C.eindex (C.emember (C.eid "_ctx" this_type) name t) iter_exp at ]
                       C.void_t)
                in
                let body = { s = StmtBlock [ call; iter_incr ]; loc } in
                let loop = { s = StmtWhile (C.elt iter_exp (C.eint size), body); loc } in
                Some { s = StmtBlock [ start_decl; start_bind; iter_decl; push_array; loop; update_array ]; loc }
              | { t = TArray (None, _t); _ } -> failwith "Serializing array with unknonw size"
              | { t = TTuple _; _ } -> failwith "serialization of tuple")
            else
              None)
          s.members
      in
      (* statement to update the size *)
      let update_size =
        { s =
            StmtBind
              ( { l = LWild; loc; t = C.void_t }
              , { e =
                    ECall
                      { path = update_size
                      ; args = [ buffer; start; { e = EOp (OpSub, index, start); t = C.int_t; loc } ]
                      }
                ; t = C.void_t
                ; loc
                } )
        ; loc
        }
      in
      let return = { s = StmtReturn index; loc } in
      let body =
        { s = StmtBlock ([ start_decl; start_bind; push_block_header ] @ member_stmts @ [ update_size; return ]); loc }
      in
      let args, t =
        ( [ "buffer", buffer_type, loc; "index", C.int_t, loc; "_ctx", this_type, loc ]
        , ([ buffer_type; C.int_t; this_type ], C.int_t) )
      in
      Some { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc })
    else
      None
  | { top = TopAlias { path; alias_of; _ }; loc } ->
    let _, save, _, _ = TypeTable.find path table in
    if save then (
      let name = path ^ "_serialize_data" in
      let name_alias = alias_of ^ "_serialize_data" in
      let this_type = { t = TStruct { path; members = [] }; loc } in
      let buffer_type = { t = TStruct { path = buffer_name; members = [] }; loc } in
      let ectx = { e = EId "_ctx"; t = this_type; loc } in
      let index = C.eid "index" C.int_t in
      let buffer = C.eid "buffer" buffer_type in
      let call_serializer =
        { s = StmtReturn { e = ECall { path = name_alias; args = [ buffer; index; ectx ] }; t = C.int_t; loc }; loc }
      in
      let body = { s = StmtBlock [ call_serializer ]; loc } in
      let args, t =
        ( [ "buffer", buffer_type, loc; "index", C.int_t, loc; "_ctx", this_type, loc ]
        , ([ buffer_type; C.int_t; this_type ], C.int_t) )
      in
      Some { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc })
    else
      None
  | _ -> None


let createDeserializer table (stmt : top_stmt) =
  match stmt with
  | { top = TopType { path; _ }; loc } ->
    let s, save, _, _ = TypeTable.find path table in
    if save && s <> None then (
      let s = Option.get s in
      let name = s.path ^ "_deserialize_data" in
      let this_type = { t = TStruct s; loc } in
      let buffer_type = { t = TStruct { path = buffer_name; members = [] }; loc } in
      let typedescr_type = { t = TStruct { path = typdescr_name; members = [] }; loc } in
      let bool_type = { t = TBool; loc } in
      let string_type = { t = TString; loc } in
      let typedescr = { e = EId "type_descr"; t = typedescr_type; loc } in
      let index = C.eid "index" C.int_t in
      let buffer = C.eid "buffer" buffer_type in
      let lctx = { l = LId "_ctx"; t = this_type; loc } in
      let ectx = { e = EId "_ctx"; t = this_type; loc } in
      let member_stmts =
        let tick = ref 0 in
        CCList.filter_map
          (fun (name, (t : type_), tags, loc) ->
            let search_field =
              { e =
                  ECall
                    { path = search_field_name
                    ; args = [ buffer; typedescr; index; { e = EString name; t = string_type; loc } ]
                    }
              ; t = C.int_t
              ; loc
              }
            in
            let field_index = { e = EId "field_index"; t = C.int_t; loc } in
            let search_stmt = { s = StmtBind ({ l = LId "field_index"; t = C.int_t; loc }, search_field); loc } in
            let found_index stmts =
              { s =
                  StmtIf
                    ({ e = EOp (OpGe, field_index, C.eint 0); t = bool_type; loc }, { s = StmtBlock stmts; loc }, None)
              ; loc
              }
            in
            let callDeserializer path =
              { s =
                  StmtBind
                    ( { l = LMember (lctx, name); t; loc }
                    , { e = ECall { path; args = [ buffer; field_index ] }; t; loc } )
              ; loc
              }
            in
            if Pparser.Ptags.has tags "save" then (
              match t with
              | { t = TInt | TFix16; _ } ->
                Some { s = StmtBlock [ search_stmt; found_index [ callDeserializer "deserialize_int" ] ]; loc }
              | { t = TReal; _ } ->
                Some { s = StmtBlock [ search_stmt; found_index [ callDeserializer "deserialize_float" ] ]; loc }
              | { t = TString; _ } ->
                Some { s = StmtBlock [ search_stmt; found_index [ callDeserializer "deserialize_string" ] ]; loc }
              | { t = TBool; _ } ->
                Some { s = StmtBlock [ search_stmt; found_index [ callDeserializer "deserialize_int" ] ]; loc }
              | { t = TStruct { path = _; _ }; _ } ->
                let field_descr = "field_descr_" ^ string_of_int !tick in
                let () = incr tick in
                let decl = C.sdecl field_descr typedescr_type in
                let search_type =
                  C.sbind
                    (C.lid field_descr typedescr_type)
                    (C.ecall "search_type_description" [ buffer; getTypeNameStringExp t ] typedescr_type)
                in
                let call_deserializer =
                  C.sbind_wild
                    (C.ecall
                       (getTypeNameString t ^ "_deserialize_data")
                       [ buffer; C.eid field_descr typedescr_type; field_index; C.emember ectx name t ]
                       C.void_t)
                in
                Some (C.sblock [ search_stmt; found_index [ decl; search_type; call_deserializer ] ])
              | { t = TVoid _; _ } -> None
              | { t = TArray (Some n, at); _ } ->
                let field_descr = "field_descr_" ^ string_of_int !tick in
                let () = incr tick in
                let decl = C.sdecl field_descr typedescr_type in
                let search_type =
                  C.sbind
                    (C.lid field_descr typedescr_type)
                    (C.ecall "search_type_description" [ buffer; getTypeNameStringExp at ] typedescr_type)
                in
                let iter_name = "i_" ^ string_of_int !tick in
                let () = incr tick in
                let iter_decl = C.sdecl_bind iter_name (C.eint 0) C.int_t in
                let iter_id = C.eid iter_name C.int_t in
                let call_deserializer =
                  C.sbind_wild
                    (C.ecall
                       (getTypeNameString at ^ "_deserialize_data")
                       [ buffer
                       ; C.eid field_descr typedescr_type
                       ; field_index
                       ; C.eindex (C.emember ectx name t) iter_id at
                       ]
                       C.void_t)
                in
                let skip_size =
                  C.sbind (C.lid "field_index" C.int_t) (C.ecall "first_array_element" [ buffer; field_index ] C.int_t)
                in
                let cond = C.elt iter_id (C.eint n) in
                let body =
                  let next_element =
                    C.sbind (C.lid "field_index" C.int_t) (C.ecall "next_object" [ buffer; field_index ] C.int_t)
                  in
                  C.sblock
                    [ call_deserializer; C.sbind (C.lid iter_name C.int_t) (C.eadd iter_id (C.eint 1)); next_element ]
                in
                let loop = C.swhile cond body in
                Some (C.sblock [ decl; search_type; search_stmt; found_index ((skip_size :: iter_decl) @ [ loop ]) ])
              | { t = TArray _; _ } -> failwith "deserialization of array with unknow dimensions"
              | { t = TTuple _; _ } -> failwith "deserialization of tuple")
            else
              None)
          s.members
      in
      let field_decl = { s = StmtDecl { d = DId ("field_index", None); t = C.int_t; loc }; loc } in
      let body = { s = StmtBlock ([ field_decl ] @ member_stmts); loc } in
      let args, t =
        ( [ "buffer", buffer_type, loc
          ; "type_descr", typedescr_type, loc
          ; "index", C.int_t, loc
          ; "_ctx", this_type, loc
          ]
        , ([ buffer_type; C.int_t; this_type ], C.void_t) )
      in
      Some { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc })
    else
      None
  (*
     | { top = TopAlias { path; alias_of; _ }; loc } ->
      let name = path ^ "_serialize_data" in
      let name_alias = alias_of ^ "_serialize_data" in
      let this_type = { t = TStruct { path; members = [] }; loc } in
      let buffer_type = { t = TStruct { path = buffer_name; members = [] }; loc } in
      let C.int_t = { t = TInt; loc } in
      let ectx = { e = EId "_ctx"; t = this_type; loc } in
      let index = C.eid "index" C.int_t in
      let buffer = C.eid "buffer" buffer_type in
      let call_serializer =
        { s = StmtReturn { e = ECall { path = name_alias; args = [ buffer; index; ectx ] }; t = C.int_t; loc }; loc }
      in
      let body = { s = StmtBlock [ call_serializer ]; loc } in
      let args, t =
        ( [ "buffer", buffer_type, loc; "index", C.int_t, loc; "_ctx", this_type, loc ]
        , ([ buffer_type; C.int_t; this_type ], C.int_t) )
      in
      Some { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc }
  *)
  | _ -> None


let createTypeDescriptor n_types table (stmt : top_stmt) =
  match stmt with
  | { top = TopType { path; _ }; loc } ->
    let s, save, deps, n_type = TypeTable.find path table in
    if save && s <> None then (
      let s = Option.get s in
      let name = path ^ "_serialize_type_descr" in
      let buffer_type = { t = TStruct { path = buffer_name; members = [] }; loc } in
      let string_type = { t = TString; loc } in
      let index = C.eid "index" C.int_t in
      let lindex = C.lid "index" C.int_t in
      let buffer = C.eid "buffer" buffer_type in
      let string_array_type n : type_ = { t = TArray (Some n, string_type); loc } in
      let members =
        CCList.filter_map
          (fun (name, (_t : type_), tags, loc) ->
            if Pparser.Ptags.has tags "save" then
              Some { e = EString name; t = string_type; loc }
            else
              None)
          s.members
      in
      let marks_type = C.array_t ~dim:n_types C.bool_t in
      let marks = C.eid "marks" marks_type in
      (* mark/ serialize all the dependencies all*)
      let n = List.length members in
      let members = { e = EArray members; t = string_array_type n; loc } in
      let type_name = { e = EString path; t = string_type; loc } in
      let call = C.ecall "serialize_type_descr" [ buffer; index; type_name; members ] C.int_t in
      let return = { s = StmtReturn call; loc } in
      let mark_me = C.sbind (C.lindex (C.lid "marks" marks_type) (C.eint n_type) C.bool_t) (C.ebool true) in
      let shortcut = C.sif (C.eindex marks (C.eint n_type) C.bool_t) (C.sreturn index) None in
      let call_deps =
        List.filter_map
          (fun name ->
            match TypeTable.find_opt name table with
            | Some (_, true, _, _) ->
              let path = name ^ "_serialize_type_descr" in
              Some (C.sbind lindex (C.ecall path [ buffer; index; marks ] C.int_t))
            | _ -> None)
          deps
      in
      let body = C.sblock ([ shortcut; mark_me ] @ call_deps @ [ return ]) in
      let args, t =
        ( [ "buffer", buffer_type, loc; "index", C.int_t, loc; "marks", marks_type, loc ]
        , ([ buffer_type; C.int_t ], C.int_t) )
      in
      Some { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc })
    else
      None
  | _ -> None


let createSerializers (prog : prog) =
  let fully_saved = collectFullySavedTypes prog in
  let prog = tagAllMembers fully_saved prog in
  let table = typeDependencyTable prog in
  (*
     let () =
    TypeTable.iter
      (fun key (s, save, deps) ->
        let () = print_endline (key ^ " = " ^ if save then "true" else "false") in
        let () = print_endline (" : " ^ String.concat ", " deps) in
        let members_str =
          match s with
          | Some s -> Pla.print @@ Pla.indent @@ Pla.map_sep_all Pla.newline Print.print_member s.members
          | None -> ""
        in
        print_endline members_str)
      table
  in
  *)
  let table = propagateSaveTag table in
  let n_types = TypeTable.fold (fun _ (_, _, _, n) acc -> if n >= 0 then acc + 1 else acc) table 0 in
  let serializers = CCList.filter_map (fun stmt -> createSerializer table stmt) prog in
  let deserializers = CCList.filter_map (fun stmt -> createDeserializer table stmt) prog in
  let descriptors = CCList.filter_map (fun stmt -> createTypeDescriptor n_types table stmt) prog in
  serializers @ descriptors @ deserializers

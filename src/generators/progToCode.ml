(*
   The MIT License (MIT)

   Copyright (c) 2014 Leonardo Laguna Ruiz

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

open Prog
open Code
open Common
open Args

let unit_typ = CTSimple "unit"

type parameters =
   { repl : Replacements.t
   ; code : Args.code
   ; cleanup : bool
   ; shorten : bool
   ; used : used_function Maps.IdMap.t
   ; functions : NameTable.t
   ; variables : NameTable.t
   ; output_prefix : string option
   }

type lhs_kind =
   | Declaration
   | Other

let makeSingleBlock stmts =
   match stmts with
   | [] -> CSBlock []
   | [ stmt ] -> stmt
   | _ -> CSBlock stmts


let rec tryMakeSwitchLoop id cases next =
   match next with
   | None -> Some (List.rev cases, None)
   | Some (CSIf (CEOp ("==", [ nid; (CEInt _ as i) ], _), stmt, next)) ->
      if Code.compare_cexp id nid = 0 then
         tryMakeSwitchLoop id ((i, stmt) :: cases) next
      else
         None
   | Some def -> Some (List.rev cases, Some def)


let tryMakeSwitch e =
   match e with
   | CSIf (CEOp ("==", [ var; (CEInt _ as i) ], _), stmt, next) ->
      begin
         match tryMakeSwitchLoop var [ i, stmt ] next with
         | Some ((_ :: _ :: _ as cases), def) -> CSSwitch (var, cases, def)
         | _ -> e
      end
   | _ -> e


let rec makeSwitch p block =
   match block with
   | CSBlock stmts ->
      let stmts = makeSwitchList p stmts in
      makeSingleBlock stmts
   | CSIf _ -> tryMakeSwitch block
   | CSWhile (cond, body) -> CSWhile (cond, makeSwitch p body)
   | _ -> block


and makeSwitchList p stmts =
   match stmts with
   | [] -> []
   | h :: t -> makeSwitch p h :: makeSwitchList p t


let convertId (p : parameters) (id : Id.t) : string = String.concat "_" id |> Replacements.getKeyword p.repl

let applyOutputPrefix (p : parameters) name =
   match p.output_prefix with
   | None -> name
   | Some p -> p ^ name


let applyOutputPrefixToId (p : parameters) id =
   match p.output_prefix with
   | None -> id
   | Some p ->
      match id with
      | [ _ ] -> id
      | [ m; n ] -> [ p ^ m; n ]
      | _ -> failwith "applyOutputPrefixToId"


let convertFunctionName (p : parameters) (id : Id.t) : string =
   let name = String.concat "_" id |> Replacements.getKeyword p.repl in
   applyOutputPrefix
      p
      ( if p.shorten then
           match Maps.IdMap.find_opt id p.used with
           | Some (Keep Root) -> NameTable.registerName p.functions name
           | _ -> NameTable.generateName p.functions name
        else
           name )


let convertVarId (p : parameters) (is_val : lhs_kind) (id : Id.t) : string list =
   let should_prefix =
      match id with
      | [ m; _ ] -> m.[0] <> '_' && String.capitalize_ascii m = m
      | _ -> false
   in
   let name = List.map (Replacements.getKeyword p.repl) id in
   let name =
      if p.shorten then
         if is_val = Declaration then
            List.map (NameTable.generateName p.variables) name
         else
            List.map (NameTable.getOrRegister p.variables) name
      else
         name
   in
   if should_prefix then
      [ applyOutputPrefix p (String.concat "_" name) ]
   else
      name


let convertExpId (p : parameters) (is_val : lhs_kind) (id : Id.t) =
   let l = convertVarId p is_val id in
   let rec loop l =
      match l with
      | [] -> failwith ""
      | [ n ] -> CEVar (n, CTSimple "none")
      | h :: t -> CEAccess (loop t, h)
   in
   loop (List.rev l)


let convertSingleVarId (p : parameters) (id : Id.t) : string =
   match id with
   | [ name ] ->
      let name = Replacements.getKeyword p.repl name in
      if p.shorten then
         NameTable.getOrRegister p.variables name
      else
         name
   | _ -> failwith "ProgToCode.convertSingleVarId: this should be a single identifier"


let rec convertType (p : parameters) (tp : Typ.t) : type_descr =
   match !tp with
   | Typ.TId ([ typ ], _) ->
      let new_type = Replacements.getType p.repl typ in
      CTSimple new_type
   | Typ.TId ([ m; typ ], _) ->
      let new_type = Replacements.getType p.repl typ in
      CTSimple (applyOutputPrefix p m ^ "_" ^ new_type)
   | Typ.TId _ -> failwith "invalid name"
   | Typ.TComposed ([ "tuple" ], _, _) ->
      let name = applyOutputPrefix p (Typ.getTupleName tp) in
      CTSimple name
   | Typ.TComposed ([ "array" ], [ kind; { contents = Typ.TInt (n, _) } ], _) ->
      let sub = convertType p kind in
      CTArray (sub, n)
   | Typ.TLink tp -> convertType p tp
   | Typ.TComposed (_, _, _)
   |Typ.TInt _
   |Typ.TArrow _
   |Typ.TUnbound _
   |Typ.TExpAlt _ ->
      failwith ("ProgToCode.convertType: unsupported type in c code generation: " ^ PrintProg.typeStr tp)


let convertTypeMakeTupleUnit (p : parameters) (tp : Typ.t) : type_descr =
   match !tp with
   | Typ.TComposed ([ "tuple" ], _, _) ->
      let new_type = Replacements.getType p.repl "unit" in
      CTSimple new_type
   | _ -> convertType p tp


let convertTypeList (p : parameters) (tp : Typ.t list) : type_descr list = List.map (convertType p) tp

let convertTypedId (p : parameters) (is_val : lhs_kind) (e : typed_id) : arg_type * string =
   match e with
   | SimpleId (_, _, _) -> failwith "ProgToCode.convertTypedId: everything should have types"
   | TypedId (id, typ, _, _) ->
      let ftype = Typ.first typ in
      let typ_c = convertType p ftype in
      let typ_ref = if Typ.isSimpleType ftype then Var typ_c else Ref typ_c in
      let ids = convertVarId p is_val id in
      typ_ref, String.concat "." ids


let getCast (p : parameters) (from_type : type_descr) (to_type : type_descr) : string =
   match from_type, to_type with
   | CTSimple from_t, CTSimple to_t -> Replacements.getCast p.repl from_t to_t
   | _ ->
      let from_str = show_type_descr from_type in
      let to_str = show_type_descr to_type in
      failwith ("ProgToCode.getCast: invalid casting of types " ^ from_str ^ " -> " ^ to_str)


let makeNestedCall (typ : type_descr) (name : string) (args : cexp list) : cexp =
   match args with
   | [] -> failwith "ProgToCode.makeNestedCall: invalid number of arguments"
   | [ _; _ ] -> CECall (name, args, typ)
   | h :: t -> List.fold_left (fun acc a -> CECall (name, [ acc; a ], typ)) h t


let convertOperator (p : parameters) (op : string) (typ : type_descr) (elems : cexp list) : cexp =
   match typ with
   | CTSimple typ_t ->
      begin
         match Replacements.getFunctionForOperator p.repl op typ_t with
         | Some fn -> makeNestedCall typ fn elems
         | None ->
            let new_op = Replacements.getOperator p.repl op typ_t in
            CEOp (new_op, elems, typ)
      end
   | _ -> CEOp (op, elems, typ)


let getFunctionSetType (elem_typs : type_descr list) : type_descr =
   match elem_typs with
   | [ _; _; v ] -> v
   | _ -> failwith "ProgToCode.getFunctionSetType: this is not a call to 'set'"


let getFunctionFirstArgType (elem_typs : type_descr list) : type_descr =
   match elem_typs with
   | v :: _ -> v
   | _ -> failwith "ProgToCode.getFunctionFirstArgType: this is not a call to 'split'"


let createWhileLoopInit size init lhs =
   match init with
   | CECall (name, [], descr) ->
      let il = CLId (CTSimple "int", [ "i" ]) in
      let i = CEVar ("i", CTSimple "int") in
      let decl = CSVar (il, None) in
      let init_decl = CSBind (il, CEInt 0) in
      let indexed = CEIndex (lhs, i, CTSimple "any") in
      let assign = CSBind (CLWild, CECall (name, [ indexed ], descr)) in
      let incr = CSBind (il, CEOp ("+", [ i; CEInt 1 ], CTSimple "int")) in
      let loop = CSWhile (CEOp ("<", [ i; size ], CTSimple "bool"), CSBlock [ assign; incr ]) in
      CSBlock [ decl; init_decl; loop ]
   | _ -> failwith "createWhileLoopInit"


let inlineArrayInit size init var =
   match var with
   | CEVar _
   |CEAccess _ ->
      createWhileLoopInit size init var
   | _ -> failwith "inlineArrayInit"


let getInitArrayFunction (p : parameters) (typ : type_descr) : string option =
   match typ with
   | CTSimple typ_t ->
      begin
         match Replacements.getArrayInit p.repl typ_t with
         | Some fn -> Some fn
         | _ -> None
      end
   | _ -> None


let getCopyArrayFunction (p : parameters) (typ : type_descr) : string =
   match typ with
   | CTSimple typ_t ->
      begin
         match Replacements.getArrayCopy p.repl typ_t with
         | Some fn -> fn
         | _ -> failwith ("getCopyArrayFunction: Invalid array type " ^ show_type_descr typ)
      end
   | _ -> failwith ("getCopyArrayFunction: Invalid array type " ^ show_type_descr typ)


let convertFunction (p : parameters) (name : Id.t) (typ : type_descr) (elems : cexp list) (elem_typs : type_descr list)
   : cexp =
   match name with
   (* For the function set we need to get the type based on one of the arguments *)
   | [ "set" ] ->
      begin
         match getFunctionSetType elem_typs with
         | CTSimple typ_t ->
            let fn = Replacements.getFunction p.repl "set" typ_t in
            CECall (fn, elems, typ)
         | _ -> failwith ("convertFunction: Invalid array type " ^ show_type_descr typ)
      end
   | [ fname ] ->
      begin
         match typ with
         | CTSimple typ_t ->
            let fn = Replacements.getFunction p.repl fname typ_t in
            CECall (fn, elems, typ)
         | _ -> CECall (convertId p name, elems, typ)
      end
   | _ -> CECall (convertFunctionName p name, elems, typ)


let attrType (p : parameters) (attr : attr) : type_descr =
   match attr.typ with
   | Some t -> convertType p t
   | _ -> failwith "ProgToCode.attrType: everything should have types"


let expType (p : parameters) (e : exp) : type_descr = GetAttr.fromExp e |> attrType p

let rec convertExp (p : parameters) (e : exp) : cexp =
   let typ = attrType p in
   match e with
   | PUnit _ -> CEEmpty
   | PBool (v, _) -> CEBool v
   | PInt (n, _) -> CEInt n
   | PString (s, _) -> CEString s
   | PReal (v, Float, _) ->
      let s = Replacements.getRealToString p.repl (Float.crop v) "real" in
      CEFloat (s, Float.crop v)
   | PReal (v, Fix16, _) ->
      let s = Replacements.getRealToString p.repl (Float.crop v) "fix16" in
      CEFloat (s, Float.crop v)
   | PId (([ _ ] as id), attr) -> CEVar (convertSingleVarId p id, typ attr)
   | PId (id, _) -> convertExpId p Other id
   | PIndex (e, index, attr) ->
      let e' = convertExp p e in
      let index' = convertExp p index in
      CEIndex (e', index', typ attr)
   | PArray (elems, attr) ->
      let elems' = convertExpArray p elems in
      CEArray (elems', typ attr)
   | PUnOp (op, e1, attr) ->
      let e1' = convertExp p e1 in
      CEUnOp (op, e1', typ attr)
   | POp (op, elems, attr) ->
      let elems' = convertExpList p elems in
      let typ = attrType p attr in
      convertOperator p op typ elems'
   | PCall (_, [ name ], [ arg ], attr) when name = "real" || name = "int" || name = "bool" || name = "fix16" ->
      let arg' = convertExp p arg in
      let from_typ = expType p arg in
      let to_type = attrType p attr in
      if from_typ <> to_type then
         CECall (getCast p from_typ to_type, [ arg' ], typ attr)
      else
         arg'
   | PCall (_, name, elems, attr) ->
      let elems' = convertExpList p elems in
      let elem_typ = List.map (expType p) elems in
      let ret_typ = attrType p attr in
      convertFunction p name ret_typ elems' elem_typ
   | PIf (cond, then_, else_, attr) ->
      let cond' = convertExp p cond in
      let then_' = convertExp p then_ in
      let else_' = convertExp p else_ in
      CEIf (cond', then_', else_', typ attr)
   | PGroup (e1, _) -> convertExp p e1
   | PTuple ([ e1 ], _) -> convertExp p e1
   | PTuple (elems, attr) ->
      let elems' =
         List.mapi
            (fun i a ->
                let a' = convertExp p a in
                "field_" ^ string_of_int i, a')
            elems
      in
      CETuple (elems', typ attr)
   | PAccess (e, n, _) ->
      let e' = convertExp p e in
      CEAccess (e', n)
   | PSeq _ -> failwith "ProgToCode.convertExp: Sequences are not yet supported for js"
   | PEmpty -> failwith "ProgToCode.convertExp: Empty expressions are not allowed"


and convertExpList (p : parameters) (e : exp list) : cexp list = List.map (convertExp p) e

and convertExpArray (p : parameters) (e : exp array) : cexp list = Array.map (convertExp p) e |> Array.to_list

and convertLhsExp (is_val : lhs_kind) (p : parameters) (e : lhs_exp) : clhsexp =
   match e with
   | LId (id, Some typ, _) ->
      let new_id = convertVarId p is_val id in
      let typl = convertType p typ in
      CLId (typl, new_id)
   | LId (_, None, _) -> failwith "ProgToCode.convertLhsExp: everything should have types"
   | LTyped (e1, _, _) -> convertLhsExp is_val p e1
   | LTuple (elems, _) ->
      let elems' = convertLhsExpList is_val p elems in
      CLTuple elems'
   | LWild _ -> CLWild
   | LGroup (e, _) -> convertLhsExp is_val p e
   | LIndex (_, None, _, _) -> failwith "ProgToCode.convertLhsExp: everything should have types"
   | LIndex (id, Some typ, index, _) ->
      let new_id = convertVarId p is_val id in
      let index = convertExp p index in
      let typl = convertType p typ in
      CLIndex (typl, new_id, index)


and convertLhsExpList (is_val : lhs_kind) (p : parameters) (lhsl : lhs_exp list) : clhsexp list =
   List.fold_left (fun acc lhs -> convertLhsExp is_val p lhs :: acc) [] lhsl |> List.rev


let getRecordField (name : lhs_exp) (index : int) (typ : Typ.t option) : lhs_exp =
   match name with
   | LId (id, _, attr) ->
      let field = "field_" ^ string_of_int index in
      LId (id @ [ field ], typ, { attr with typ })
   | _ -> failwith "ProgToCode.getRecordFiled: Invalid input"


let rec collectVarBind p stmts =
   match stmts with
   | [] -> []
   | CSVar (CLId (_, [ lhs ]), None) :: CSBind (CLId (_, [ lhs2 ]), rhs) :: CSIf (CEVar (cond, _), then_, else_) :: t
      when lhs = cond && lhs2 = cond && cond.[0] = '_' ->
      collectVarBind p (CSIf (rhs, then_, else_) :: t)
   | CSVar (lhs1, None) :: CSBind (lhs2, rhs) :: t when lhs1 = lhs2 && p.code <> CCode ->
      collectVarBind p (CSVar (lhs1, Some rhs) :: t)
   | CSVar (CLId (_, [ lhs ]), Some rhs) :: CSIf (CEVar (cond, _), then_, else_) :: t when lhs = cond && cond.[0] = '_'
      ->
      collectVarBind p (CSIf (rhs, then_, else_) :: t)
   | CSVar (lhs1, None) :: CSBind (lhs2, rhs) :: t -> CSVar (lhs1, None) :: CSBind (lhs2, rhs) :: collectVarBind p t
   | h :: t -> collectVarBindBlock p h :: collectVarBind p t


and collectVarBindBlock p block =
   match block with
   | CSBlock stmts ->
      let stmts = collectVarBind p stmts in
      makeSingleBlock stmts
   | CSIf (cond, then_, else_) -> CSIf (cond, collectVarBindBlock p then_, CCOpt.map (collectVarBindBlock p) else_)
   | CSWhile (cond, body) -> CSWhile (cond, collectVarBindBlock p body)
   | _ -> block


let collectStmt (p : parameters) stmt =
   match stmt with
   | CSBlock stmts -> makeSingleBlock (collectVarBind p stmts)
   | _ -> stmt


let isSmall exp =
   match exp with
   | PInt (i, _) when i < 1000 -> true
   | _ -> false


let removeFunction used =
   match used with
   | Keep _ -> false
   | _ -> true


let isRoot used =
   match used with
   | Keep Root
   |Used Root ->
      true
   | _ -> false


let refreshTable (p : parameters) = { p with variables = NameTable.make 1 }

let rec convertStmt (p : parameters) (s : stmt) : cstmt =
   match s with
   | StmtVal (lhs, None, _) ->
      let lhs' = convertLhsExp Declaration p lhs in
      CSVar (lhs', None)
   | StmtVal (_, _, attr) when p.cleanup && removeFunction attr.used -> CSEmpty
   | StmtVal (lhs, Some rhs, _) ->
      let lhs' = convertLhsExp Other p lhs in
      let rhs' = convertExp p rhs in
      CSVar (lhs', Some rhs')
   | StmtConst (_, _, attr) when p.cleanup && removeFunction attr.used -> CSEmpty
   | StmtConst (LId ([ name ], Some typ, _), rhs, _) ->
      let typl = convertType p typ in
      let lhs' = CLId (typl, [ applyOutputPrefix p name ]) in
      let rhs' = convertExp p rhs in
      CSConst (lhs', rhs')
   | StmtConst (_, _, _) -> failwith "ProgToCode.convertStmt: const should not have initializations"
   | StmtMem _ -> CSEmpty
   | StmtWhile (cond, stmt, _) ->
      let cond' = convertExp p cond in
      let stmt' = convertStmt p stmt in
      (* the env is ignored *)
      CSWhile (cond', stmt')
   | StmtReturn (e1, _) ->
      let e1' = convertExp p e1 in
      CSReturn e1'
   | StmtIf (cond, then_, None, _) ->
      let cond' = convertExp p cond in
      let then_' = convertStmt p then_ in
      CSIf (cond', then_', None)
   | StmtIf (cond, then_, Some else_, _) ->
      let cond' = convertExp p cond in
      let then_' = convertStmt p then_ in
      let else_' = convertStmt p else_ in
      CSIf (cond', then_', Some else_')
   | StmtFun (_, _, _, None, _) -> failwith "CodeC.convertStmt: everything should have types"
   | StmtFun (_, _, _, _, attr) when p.cleanup && removeFunction attr.used -> CSEmpty
   | StmtFun (name, args, body, Some ret, attr) ->
      let p = refreshTable p in
      let is_root = isRoot attr.used in
      let is_var = if is_root then Other else Declaration in
      let arg_names = List.map (convertTypedId p is_var) args in
      let body' = convertStmt p body in
      let body' = collectStmt p body' in
      let body' = if p.code <> LuaCode then makeSwitch p body' else body' in
      let fname = convertFunctionName p name in
      let attr = { is_root } in
      CSFunction (convertTypeMakeTupleUnit p ret, fname, arg_names, body', attr)
   (* special case for c/c++ to replace the makeArray function *)
   | StmtBind (LWild _, PCall (NoInst, [ ("makeArray" | "makeComplexArray") ], [ size; init; var ], attr), _)
      when p.code = CCode ->
      let init' = convertExp p init in
      let size' = convertExp p size in
      let init_typ = expType p init in
      let var' = convertExp p var in
      begin
         match getInitArrayFunction p init_typ with
         | Some init_func ->
            if isSmall size then
               CSBind (CLWild, CECall (init_func, [ size'; init'; var' ], attrType p attr))
            else
               CSBind (CLWild, CEEmpty)
         | None -> inlineArrayInit size' init' var'
      end
   (* special case for c/c++ to replace the makeArray function *)
   | StmtBind (LId (var, _, vattr), PCall (NoInst, [ "makeArray" ], [ size; init ], attr), _) when p.code = CCode ->
      let init' = convertExp p init in
      let size' = convertExp p size in
      let init_typ = expType p init in
      let var' = convertExp p (PId (var, vattr)) in
      begin
         match getInitArrayFunction p init_typ with
         | Some init_func ->
            if isSmall size then
               CSBind (CLWild, CECall (init_func, [ size'; init'; var' ], attrType p attr))
            else
               CSBind (CLWild, CEEmpty)
         | None -> inlineArrayInit size' init' var'
      end
   (* special case to bind tuples in c/c++ It expands tuple assigns *)
   | StmtBind ((LId (_, _, _) as lhs), PTuple (elems, _), attr) when p.code = CCode ->
      let stmts =
         List.mapi
            (fun i e ->
                let etype = (GetAttr.fromExp e).typ in
                convertStmt p (StmtBind (getRecordField lhs i etype, e, attr)))
            elems
      in
      makeSingleBlock stmts
   (* special for c/c++ initialize array variables *)
   | StmtBind (LId (lhs, Some atyp, _), PArray (elems, _), _) when p.code = CCode ->
      let elems' = convertExpArray p elems in
      let atype, _ = Typ.arrayTypeAndSize atyp in
      let lhs' = convertVarId p Other lhs in
      let typ = convertType p atype in
      let stmts = List.mapi (fun i e -> CSBind (CLIndex (typ, lhs', CEInt i), e)) elems' in
      makeSingleBlock stmts
   (* special for c/c++ to copy array variables *)
   | StmtBind (LId (lhs, _, { typ = Some typ }), rhs, _) when p.code = CCode && Typ.isArray typ ->
      let rhs' = convertExp p rhs in
      let atyp, size = Typ.arrayTypeAndSize typ in
      let atyp' = convertType p atyp in
      let copy_fn = getCopyArrayFunction p atyp' in
      CSBind (CLWild, CECall (copy_fn, [ CEInt size; CEVar (convertSingleVarId p lhs, atyp'); rhs' ], unit_typ))
   | StmtBind (lhs, rhs, _) ->
      let lhs' = convertLhsExp Other p lhs in
      let rhs' = convertExp p rhs in
      CSBind (lhs', rhs')
   | StmtBlock (_, stmts, _) ->
      let stmts' = convertStmtList p stmts in
      makeSingleBlock stmts'
   | StmtType (name, members, attr) ->
      let type_name =
         match convertType p name with
         | CTSimple t -> t
         | _ -> failwith "CodeC.convertStmt: invalid alias type"
      in
      let member_pairs = List.map (fun (id, typ, _) -> convertType p typ, convertSingleVarId p id) members in
      let attr = { is_root = isRoot attr.used } in
      CSType (type_name, member_pairs, attr)
   | StmtAliasType (t1, t2, _) ->
      let t1_name = convertType p t1 in
      let type_name =
         match convertType p t2 with
         | CTSimple t -> t
         | _ -> failwith "CodeC.convertStmt: invalid alias type"
      in
      CSAlias (type_name, t1_name)
   | StmtExternal (_, args, ret, Some link_name, attr) ->
      let p = refreshTable p in
      let is_root = isRoot attr.used in
      let is_var = if is_root then Other else Declaration in
      let arg_names = List.map (convertTypedId p is_var) args in
      CSExtFunc (convertType p ret, link_name, arg_names)
   | StmtExternal (name, args, ret, None, attr) ->
      let p = refreshTable p in
      let is_root = isRoot attr.used in
      let is_var = if is_root then Other else Declaration in
      let fname = convertId p name in
      let arg_names = List.map (convertTypedId p is_var) args in
      CSExtFunc (convertType p ret, fname, arg_names)
   | StmtEmpty -> CSEmpty


and convertStmtList (p : parameters) (stmts : stmt list) : cstmt list =
   let stmts_rev = List.fold_left (fun acc stmt -> convertStmt p stmt :: acc) [] stmts in
   List.rev stmts_rev


let convert (p : parameters) (stmts : stmt list) : cstmt list =
   let cstmts = convertStmtList p stmts in
   cstmts

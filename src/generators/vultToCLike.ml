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

open TypesVult
open CLike
open Common

type parameters =
   {
      repl : Replacements.t;
      ccode : bool; (* true if we are generating ccode *)
   }

let convertId (p:parameters) (id:id) : string =
   String.concat "_" id |> Replacements.getKeyword p.repl

let convertVarId (p:parameters) (id:id) : string list =
   List.map (Replacements.getKeyword p.repl) id

let convertSingleVarId (p:parameters) (id:id) : string =
   match id with
   | [name] ->
      Replacements.getKeyword p.repl name
   | _ -> failwith "VultToCLike.convertSingleVarId: this should be a single identifier"

let rec convertType (p:parameters) (tp:VType.t) : type_descr =
   match !tp with
   | VType.TId([typ],_) ->
      let new_type = Replacements.getType p.repl typ in
      CTSimple(new_type)
   | VType.TId(id,_) -> CTSimple(convertId p id)
   | VType.TComposed(["tuple"],_,_) -> CTSimple(VType.getTupleName tp)
   | VType.TComposed(["array"],[kind;{ contents = VType.TInt(n,_)}],_) ->
      let sub = convertType p kind in
      CTArray(sub,n)
   | VType.TLink(tp) -> convertType p tp
   | VType.TComposed(_,_,_)
   | VType.TInt _
   | VType.TArrow _
   | VType.TUnbound _
   | VType.TExpAlt _ ->
      failwith ("VultToCLike.convertType: unsupported type in c code generation: " ^ PrintTypes.typeStr tp)


let convertTypedId (p:parameters) (e:typed_id) : arg_type * string =
   match e with
   | SimpleId(_,_,_)  -> failwith "VultToCLike.convertTypedId: everything should have types"
   | TypedId(id,typ,_,_) ->
      let typ_c   = convertType p typ in
      let typ_ref = if VType.isSimpleType typ then Var(typ_c) else Ref(typ_c) in
      let ids = convertVarId p id in
      typ_ref, String.concat "." ids

let getCast (p:parameters) (from_type:type_descr) (to_type:type_descr) : string =
   match from_type, to_type with
   | CTSimple(from_t),CTSimple(to_t) ->
      Replacements.getCast p.repl from_t to_t
   | _ ->
      let from_str = show_type_descr from_type in
      let to_str   = show_type_descr to_type in
      failwith ("VultToCLike.getCast: invalid casting of types " ^ from_str ^ " -> " ^ to_str)

let makeNestedCall (name:string) (args:cexp list) : cexp =
   match args with
   | []     -> failwith "VultToCLike.makeNestedCall: invalid number of arguments"
   | [_;_]  -> CECall(name,args)
   | h :: t -> List.fold_left (fun acc a -> CECall(name,[acc;a])) h t

let convertOperator (p:parameters) (op:string) (typ:type_descr) (elems:cexp list) : cexp =
   match typ with
   | CTSimple(typ_t) ->
      begin match Replacements.getFunctionForOperator p.repl op typ_t with
      | Some(fn) -> makeNestedCall fn elems
      | None ->
         let new_op = Replacements.getOperator p.repl op typ_t in
         CEOp(new_op,elems)
      end
   | _ -> CEOp(op,elems)

let getFunctionSetType (elem_typs:type_descr list) : type_descr =
   match elem_typs with
   | [_;_;v] -> v
   | _ -> failwith "VultToCLike.getFunctionSetType: this is not a call to 'set'"

let getInitArrayFunction (p:parameters) (typ:type_descr) : string =
   match typ with
   | CTSimple(typ_t) ->
      begin match Replacements.getArrayInit p.repl typ_t  with
      | Some(fn) -> fn
      | _ -> failwith ("Invalid array type "^ (show_type_descr typ))
      end
   | _ -> failwith ("Invalid array type "^ (show_type_descr typ))

let getCopyArrayFunction (p:parameters) (typ:type_descr) : string =
   match typ with
   | CTSimple(typ_t) ->
      begin match Replacements.getArrayCopy p.repl typ_t  with
      | Some(fn) -> fn
      | _ -> failwith ("Invalid array type "^ (show_type_descr typ))
      end
   | _ -> failwith ("Invalid array type "^ (show_type_descr typ))

let convertFunction (p:parameters) (name:id) (typ:type_descr) (elems:cexp list) (elem_typs:type_descr list) : cexp =
   match name with
   (* For the function set we need to get the type based on one of the arguments *)
   | ["set"] ->
      begin match getFunctionSetType elem_typs with
      | CTSimple(typ_t) ->
         let fn = Replacements.getFunction p.repl "set" typ_t in
         CECall(fn, elems)
      | _ -> failwith ("Invalid array type "^ (show_type_descr typ))
      end
   | [fname] ->
      begin match typ with
      | CTSimple(typ_t) ->
         let fn = Replacements.getFunction p.repl fname typ_t in
         CECall(fn, elems)
      | _ -> CECall(convertId p name, elems)
      end
   | _ -> CECall(convertId p name, elems)


let attrType (p:parameters) (attr:attr) : type_descr =
   match attr.typ with
   | Some(t) -> convertType p t
   | _ -> failwith "VultToCLike.attrType: everything should have types"

let expType (p:parameters) (e:exp) : type_descr =
   GetAttr.fromExp e
   |> attrType p

let rec convertExp (p:parameters) (e:exp) : cexp =
   match e with
   | PUnit(_)       -> CEEmpty
   | PBool(v,_)     -> CEBool(v)
   | PInt(n,_)      -> CEInt(n)
   | PReal(v,_)     ->
      let s = Replacements.getRealToString p.repl v "real" in
      CEFloat(s,v)
   | PId(id,_)      -> CEVar(convertVarId p id)
   | PArray(elems,_) ->
      let elems' = convertExpList p elems in
      CEArray(elems')
   | PUnOp(op,e1,_) ->
      let e1' = convertExp p e1 in
      CEUnOp(op,e1')
   | POp(op,elems,attr) ->
      let elems' = convertExpList p elems in
      let typ    = attrType p attr in
      convertOperator p op typ elems'
   | PCall(_,[name],[arg],attr) when name="real" || name="int" || name="bool" ->
      let arg'     = convertExp p arg in
      let from_typ = expType p arg in
      let to_type  = attrType p attr in
      if from_typ <> to_type then
         CECall(getCast p from_typ to_type, [arg'])
      else arg'
   | PCall(_,name,elems,attr) ->
      let elems'   = convertExpList p elems in
      let elem_typ = List.map (expType p) elems in
      let ret_typ  = attrType p attr in
      convertFunction p name ret_typ elems' elem_typ
   | PIf(cond,then_,else_,_) ->
      let cond'  = convertExp p cond in
      let then_' = convertExp p then_ in
      let else_' = convertExp p else_ in
      CEIf(cond', then_', else_')
   | PGroup(e1,_)      -> convertExp p e1
   | PTuple([e1],_)    -> convertExp p e1
   | PTuple(elems,_)   ->
      let elems' =
         List.mapi
            (fun i a ->
                let a' = convertExp p a in
                "field_"^(string_of_int i), a')
            elems
      in
      CETuple(elems')
   | PSeq _            -> failwith "VultToCLike.convertExp: Sequences are not yet supported for js"
   | PEmpty            -> failwith "VultToCLike.convertExp: Empty expressions are not allowed"

and convertExpList (p:parameters) (e:exp list) : cexp list =
   List.map (convertExp p) e

let rec convertLhsExp (is_val:bool) (p:parameters) (e:lhs_exp) : clhsexp =
   match e with
   | LId(id,Some(typ),_) ->
      let new_id = convertVarId p id in
      CLId(convertType p typ, new_id)
   | LId(_,None,_)   -> failwith "VultToCLike.convertLhsExp: everything should have types"
   | LTyped(e1,_,_)  -> convertLhsExp is_val p e1
   | LTuple(elems,_) ->
      let elems' = convertLhsExpList is_val p elems in
      CLTuple(elems')
   | LWild _ -> CLWild
   | LGroup(e,_) -> convertLhsExp is_val p e

and convertLhsExpList (is_val:bool) (p:parameters) (lhsl:lhs_exp list) : clhsexp list =
   List.fold_left
      (fun acc lhs ->
          (convertLhsExp is_val p lhs) :: acc)
      [] lhsl
   |> List.rev

let getRecordField (name:lhs_exp) (index:int) (typ:VType.t option) : lhs_exp =
   match name with
   | LId(id,_,attr) ->
      let field = "field_"^(string_of_int index) in
      (* possible future bug, the descr does not match the actual type *)
      LId(id@[field],typ,{ attr with typ })
   | _ -> failwith "VultToCLike.getRecordFiled: Invalid input"

let rec convertStmt (p:parameters) (s:stmt) : cstmt =
   match s with
   | StmtVal(lhs,None,_) ->
      let lhs' = convertLhsExp true p lhs in
      CSVar(lhs')
   | StmtVal(_,Some(_),_) -> failwith "VultToCLike.convertStmt: val should not have initializations"
   | StmtMem _ -> CSEmpty
   | StmtWhile(cond,stmt,_) ->
      let cond' = convertExp p cond in
      let stmt' = convertStmt p stmt in (* the env is ignored *)
      CSWhile(cond', stmt')
   | StmtReturn(e1,_) ->
      let e1' = convertExp p e1 in
      CSReturn(e1')
   | StmtIf(cond,then_,None,_) ->
      let cond'  = convertExp p cond in
      let then_' = convertStmt p then_ in
      CSIf(cond',then_', None)
   | StmtIf(cond,then_,Some(else_),_) ->
      let cond'  = convertExp p cond in
      let then_' = convertStmt p then_ in
      let else_' = convertStmt p else_ in
      CSIf(cond', then_', Some(else_'))
   | StmtFun(_,_,_,None,_) -> failwith "VultCh.convertStmt: everything should have types"
   | StmtFun(name,args,body,Some(ret),_) ->
      let arg_names = List.map (convertTypedId p) args in
      let body' = convertStmt p body in
      let fname = convertId p name in
      CSFunction(convertType p ret, fname,arg_names,body')
   (* special case for c/c++ to replace the makeArray function *)
   | StmtBind(LWild(_) ,PCall(None,["makeArray"],[size;init;var],_),_) when p.ccode ->
      let init' = convertExp p init in
      let size' = convertExp p size in
      let init_typ  = expType p init in
      let init_func = getInitArrayFunction p init_typ in
      let var'  = convertExp p var in
      CSBind(CLWild,CECall(init_func,[size';init';var']))
   (* special case to bind tuples in c/c++ It expands tuple assigns *)
   | StmtBind(LId(_,_,_) as lhs,PTuple(elems,_),attr) when p.ccode ->
      let stmts =
         List.mapi (fun i e ->
               let etype = (GetAttr.fromExp e).typ in
               convertStmt p (StmtBind(getRecordField lhs i etype,e,attr))) elems in
      CSBlock(stmts)
   (* special for c/c++ initialize array variables *)
   | StmtBind(LId(lhs,Some(typ),_),PArray(elems,_),_) when p.ccode ->
      let elems' = convertExpList p elems in
      let atype,_ = VType.arrayTypeAndSize typ in
      let lhs' = convertVarId p lhs in
      begin match convertType p atype with
      | CTSimple(typ_t) ->
         let fn = Replacements.getFunction p.repl "set" typ_t in
         let stmts = List.mapi (fun i e -> CSBind(CLWild,CECall(fn,[CEVar(lhs');CEInt(i);e]))) elems' in
         CSBlock(stmts)
      | _ -> failwith ""
      end
   (* special for c/c++ to copy array variables *)
   | StmtBind(LId(lhs,_,{ typ = Some(typ)}),rhs,_) when p.ccode && VType.isArray typ ->
      let rhs' = convertExp p rhs in
      let atyp,size = VType.arrayTypeAndSize typ in
      let atyp' = convertType p atyp in
      let copy_fn = getCopyArrayFunction p atyp' in
      CSBind(CLWild,CECall(copy_fn,[CEInt(size);CEVar(convertVarId p lhs);rhs']))
   | StmtBind(lhs,rhs,_) ->
      let lhs' = convertLhsExp false p lhs in
      let rhs' = convertExp p rhs in
      CSBind(lhs', rhs')
   | StmtBlock(_,stmts,_) ->
      let stmts' = convertStmtList p stmts in
      CSBlock(stmts')
   | StmtType(name,members,_) ->
      let type_name =
         match convertType p name with
         | CTSimple(t) -> t
         | _ -> failwith "VultCh.convertStmt: invalid alias type"
      in
      let member_pairs = List.map (fun (id,typ,_) -> convertType p typ, convertSingleVarId p id) members in
      CSType(type_name,member_pairs)
   | StmtAliasType(t1,t2,_) ->
      let t1_name   = convertType p t1 in
      let type_name =
         match convertType p t2 with
         | CTSimple(t) -> t
         | _ -> failwith "VultCh.convertStmt: invalid alias type"
      in
      CSAlias(type_name,t1_name)
   | StmtExternal(_,args,ret,name,_) ->
      let arg_names = List.map (convertTypedId p) args in
      CSExtFunc(convertType p ret,name,arg_names)
   | StmtEmpty       -> CSEmpty

and convertStmtList (p:parameters) (stmts:stmt list) : cstmt list =
   let stmts_rev =
      List.fold_left
         (fun acc stmt ->
             convertStmt p stmt :: acc)
         [] stmts
   in
   List.rev stmts_rev
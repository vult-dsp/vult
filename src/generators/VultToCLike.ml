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

let rec join (sep:string) (id:string list) : string =
   match id with
   | [] -> ""
   | [ name ] -> name
   | h :: t -> h ^ sep ^ (join sep t)

let convertId (r:Replacements.t) (id:id) : string =
   join "_" id |> Replacements.getKeyword r

let convertVarId (r:Replacements.t) (id:id) : string =
   List.map (Replacements.getKeyword r) id
   |> join "."

let rec convertType (r:Replacements.t) (tp:VType.t) : type_descr =
   match !tp with
   | VType.TId([typ],_) ->
      let new_type = Replacements.getType r typ in
      CTSimple(new_type)
   | VType.TId(id,_) -> CTSimple(convertId r id)
   | VType.TComposed(["tuple"],_,_) -> CTSimple(VType.getTupleName tp)
   | VType.TComposed(["array"],[kind;{ contents = VType.TInt(n,_)}],_) ->
      let sub = convertType r kind in
      CTArray(sub,n)
   | VType.TLink(tp) -> convertType r tp
   | VType.TComposed(_,_,_)
   | VType.TInt _
   | VType.TArrow _
   | VType.TUnbound _
   | VType.TExpAlt _ ->
      failwith ("VultToCLike.convertType: unsupported type in c code generation" ^ PrintTypes.typeStr tp)


let isValue (typ:VType.t) : bool =
   match !(VType.unlink typ) with
   | VType.TId(name,_) when name=["int"] || name=["real"] || name=["bool"] || name=["void"] ->
      true
   | _ -> false

let convertTypedId (r:Replacements.t) (e:typed_id) : arg_type * string =
   match e with
   | SimpleId(_,_)  -> failwith "VultToCLike.convertTypedId: everything should have types"
   | TypedId(id,typ,_) ->
      let typ_c   = convertType r typ in
      let typ_ref = if isValue typ then Var(typ_c) else Ref(typ_c) in
      typ_ref, convertVarId r id

let getCast (r:Replacements.t) (from_type:type_descr) (to_type:type_descr) : string =
   match from_type, to_type with
   | CTSimple(from_t),CTSimple(to_t) ->
      Replacements.getCast r from_t to_t
   | _ ->
      let from_str = show_type_descr from_type in
      let to_str   = show_type_descr to_type in
      failwith ("VultToCLike.getCast: invalid casting of types " ^ from_str ^ " -> " ^ to_str)

let makeNestedCall (name:string) (args:cexp list) : cexp =
   match args with
   | []     -> failwith "VultToCLike.makeNestedCall: invalid number of arguments"
   | [_;_]  -> CECall(name,args)
   | h :: t -> List.fold_left (fun acc a -> CECall(name,[acc;a])) h t


let convertOperator (r:Replacements.t) (op:string) (typ:type_descr) (elems:cexp list) : cexp =
   match typ with
   | CTSimple(typ_t) ->
      begin match Replacements.getFunctionForOperator r op typ_t with
         | Some(fn) -> makeNestedCall fn elems
         | None ->
            let new_op = Replacements.getOperator r op typ_t in
            CEOp(new_op,elems)
      end
   | _ -> CEOp(op,elems)

let getFunctionSetType (elem_typs:type_descr list) : type_descr =
   match elem_typs with
   | [_;_;v] -> v
   | _ -> failwith "VultToCLike.getFunctionSetType: this is not a call to 'set'"

let getInitArrayFunction (r:Replacements.t) (typ:type_descr) : string =
   match typ with
   | CTSimple(typ_t) ->
      begin match Replacements.getArrayInit r typ_t  with
         | Some(fn) -> fn
         | _ -> failwith ("Invalid array type "^ (show_type_descr typ))
      end
   | _ -> failwith ("Invalid array type "^ (show_type_descr typ))

let convertFunction (r:Replacements.t) (name:id) (typ:type_descr) (elems:cexp list) (elem_typs:type_descr list) : cexp =
   match name with
   (* For the function set we need to get the type based on one of the arguments *)
   | ["set"] ->
      begin match getFunctionSetType elem_typs with
         | CTSimple(typ_t) ->
            let fn = Replacements.getFunction r "set" typ_t in
            CECall(fn, elems)
         | _ -> failwith ("Invalid array type "^ (show_type_descr typ))
      end
   | [fname] ->
      begin match typ with
         | CTSimple(typ_t) ->
            let fn = Replacements.getFunction r fname typ_t in
            CECall(fn, elems)
         | _ -> CECall(convertId r name, elems)
      end
   | _ -> CECall(convertId r name, elems)


let attrType (r:Replacements.t) (attr:attr) : type_descr =
   match attr.typ with
   | Some(t) -> convertType r t
   | _ -> failwith "VultToCLike.attrType: everything should have types"

let expType (r:Replacements.t) (e:exp) : type_descr =
   GetAttr.fromExp e
   |> attrType r

let rec convertExp (r:Replacements.t) (e:exp) : cexp =
   match e with
   | PUnit(_)       -> CEInt(0)
   | PBool(v,_)     -> CEBool(v)
   | PInt(n,_)      -> CEInt(n)
   | PReal(v,_)     ->
      let s = Replacements.getRealToString r v "real" in
      CEFloat(s,v)
   | PId(id,_)      -> CEVar(convertVarId r id)
   | PArray(elems,_) ->
      let elems' = convertExpList r elems in
      CEArray(elems')
   | PUnOp(op,e1,_) ->
      let e1' = convertExp r e1 in
      CEUnOp(op,e1')
   | POp(op,elems,attr) ->
      let elems' = convertExpList r elems in
      let typ    = attrType r attr in
      convertOperator r op typ elems'
   | PCall(_,[name],[arg],attr) when name="real" || name="int" || name="bool" ->
      let arg'     = convertExp r arg in
      let from_typ = expType r arg in
      let to_type  = attrType r attr in
      if from_typ <> to_type then
         CECall(getCast r from_typ to_type, [arg'])
      else arg'
   | PCall(_,name,elems,attr) ->
      let elems'   = convertExpList r elems in
      let elem_typ = List.map (expType r) elems in
      let ret_typ  = attrType r attr in
      convertFunction r name ret_typ elems' elem_typ
   | PIf(cond,then_,else_,_) ->
      let cond'  = convertExp r cond in
      let then_' = convertExp r then_ in
      let else_' = convertExp r else_ in
      CEIf(cond', then_', else_')
   | PGroup(e1,_)      -> convertExp r e1
   | PTuple([e1],_)    -> convertExp r e1
   | PTuple(elems,_)   ->
      let elems' =
         List.mapi
            (fun i a ->
                let a' = convertExp r a in
                "field_"^(string_of_int i), a')
            elems
      in
      CETuple(elems')
   | PSeq _            -> failwith "VultToCLike.convertExp: Sequences are not yet supported for js"
   | PEmpty            -> failwith "VultToCLike.convertExp: Empty expressions are not allowed"

and convertExpList (r:Replacements.t) (e:exp list) : cexp list =
   List.map (convertExp r) e

let rec convertLhsExp is_val (r:Replacements.t) (e:lhs_exp) : clhsexp =
   match e with
   | LId(id,Some(typ),_) ->
      let new_id = convertVarId r id in
      CLId(convertType r typ, new_id)
   | LId(_,None,_)   -> failwith "VultToCLike.convertLhsExp: everything should have types"
   | LTyped(e1,_,_)  -> convertLhsExp is_val r e1
   | LTuple(elems,_) ->
      let elems' = convertLhsExpList is_val r elems in
      CLTuple(elems')
   | LWild _ -> CLWild
   | LGroup(e,_) -> convertLhsExp is_val r e

and convertLhsExpList is_val (r:Replacements.t) (lhsl:lhs_exp list) : clhsexp list =
   List.fold_left
      (fun acc lhs ->
          (convertLhsExp is_val r lhs) :: acc)
      [] lhsl
   |> List.rev

let rec convertStmt (r:Replacements.t) (s:stmt) : cstmt =
   match s with
   | StmtVal(lhs,None,_) ->
      let lhs' = convertLhsExp true r lhs in
      CSVarDecl(lhs',None)
   | StmtVal(lhs,Some(rhs),_) ->
      let lhs' = convertLhsExp true r lhs in
      let rhs' = convertExp r rhs in
      CSVarDecl(lhs',Some(rhs'))
   | StmtMem _                -> CSEmpty
   | StmtWhile(cond,stmt,_) ->
      let cond' = convertExp r cond in
      let stmt' = convertStmt r stmt in (* the env is ignored *)
      CSWhile(cond', stmt')
   | StmtReturn(e1,_) ->
      let e1' = convertExp r e1 in
      CSReturn(e1')
   | StmtIf(cond,then_,None,_) ->
      let cond'  = convertExp r cond in
      let then_' = convertStmt r then_ in
      CSIf(cond',then_', None)
   | StmtIf(cond,then_,Some(else_),_) ->
      let cond'  = convertExp r cond in
      let then_' = convertStmt r then_ in
      let else_' = convertStmt r else_ in
      CSIf(cond', then_', Some(else_'))
   | StmtFun(_,_,_,None,_) -> failwith "VultCh.convertStmt: everything should have types"
   | StmtFun(name,args,body,Some(ret),_) ->
      let arg_names = List.map (convertTypedId r) args in
      let body' = convertStmt r body in
      let fname = convertId r name in
      CSFunction(convertType r ret, fname,arg_names,body')
   (* Special case for initializing arrays*)
   | StmtBind(LId(name,_,_),PCall(None,["makeArray"],[size;init],_),_) ->
      let init' = convertExp r init in
      let size' = convertExp r size in
      let init_typ  = expType r init in
      let init_func = getInitArrayFunction r init_typ in
      let var_name  = CEVar(convertVarId r name) in
      CSBind(CLWild,CECall(init_func,[var_name;size';init']))
   | StmtBind(lhs,rhs,_) ->
      let lhs' = convertLhsExp false r lhs in
      let rhs' = convertExp r rhs in
      CSBind(lhs', rhs')
   | StmtBlock(_,stmts,_) ->
      let stmts' = convertStmtList r stmts in
      CSBlock(stmts')
   | StmtType(name,members,_) ->
      let type_name =
         match convertType r name with
         | CTSimple(t) -> t
         | _ -> failwith "VultCh.convertStmt: invalid alias type"
      in
      let member_pairs = List.map (fun (id,typ,_) -> convertType r typ, convertVarId r id) members in
      CSType(type_name,member_pairs)
   | StmtAliasType(t1,t2,_) ->
      let t1_name   = convertType r t1 in
      let type_name =
         match convertType r t2 with
         | CTSimple(t) -> t
         | _ -> failwith "VultCh.convertStmt: invalid alias type"
      in
      CSAlias(type_name,t1_name)
   | StmtExternal(_,args,ret,name,_) ->
      let arg_names = List.map (convertTypedId r) args in
      CSExtFunc(convertType r ret,name,arg_names)
   | StmtEmpty       -> CSEmpty

and convertStmtList r (stmts:stmt list) : cstmt list =
   let stmts_rev =
      List.fold_left
         (fun acc stmt ->
             convertStmt r stmt :: acc)
         [] stmts
   in
   List.rev stmts_rev
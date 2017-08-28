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

let unit_typ = CTSimple("unit")

module Atomic = struct
   type s = { tick : int }

   let is_unit ts =
      match ts with
      | CTSimple("unit") -> true
      | CTSimple("void") -> true
      | _ -> false

   let cexpType e =
      match e with
      | CEInt _   -> CTSimple("int")
      | CEFloat _ -> CTSimple("real")
      | CEBool _  -> CTSimple("bool")
      | CEString _ -> CTSimple("string")
      | CEArray(_,ts)
      | CECall(_,_,ts)
      | CEUnOp(_,_,ts)
      | CEOp(_,_,ts)
      | CEVar(_,ts)
      | CEIndex(_,_,ts)
      | CEIf(_,_,_,ts)
      | CETuple(_,ts) -> ts
      | CEEmpty -> unit_typ

   let makeTemp s =
      let name = "$r"^(string_of_int s.tick) in
      { tick = s.tick + 1 }, name

   type bind_type =
      | Temp
      | Bind of clhsexp
      | NoBind

   let bindToTemp (s:s) (bind:bind_type) (exp:cexp) : s * cstmt list * cexp =
      match bind with
      | Bind (CLId(ts, var) as lhs) ->
         let stmt = [CSBind(lhs,exp)] in
         s, stmt, CEVar(var,ts)
      | NoBind ->
         let stmt = [CSBind(CLWild,exp)] in
         s, stmt, CEEmpty
      | _ ->
         let s', name = makeTemp s in
         let ts = cexpType exp in
         let lhs = CLId(ts,[name]) in
         let stmt = [CSVar(lhs,None); CSBind(lhs,exp)] in
         s', stmt, CEVar([name],ts)


   let rec makeOpAtomic s (bind:bind_type) op ts elems : s * cstmt list * cexp  =
      match elems with
      | []
      | [_] -> failwith "makeOpAtomic: invalid input"
      | [e1; e2] ->
         let s, pre1, e1' = makeExpAtomic s Temp e1 in
         let s, pre2, e2' = makeExpAtomic s Temp e2 in
         let s, pre3, a = bindToTemp s bind (CEOp(op, [e1'; e2'], ts)) in
         s, pre1 @ pre2@ pre3, a
      | h::t ->
         let s, pre1, h' = makeExpAtomic s Temp h in
         let s, pre2, a = makeOpAtomic s Temp op ts t in
         let s, pre3, b = bindToTemp s bind (CEOp(op, [h'; a], ts)) in
         s, pre1@pre2@pre3, b


   and makeExpAtomic (s:s) (bind:bind_type) (exp:cexp) : s * cstmt list * cexp =
      match exp with
      | CEInt _
      | CEFloat _
      | CEBool _
      | CEString _
      | CEVar _ when bind <> Temp ->
         let s, pre, e = bindToTemp s bind exp in
         s, pre, e
      | CEInt _
      | CEFloat _
      | CEBool _
      | CEString _
      | CEVar _ ->
         s, [], exp
      | CEIndex _ ->
         let s, pre, e = bindToTemp s bind exp in
         s, pre, e
      | CEArray(elems, ts) ->
         let s, pre, elems' = makeExpListAtomic s elems in
         let s, pre1, ret = bindToTemp s bind (CEArray(elems', ts)) in
         s, pre@pre1, ret

      | CECall(name, args, ts) ->
         let s, pre, args' = makeExpListAtomic s args in
         let s, pre1, ret = bindToTemp s bind (CECall(name, args', ts)) in
         s, pre @ pre1, ret

      | CEUnOp(op,arg,ts) ->
         let s, pre, arg' = makeExpAtomic s Temp arg in
         let s, pre1, ret = bindToTemp s bind (CEUnOp(op,arg',ts)) in
         s, pre@pre1, ret

      | CETuple(elems, ts) ->
         let labels, expl = List.split elems in
         let s, pre, expl' = makeExpListAtomic s expl in
         let elems' = List.combine labels expl' in
         let s, pre1, ret = bindToTemp s bind (CETuple(elems', ts)) in
         s, pre@pre1, ret

      | CEIf(cond,then_,else_,ts) ->
         let s, tmp = makeTemp s in
         let ltmp = CLId(ts, [tmp]) in
         let if_stmt = CSIf(cond,CSBind(ltmp,then_),Some(CSBind(ltmp,else_))) in
         s, [CSVar(ltmp,None); if_stmt], CEVar([tmp],ts)

      | CEOp(op,elems,ts) ->
         let s, pre, ret = makeOpAtomic s bind op ts elems in
         s, pre, ret

      | CEEmpty -> s, [], CEEmpty


   and makeExpListAtomic (s:s) (elems: cexp list) : s * cstmt list * cexp list =
      let s', pre_rev, elems_rev =
         List.fold_left
            (fun (s,pre,acc) exp ->
                let s', p, e' = makeExpAtomic s Temp exp in
                s', (p::pre), (e'::acc))
            (s,[],[])
            elems
      in
      s', List.concat (List.rev pre_rev), List.rev elems_rev

   let makeSingleStmt stmts =
      match stmts with
      | [] -> CSEmpty
      | [stmt] -> stmt
      | _ -> CSBlock(stmts)


   let rec makeStmtAtomic (s:s) (stmt:cstmt) =
      match stmt with
      | CSVar(_,None)
      | CSType _
      | CSAlias _
      | CSExtFunc _-> s, [stmt]

      | CSVar((CLId _ as lhs), Some(rhs)) ->
         let s, pre, _ = makeExpAtomic s (Bind lhs) rhs in
         s, pre

      | CSVar(lhs, Some(rhs)) ->
         let s, pre, rhs' = makeExpAtomic s Temp rhs in
         s, pre @ [CSVar(lhs, None); CSBind(lhs, rhs')]

      | CSBind(CLWild, rhs) ->
         let s, pre, _ = makeExpAtomic s NoBind rhs in
         s, pre

      | CSBind((CLId _ as lhs),rhs) ->
         let s, pre, _ = makeExpAtomic s (Bind lhs) rhs in
         s, pre

      | CSBind(lhs,rhs) ->
         let s, pre, rhs' = makeExpAtomic s Temp rhs in
         s, pre @ [CSBind(lhs, rhs')]

      | CSConst(lhs,rhs) ->
         s, [CSConst(lhs, rhs)]

      | CSReturn(e) ->
         let s, pre, e' = makeExpAtomic s Temp e in
         s, pre @ [CSReturn(e')]

      | CSWhile(cond,body) ->
         let s, pre, cond' = makeExpAtomic s Temp cond in
         let s, body' = makeStmtAtomic s body in
         s, pre @ [CSWhile(cond', makeSingleStmt body')]

      | CSIf(cond,then_,Some(else_)) ->
         let s, pre, cond' = makeExpAtomic s Temp cond in
         let s, then_' = makeStmtAtomic s then_ in
         let s, else_' = makeStmtAtomic s else_ in
         s, pre @ [CSIf(cond', makeSingleStmt then_', Some(makeSingleStmt else_'))]

      | CSIf(cond,then_, None) ->
         let s, pre, cond' = makeExpAtomic s Temp cond in
         let s, then_' = makeStmtAtomic s then_ in
         s, pre @ [CSIf(cond', makeSingleStmt then_', None)]

      | CSFunction(ts, name, args, body) ->
         let s, body' = makeStmtAtomic s body in
         s, [CSFunction(ts, name, args, makeSingleStmt body')]

      | CSBlock(stmts) ->
         let s, stmts' = makeStmtListAtomic s stmts in
         s, [CSBlock(stmts')]

      | CSEmpty -> s,[stmt]

   and makeStmtListAtomic (s:s) (stmts:cstmt list) =
      let s, stmt_rev =
         List.fold_left
            (fun (s,acc) stmt ->
                let s, stmt' = makeStmtAtomic s stmt in
                s, stmt' :: acc)
            (s,[])
            stmts
      in
      s, List.concat (List.rev stmt_rev)
end



type parameters =
   {
      repl : Replacements.t;
      code : Args.code;
   }

let convertId (p:parameters) (id:Id.t) : string =
   String.concat "_" id |> Replacements.getKeyword p.repl

let convertVarId (p:parameters) (id:Id.t) : string list =
   List.map (Replacements.getKeyword p.repl) id

let convertSingleVarId (p:parameters) (id:Id.t) : string =
   match id with
   | [name] ->
      Replacements.getKeyword p.repl name
   | _ -> failwith "ProgToCode.convertSingleVarId: this should be a single identifier"

let rec convertType (p:parameters) (tp:Typ.t) : type_descr =
   match !tp with
   | Typ.TId([typ],_) ->
      let new_type = Replacements.getType p.repl typ in
      CTSimple(new_type)
   | Typ.TId(id,_) -> CTSimple(convertId p id)
   | Typ.TComposed(["tuple"],_,_) -> CTSimple(Typ.getTupleName tp)
   | Typ.TComposed(["array"],[kind;{ contents = Typ.TInt(n,_)}],_) ->
      let sub = convertType p kind in
      CTArray(sub,n)
   | Typ.TLink(tp) -> convertType p tp
   | Typ.TComposed(_,_,_)
   | Typ.TInt _
   | Typ.TArrow _
   | Typ.TUnbound _
   | Typ.TExpAlt _ ->
      failwith ("ProgToCode.convertType: unsupported type in c code generation: " ^ PrintProg.typeStr tp)

let convertTypedId (p:parameters) (e:typed_id) : arg_type * string =
   match e with
   | SimpleId(_,_,_)  -> failwith "ProgToCode.convertTypedId: everything should have types"
   | TypedId(id,typ,_,_) ->
      let typ_c   = convertType p typ in
      let typ_ref = if Typ.isSimpleType typ then Var(typ_c) else Ref(typ_c) in
      let ids = convertVarId p id in
      typ_ref, String.concat "." ids

let getCast (p:parameters) (from_type:type_descr) (to_type:type_descr) : string =
   match from_type, to_type with
   | CTSimple(from_t),CTSimple(to_t) ->
      Replacements.getCast p.repl from_t to_t
   | _ ->
      let from_str = show_type_descr from_type in
      let to_str   = show_type_descr to_type in
      failwith ("ProgToCode.getCast: invalid casting of types " ^ from_str ^ " -> " ^ to_str)

let makeNestedCall (typ:type_descr) (name:string) (args:cexp list) : cexp =
   match args with
   | []     -> failwith "ProgToCode.makeNestedCall: invalid number of arguments"
   | [_;_]  -> CECall(name, args, typ)
   | h :: t -> List.fold_left (fun acc a -> CECall(name, [acc;a], typ)) h t

let convertOperator (p:parameters) (op:string) (typ:type_descr) (elems:cexp list) : cexp =
   match typ with
   | CTSimple(typ_t) ->
      begin match Replacements.getFunctionForOperator p.repl op typ_t with
         | Some(fn) -> makeNestedCall typ fn elems
         | None ->
            let new_op = Replacements.getOperator p.repl op typ_t in
            CEOp(new_op, elems, typ)
      end
   | _ -> CEOp(op, elems, typ)

let getFunctionSetType (elem_typs:type_descr list) : type_descr =
   match elem_typs with
   | [_;_;v] -> v
   | _ -> failwith "ProgToCode.getFunctionSetType: this is not a call to 'set'"

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

let convertFunction (p:parameters) (name:Id.t) (typ:type_descr) (elems:cexp list) (elem_typs:type_descr list) : cexp =
   match name with
   (* For the function set we need to get the type based on one of the arguments *)
   | ["set"] ->
      begin match getFunctionSetType elem_typs with
         | CTSimple(typ_t) ->
            let fn = Replacements.getFunction p.repl "set" typ_t in
            CECall(fn, elems, typ)
         | _ -> failwith ("Invalid array type "^ (show_type_descr typ))
      end
   | [fname] ->
      begin match typ with
         | CTSimple(typ_t) ->
            let fn = Replacements.getFunction p.repl fname typ_t in
            CECall(fn, elems, typ)
         | _ -> CECall(convertId p name, elems, typ)
      end
   | _ -> CECall(convertId p name, elems, typ)


let attrType (p:parameters) (attr:attr) : type_descr =
   match attr.typ with
   | Some(t) -> convertType p t
   | _ -> failwith "ProgToCode.attrType: everything should have types"

let expType (p:parameters) (e:exp) : type_descr =
   GetAttr.fromExp e
   |> attrType p

let rec convertExp (p:parameters) (e:exp) : cexp =
   let typ = attrType p in
   match e with
   | PUnit(_)    -> CEEmpty
   | PBool(v,_)  -> CEBool(v)
   | PInt(n,_)   -> CEInt(n)
   | PString(s,_)   -> CEString(s)
   | PReal(v,_)  ->
      let s = Replacements.getRealToString p.repl (Float.crop v) "real" in
      CEFloat(s,Float.crop v)
   | PId(id,attr) ->
      CEVar(convertVarId p id, typ attr)
   | PIndex(e, index, attr) ->
      let e' = convertExp p e in
      let index' = convertExp p index in
      CEIndex(e', index', typ attr)

   | PArray(elems,attr) ->
      let elems' = convertExpArray p elems in
      CEArray(elems', typ attr)
   | PUnOp(op,e1,attr) ->
      let e1' = convertExp p e1 in
      CEUnOp(op, e1', typ attr)
   | POp(op,elems,attr) ->
      let elems' = convertExpList p elems in
      let typ    = attrType p attr in
      convertOperator p op typ elems'
   | PCall(_,[name],[arg],attr) when name="real" || name="int" || name="bool" ->
      let arg'     = convertExp p arg in
      let from_typ = expType p arg in
      let to_type  = attrType p attr in
      if from_typ <> to_type then
         CECall(getCast p from_typ to_type, [arg'], typ attr)
      else arg'
   | PCall(_,name,elems,attr) ->
      let elems'   = convertExpList p elems in
      let elem_typ = List.map (expType p) elems in
      let ret_typ  = attrType p attr in
      convertFunction p name ret_typ elems' elem_typ
   | PIf(cond,then_,else_,attr) ->
      let cond'  = convertExp p cond in
      let then_' = convertExp p then_ in
      let else_' = convertExp p else_ in
      CEIf(cond', then_', else_', typ attr)
   | PGroup(e1,_)      -> convertExp p e1
   | PTuple([e1],_)    -> convertExp p e1
   | PTuple(elems,attr)   ->
      let elems' =
         List.mapi
            (fun i a ->
                let a' = convertExp p a in
                "field_"^(string_of_int i), a')
            elems
      in
      CETuple(elems', typ attr)
   | PSeq _            -> failwith "ProgToCode.convertExp: Sequences are not yet supported for js"
   | PEmpty            -> failwith "ProgToCode.convertExp: Empty expressions are not allowed"

and convertExpList (p:parameters) (e:exp list) : cexp list =
   List.map (convertExp p) e

and convertExpArray (p:parameters) (e:exp array) : cexp list =
   Array.map (convertExp p) e |> Array.to_list

and convertLhsExp (is_val:bool) (p:parameters) (e:lhs_exp) : clhsexp =
   match e with
   | LId(id,Some(typ),_) ->
      let new_id = convertVarId p id in
      CLId(convertType p typ, new_id)
   | LId(_,None,_)   -> failwith "ProgToCode.convertLhsExp: everything should have types"
   | LTyped(e1,_,_)  -> convertLhsExp is_val p e1
   | LTuple(elems,_) ->
      let elems' = convertLhsExpList is_val p elems in
      CLTuple(elems')
   | LWild _ -> CLWild
   | LGroup(e,_) -> convertLhsExp is_val p e
   | LIndex (_, None, _, _) -> failwith "ProgToCode.convertLhsExp: everything should have types"
   | LIndex (id,Some(typ), index, _) ->
      let new_id = convertVarId p id in
      let index = convertExp p index in
      CLIndex (convertType p typ, new_id, index)


and convertLhsExpList (is_val:bool) (p:parameters) (lhsl:lhs_exp list) : clhsexp list =
   List.fold_left
      (fun acc lhs ->
          (convertLhsExp is_val p lhs) :: acc)
      [] lhsl
   |> List.rev

let getRecordField (name:lhs_exp) (index:int) (typ:Typ.t option) : lhs_exp =
   match name with
   | LId(id,_,attr) ->
      let field = "field_"^(string_of_int index) in
      (* possible future bug, the descr does not match the actual type *)
      LId(id@[field],typ,{ attr with typ })
   | _ -> failwith "ProgToCode.getRecordFiled: Invalid input"

let rec collectVarBind stmts =
   match stmts with
   | [] -> []
   | CSVar(lhs1,None)::CSBind(lhs2,rhs)::t when lhs1 = lhs2 ->
      collectVarBind (CSVar(lhs1,Some(rhs)) ::  t)
   | CSVar(CLId(_,lhs),Some(rhs))::CSIf(CEVar(cond,_),then_,else_)::t when lhs = cond ->
      collectVarBind (CSIf(rhs,then_,else_)::t)
   | h::t -> h :: collectVarBind t

let collectStmt (p:parameters) stmt =
   match stmt with
   | CSBlock(stmts) when not (p.code = CCode) -> CSBlock(collectVarBind stmts)
   | _ -> stmt

let rec convertStmt (p:parameters) (s:stmt) : cstmt =
   match s with
   | StmtVal(lhs,None,_) ->
      let lhs' = convertLhsExp true p lhs in
      CSVar(lhs',None)
   | StmtVal(lhs,Some(rhs),attr) when attr.const ->
      let lhs' = convertLhsExp false p lhs in
      let rhs' = convertExp p rhs in
      CSConst(lhs', rhs')
   | StmtVal(lhs,Some(rhs),attr) when attr.const ->
      let lhs' = convertLhsExp false p lhs in
      let rhs' = convertExp p rhs in
      CSVar(lhs', Some(rhs'))
   | StmtVal(_,Some(_),_) -> failwith "ProgToCode.convertStmt: val should not have initializations"
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
   | StmtFun(_,_,_,None,_) -> failwith "CodeC.convertStmt: everything should have types"
   | StmtFun(name,args,body,Some(ret),_) ->
      let arg_names = List.map (convertTypedId p) args in
      let body' = convertStmt p body in
      let fname = convertId p name in
      CSFunction(convertType p ret, fname,arg_names,collectStmt p body')
   (* special case for c/c++ to replace the makeArray function *)
   | StmtBind(LWild(_) ,PCall(None,["makeArray"],[size;init;var],attr),_) when p.code = CCode ->
      let init' = convertExp p init in
      let size' = convertExp p size in
      let init_typ  = expType p init in
      let init_func = getInitArrayFunction p init_typ in
      let var'  = convertExp p var in
      CSBind(CLWild,CECall(init_func,[size';init';var'], attrType p attr))
   (* special case to bind tuples in c/c++ It expands tuple assigns *)
   | StmtBind(LId(_,_,_) as lhs,PTuple(elems,_),attr) when p.code = CCode ->
      let stmts =
         List.mapi (fun i e ->
               let etype = (GetAttr.fromExp e).typ in
               convertStmt p (StmtBind(getRecordField lhs i etype,e,attr))) elems in
      CSBlock(stmts)
   (* special for c/c++ initialize array variables *)
   | StmtBind(LId(lhs,Some(atyp),_),PArray(elems,_),_) when p.code = CCode ->
      let elems' = convertExpArray p elems in
      let atype,_ = Typ.arrayTypeAndSize atyp in
      let lhs' = convertVarId p lhs in
      let typ = convertType p atype in
      let stmts = List.mapi (fun i e -> CSBind(CLIndex(typ, lhs', CEInt(i)),e)) elems' in
      CSBlock(stmts)
   (* special for c/c++ to copy array variables *)
   | StmtBind(LId(lhs,_,{ typ = Some(typ)}),rhs,_) when p.code = CCode && Typ.isArray typ ->
      let rhs' = convertExp p rhs in
      let atyp,size = Typ.arrayTypeAndSize typ in
      let atyp' = convertType p atyp in
      let copy_fn = getCopyArrayFunction p atyp' in
      CSBind(CLWild,CECall(copy_fn,[CEInt(size);CEVar(convertVarId p lhs, atyp');rhs'],unit_typ))
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
         | _ -> failwith "CodeC.convertStmt: invalid alias type"
      in
      let member_pairs = List.map (fun (id,typ,_) -> convertType p typ, convertSingleVarId p id) members in
      CSType(type_name,member_pairs)
   | StmtAliasType(t1,t2,_) ->
      let t1_name   = convertType p t1 in
      let type_name =
         match convertType p t2 with
         | CTSimple(t) -> t
         | _ -> failwith "CodeC.convertStmt: invalid alias type"
      in
      CSAlias(type_name,t1_name)
   | StmtExternal(_,args,ret,Some link_name,_) ->
      let arg_names = List.map (convertTypedId p) args in
      CSExtFunc(convertType p ret,link_name,arg_names)
   | StmtExternal(name,args,ret,None,_) ->
      let fname = convertId p name in
      let arg_names = List.map (convertTypedId p) args in
      CSExtFunc(convertType p ret,fname,arg_names)
   | StmtEmpty -> CSEmpty

and convertStmtList (p:parameters) (stmts:stmt list) : cstmt list =
   let stmts_rev =
      List.fold_left
         (fun acc stmt ->
             convertStmt p stmt :: acc)
         [] stmts
   in
   List.rev stmts_rev

let convert (p:parameters) (stmts:stmt list) : cstmt list =
   let cstmts = convertStmtList p stmts in
   if p.code = LLVMCode then
      let _ , ctmts = Atomic.makeStmtListAtomic { Atomic.tick = 0 } cstmts in
      ctmts
   else
      cstmts
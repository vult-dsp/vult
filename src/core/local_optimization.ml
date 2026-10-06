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

open Prog
open Util.Maps
open Analysis

module ExpMap = Stdlib.Map.Make (struct
  type t = exp

  let compare = Compare.exp
end)

type cached = {name: string; dependencies: Set.t}

(* Whether the expression being visited runs every time the statement does.
   Only then may its value be hoisted into a temporary above the statement. *)
type evaluation = Always | Conditional

let pure_builtin = Builtin.is_pure_name

let scalar_type (t : type_) = match t.t with TInt | TInt16 | TReal | TString | TBool | TFix16 -> true | _ -> false

let rec calls_exp calls (e : exp) =
  match e.e with
  | ECall {path; args} ->
      CCList.fold_left calls_exp (Set.add path calls) args
  | EUnOp (_, e) | EMember (e, _) | ETMember (e, _) ->
      calls_exp calls e
  | EOp (_, lhs, rhs) | EIndex {e= lhs; index= rhs} ->
      calls_exp (calls_exp calls lhs) rhs
  | EArray elems | ETuple elems ->
      CCList.fold_left calls_exp calls elems
  | EIf {cond; then_; else_} ->
      calls_exp (calls_exp (calls_exp calls cond) then_) else_
  | ERecord {elems; _} ->
      CCList.fold_left (fun calls (_, e) -> calls_exp calls e) calls elems
  | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ ->
      calls

let calls_lexp calls (l : lexp) =
  let rec loop calls (l : lexp) =
    match l.l with
    | LIndex {e; index} ->
        calls_exp (loop calls e) index
    | LMember (e, _) ->
        loop calls e
    | LTuple elems ->
        CCList.fold_left loop calls elems
    | LWild | LId _ ->
        calls
  in
  loop calls l

let calls_stmt calls s =
  fold_stmt
    (fun calls (s : stmt) ->
      match s.s with
      | StmtDecl (_, init) ->
          Option.fold ~none:calls ~some:(calls_exp calls) init
      | StmtBind (lhs, rhs) ->
          calls_exp (calls_lexp calls lhs) rhs
      | StmtReturn e ->
          calls_exp calls e
      | StmtIf (cond, _, _) | StmtWhile (cond, _) ->
          calls_exp calls cond
      | StmtSwitch (cond, cases, _) ->
          CCList.fold_left (fun calls (case, _) -> calls_exp calls case) (calls_exp calls cond) cases
      | StmtBlock _ ->
          calls )
    calls s

let rec writes_nonlocal_lexp (l : lexp) =
  match l.l with
  | LMember _ | LIndex _ ->
      true
  | LTuple elems ->
      CCList.exists writes_nonlocal_lexp elems
  | LWild | LId _ ->
      false

let rec writes_nonlocal_stmt (s : stmt) =
  match s.s with
  | StmtBind (lhs, _) ->
      writes_nonlocal_lexp lhs
  | StmtBlock stmts ->
      CCList.exists writes_nonlocal_stmt stmts
  | StmtIf (_, then_, else_) ->
      writes_nonlocal_stmt then_ || Option.fold ~none:false ~some:writes_nonlocal_stmt else_
  | StmtWhile (_, body) ->
      writes_nonlocal_stmt body
  | StmtSwitch (_, cases, default) ->
      CCList.exists (fun (_, body) -> writes_nonlocal_stmt body) cases
      || Option.fold ~none:false ~some:writes_nonlocal_stmt default
  | StmtDecl _ | StmtReturn _ ->
      false

let analyze_pure_functions prog =
  let definitions =
    CCList.filter_map
      (fun (top : top_stmt) ->
        match top.top with
        | TopFunction (def, body)
          when CCList.for_all (fun (param : param) -> scalar_type param.t) def.args
               && scalar_type (snd def.t)
               && not (writes_nonlocal_stmt body) ->
            Some (def.name, body)
        | _ ->
            None )
      prog
  in
  let rec refine candidates =
    let candidates' =
      CCList.fold_left
        (fun result (name, body) ->
          if
            Set.mem name candidates
            && Set.for_all (fun call -> pure_builtin call || Set.mem call candidates) (calls_stmt Set.empty body)
          then Set.add name result
          else result )
        Set.empty definitions
    in
    if Set.equal candidates candidates' then candidates else refine candidates'
  in
  refine (CCList.fold_left (fun names (name, _) -> Set.add name names) Set.empty definitions)

let rec pure_exp pure_functions (e : exp) =
  match e.e with
  | ECall {path; args} ->
      (pure_builtin path || Set.mem path pure_functions) && CCList.for_all (pure_exp pure_functions) args
  | EUnOp (_, e) | EMember (e, _) | ETMember (e, _) ->
      pure_exp pure_functions e
  | EOp (_, lhs, rhs) | EIndex {e= lhs; index= rhs} ->
      pure_exp pure_functions lhs && pure_exp pure_functions rhs
  | EArray elems | ETuple elems ->
      CCList.for_all (pure_exp pure_functions) elems
  | EIf {cond; then_; else_} ->
      pure_exp pure_functions cond && pure_exp pure_functions then_ && pure_exp pure_functions else_
  | ERecord {elems; _} ->
      CCList.for_all (fun (_, e) -> pure_exp pure_functions e) elems
  | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ ->
      true

let cacheable_call pure_functions (e : exp) =
  match e.e with ECall {path; _} -> pure_builtin path || Set.mem path pure_functions | _ -> false

let add_count counts e =
  let count = Option.value ~default:0 (ExpMap.find_opt e counts) in
  ExpMap.add e (count + 1) counts

let rec count_exp pure_functions evaluation counts (e : exp) =
  let counts =
    match evaluation with
    | Always when cacheable_call pure_functions e ->
        add_count counts e
    | Always | Conditional ->
        counts
  in
  match e.e with
  | ECall {args; _} | EArray args | ETuple args ->
      CCList.fold_left (count_exp pure_functions evaluation) counts args
  | EUnOp (_, e) | EMember (e, _) | ETMember (e, _) ->
      count_exp pure_functions evaluation counts e
  (* The right side of a short-circuit operator may never run. *)
  | EOp ((OpLand | OpLor), lhs, rhs) ->
      count_exp pure_functions Conditional (count_exp pure_functions evaluation counts lhs) rhs
  | EOp (_, lhs, rhs) | EIndex {e= lhs; index= rhs} ->
      count_exp pure_functions evaluation (count_exp pure_functions evaluation counts lhs) rhs
  | EIf {cond; then_; else_} ->
      let counts = count_exp pure_functions evaluation counts cond in
      let counts = count_exp pure_functions Conditional counts then_ in
      count_exp pure_functions Conditional counts else_
  | ERecord {elems; _} ->
      CCList.fold_left (fun counts (_, e) -> count_exp pure_functions evaluation counts e) counts elems
  | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ ->
      counts

let count_direct_stmt pure_functions counts (s : stmt) =
  match s.s with
  | StmtDecl (_, init) ->
      Option.fold ~none:counts ~some:(count_exp pure_functions Always counts) init
  | StmtBind (_, rhs) | StmtReturn rhs ->
      count_exp pure_functions Always counts rhs
  | StmtIf (cond, _, _) | StmtSwitch (cond, _, _) ->
      count_exp pure_functions Always counts cond
  | StmtWhile _ | StmtBlock _ ->
      counts

let rec names_lexp names (l : lexp) =
  match l.l with
  | LId name ->
      Set.add name names
  | LMember (base, _) ->
      names_lexp names base
  | LIndex {e; _} ->
      names_lexp names e
  | LTuple elems ->
      CCList.fold_left names_lexp names elems
  | LWild ->
      names

let names_stmt names s =
  fold_stmt
    (fun names (s : stmt) ->
      match s.s with
      | StmtDecl ({d= DId (name, _); _}, _) ->
          Set.add name names
      | StmtBind (lhs, _) ->
          names_lexp names lhs
      | _ ->
          names )
    names s

let fresh_name used counter =
  let rec loop () =
    let name = "_cse_temp_" ^ string_of_int !counter in
    incr counter ;
    if Set.mem name !used then loop ()
    else (
      used := Set.add name !used ;
      name )
  in
  loop ()

let cache_after_stmt pure_functions cache (s : stmt) =
  let calls = calls_stmt Set.empty s in
  if Set.exists (fun call -> not (pure_builtin call || Set.mem call pure_functions)) calls then ExpMap.empty
  else
    let writes = match s.s with StmtBind (lhs, _) -> GetVariables.in_lexp lhs | _ -> Set.empty in
    ExpMap.filter (fun _ cached -> Set.is_empty (Set.inter writes cached.dependencies)) cache

let run_cse pure_functions prog =
  let used =
    ref
      (CCList.fold_left
         (fun names (top : top_stmt) ->
           match top.top with
           | TopFunction (def, body) ->
               let names = CCList.fold_left (fun names (p : param) -> Set.add p.name names) names def.args in
               names_stmt names body
           | _ ->
               names )
         Set.empty prog )
  in
  let counter = ref 0 in
  let rec rewrite_exp counts evaluation cache (e : exp) =
    (* Prefixes and elements are accumulated in reverse so that a long
       argument list does not cost a quadratic number of appends. *)
    let rewrite_list cache elems =
      let cache, prefix, elems =
        CCList.fold_left
          (fun (cache, prefix, elems) elem ->
            let cache, before, elem = rewrite_exp counts evaluation cache elem in
            (cache, CCList.rev_append before prefix, elem :: elems) )
          (cache, [], []) elems
      in
      (cache, CCList.rev prefix, CCList.rev elems)
    in
    let cache, prefix, e =
      match e.e with
      | ECall ({args; _} as call) ->
          let cache, prefix, args = rewrite_list cache args in
          (cache, prefix, {e with e= ECall {call with args}})
      | EArray elems ->
          let cache, prefix, elems = rewrite_list cache elems in
          (cache, prefix, {e with e= EArray elems})
      | ETuple elems ->
          let cache, prefix, elems = rewrite_list cache elems in
          (cache, prefix, {e with e= ETuple elems})
      | EUnOp (op, arg) ->
          let cache, prefix, arg = rewrite_exp counts evaluation cache arg in
          (cache, prefix, {e with e= EUnOp (op, arg)})
      | EMember (base, member) ->
          let cache, prefix, base = rewrite_exp counts evaluation cache base in
          (cache, prefix, {e with e= EMember (base, member)})
      | ETMember (base, member) ->
          let cache, prefix, base = rewrite_exp counts evaluation cache base in
          (cache, prefix, {e with e= ETMember (base, member)})
      | EOp (((OpLand | OpLor) as op), lhs, rhs) ->
          let cache, prefix, lhs = rewrite_exp counts evaluation cache lhs in
          let _, _, rhs = rewrite_exp counts Conditional ExpMap.empty rhs in
          (cache, prefix, {e with e= EOp (op, lhs, rhs)})
      | EOp (op, lhs, rhs) ->
          let cache, prefix1, lhs = rewrite_exp counts evaluation cache lhs in
          let cache, prefix2, rhs = rewrite_exp counts evaluation cache rhs in
          (cache, prefix1 @ prefix2, {e with e= EOp (op, lhs, rhs)})
      | EIndex {e= base; index} ->
          let cache, prefix1, base = rewrite_exp counts evaluation cache base in
          let cache, prefix2, index = rewrite_exp counts evaluation cache index in
          (cache, prefix1 @ prefix2, {e with e= EIndex {e= base; index}})
      | EIf {cond; then_; else_} ->
          let cache, prefix, cond = rewrite_exp counts evaluation cache cond in
          let _, _, then_ = rewrite_exp counts Conditional ExpMap.empty then_ in
          let _, _, else_ = rewrite_exp counts Conditional ExpMap.empty else_ in
          (cache, prefix, {e with e= EIf {cond; then_; else_}})
      | ERecord ({elems; _} as record) ->
          let cache, prefix, elems =
            CCList.fold_left
              (fun (cache, prefix, elems) (name, elem) ->
                let cache, before, elem = rewrite_exp counts evaluation cache elem in
                (cache, CCList.rev_append before prefix, (name, elem) :: elems) )
              (cache, [], []) elems
          in
          (cache, CCList.rev prefix, {e with e= ERecord {record with elems= CCList.rev elems}})
      | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ ->
          (cache, [], e)
    in
    let repeated = Option.value ~default:0 (ExpMap.find_opt e counts) > 1 in
    match evaluation with
    | Always when repeated && cacheable_call pure_functions e -> (
      match ExpMap.find_opt e cache with
      | Some cached ->
          (cache, prefix, {e= EId cached.name; t= e.t; loc= e.loc})
      | None ->
          let name = fresh_name used counter in
          let value = {e= EId name; t= e.t; loc= e.loc} in
          let decl = {s= StmtDecl ({d= DId (name, None); t= e.t; loc= e.loc}, Some e); loc= e.loc} in
          let cached = {name; dependencies= GetVariables.in_exp e} in
          (ExpMap.add e cached cache, prefix @ [decl], value) )
    | Always | Conditional ->
        (cache, prefix, e)
  and rewrite_nested (s : stmt) =
    match s.s with
    | StmtBlock stmts ->
        {s with s= StmtBlock (rewrite_block stmts)}
    | _ ->
        Mapper.block (rewrite_block [s]) s.loc
  and rewrite_stmt counts cache (s : stmt) =
    match s.s with
    | StmtDecl (decl, Some init) ->
        let cache, prefix, init = rewrite_exp counts Always cache init in
        let stmt = {s with s= StmtDecl (decl, Some init)} in
        (cache_after_stmt pure_functions cache stmt, prefix @ [stmt])
    | StmtBind (lhs, rhs) ->
        let cache, prefix, rhs = rewrite_exp counts Always cache rhs in
        let stmt = {s with s= StmtBind (lhs, rhs)} in
        (cache_after_stmt pure_functions cache stmt, prefix @ [stmt])
    | StmtReturn rhs ->
        let cache, prefix, rhs = rewrite_exp counts Always cache rhs in
        let stmt = {s with s= StmtReturn rhs} in
        (cache_after_stmt pure_functions cache stmt, prefix @ [stmt])
    | StmtIf (cond, then_, else_) ->
        let _, prefix, cond = rewrite_exp counts Always cache cond in
        let then_ = rewrite_nested then_ in
        let else_ = Option.map rewrite_nested else_ in
        (ExpMap.empty, prefix @ [{s with s= StmtIf (cond, then_, else_)}])
    | StmtWhile (cond, body) ->
        let body = rewrite_nested body in
        (ExpMap.empty, [{s with s= StmtWhile (cond, body)}])
    | StmtSwitch (cond, cases, default) ->
        let _, prefix, cond = rewrite_exp counts Always cache cond in
        let cases = CCList.map (fun (case, body) -> (case, rewrite_nested body)) cases in
        let default = Option.map rewrite_nested default in
        (ExpMap.empty, prefix @ [{s with s= StmtSwitch (cond, cases, default)}])
    | StmtBlock stmts ->
        (ExpMap.empty, [{s with s= StmtBlock (rewrite_block stmts)}])
    | StmtDecl (_, None) ->
        (cache, [s])
  and rewrite_block stmts =
    let counts = CCList.fold_left (count_direct_stmt pure_functions) ExpMap.empty stmts in
    CCList.fold_left
      (fun (cache, result) stmt ->
        let cache, stmts = rewrite_stmt counts cache stmt in
        (cache, CCList.rev_append stmts result) )
      (ExpMap.empty, []) stmts
    |> snd |> CCList.rev
  in
  CCList.map
    (fun (top : top_stmt) ->
      match top.top with TopFunction (def, body) -> {top with top= TopFunction (def, rewrite_nested body)} | _ -> top )
    prog

let local_declarations locals s =
  fold_stmt
    (fun locals (s : stmt) -> match s.s with StmtDecl ({d= DId (name, _); _}, _) -> Set.add name locals | _ -> locals)
    locals s

let run_dead_stores pure_functions prog =
  let rec references_lexp references (l : lexp) =
    match l.l with
    | LId name ->
        Set.add name references
    | LMember (base, _) ->
        references_lexp references base
    | LIndex {e= base; index} ->
        Set.union (references_lexp references base) (GetVariables.in_exp index)
    | LTuple elems ->
        CCList.fold_left references_lexp references elems
    | LWild ->
        references
  in
  let references_stmt references s =
    fold_stmt
      (fun references (s : stmt) ->
        match s.s with
        | StmtDecl (_, init) ->
            Option.fold ~none:references ~some:(fun init -> Set.union references (GetVariables.in_exp init)) init
        | StmtBind (lhs, rhs) ->
            Set.union (references_lexp references lhs) (GetVariables.in_exp rhs)
        | StmtReturn e ->
            Set.union references (GetVariables.in_exp e)
        | StmtIf (cond, _, _) | StmtWhile (cond, _) ->
            Set.union references (GetVariables.in_exp cond)
        | StmtSwitch (cond, cases, _) ->
            CCList.fold_left
              (fun references (case, _) -> Set.union references (GetVariables.in_exp case))
              (Set.union references (GetVariables.in_exp cond))
              cases
        | StmtBlock _ ->
            references )
      references s
  in
  let rec remove_unused_declarations references (s : stmt) =
    match s.s with
    | StmtDecl ({d= DId (name, _); _}, None) when not (Set.mem name references) ->
        None
    | StmtBlock stmts ->
        Some {s with s= StmtBlock (CCList.filter_map (remove_unused_declarations references) stmts)}
    | StmtIf (cond, then_, else_) ->
        let then_ =
          Option.value ~default:{s= StmtBlock []; loc= then_.loc} (remove_unused_declarations references then_)
        in
        let else_ = Option.bind else_ (remove_unused_declarations references) in
        Some {s with s= StmtIf (cond, then_, else_)}
    | StmtWhile (cond, body) ->
        let body =
          Option.value ~default:{s= StmtBlock []; loc= body.loc} (remove_unused_declarations references body)
        in
        Some {s with s= StmtWhile (cond, body)}
    | StmtSwitch (cond, cases, default) ->
        let cases =
          CCList.map
            (fun (case, (body : stmt)) ->
              let body =
                Option.value ~default:{s= StmtBlock []; loc= body.loc} (remove_unused_declarations references body)
              in
              (case, body) )
            cases
        in
        let default = Option.bind default (remove_unused_declarations references) in
        Some {s with s= StmtSwitch (cond, cases, default)}
    | _ ->
        Some s
  in
  let rec optimize_stmt locals live_out (s : stmt) =
    match s.s with
    | StmtBind (({l= LId name; _} as lhs), rhs) when Set.mem name locals && not (Set.mem name live_out) ->
        if pure_exp pure_functions rhs then (live_out, None)
        else (Set.union live_out (GetVariables.in_exp rhs), Some {s with s= StmtBind ({lhs with l= LWild}, rhs)})
    | StmtBind (lhs, rhs) ->
        let live =
          match lhs.l with
          | LId name ->
              Set.union (Set.remove name live_out) (GetVariables.in_exp rhs)
          | _ ->
              Set.union live_out (Set.union (GetVariables.in_lexp lhs) (GetVariables.in_exp rhs))
        in
        (live, Some s)
    | StmtDecl (({d= DId (name, _); _} as decl), init) ->
        let live = Set.remove name live_out in
        let live = Option.fold ~none:live ~some:(fun init -> Set.union live (GetVariables.in_exp init)) init in
        let init =
          match init with
          | Some init when (not (Set.mem name live_out)) && pure_exp pure_functions init ->
              None
          | _ ->
              init
        in
        (live, Some {s with s= StmtDecl (decl, init)})
    | StmtReturn e ->
        (Set.union live_out (GetVariables.in_exp e), Some s)
    | StmtBlock stmts ->
        let live, stmts = optimize_block locals live_out stmts in
        (live, Some {s with s= StmtBlock stmts})
    | StmtIf (cond, then_, else_) ->
        let then_loc = then_.loc in
        let then_live, then_ = optimize_stmt locals live_out then_ in
        let else_live, else_ =
          match else_ with
          | None ->
              (live_out, None)
          | Some else_ ->
              let live, else_ = optimize_stmt locals live_out else_ in
              (live, else_)
        in
        let then_ = Option.value ~default:{s= StmtBlock []; loc= then_loc} then_ in
        let live = Set.union (GetVariables.in_exp cond) (Set.union then_live else_live) in
        (live, Some {s with s= StmtIf (cond, then_, else_)})
    | StmtSwitch (cond, cases, default) ->
        let lives, cases =
          CCList.fold_left
            (fun (lives, cases) (case, (body : stmt)) ->
              let body_loc = body.loc in
              let live, body = optimize_stmt locals live_out body in
              let body = Option.value ~default:{s= StmtBlock []; loc= body_loc} body in
              (Set.union lives (Set.union live (GetVariables.in_exp case)), (case, body) :: cases) )
            (live_out, []) cases
        in
        let lives, default =
          match default with
          | None ->
              (lives, None)
          | Some default ->
              let live, default' = optimize_stmt locals live_out default in
              (Set.union lives live, default')
        in
        (Set.union lives (GetVariables.in_exp cond), Some {s with s= StmtSwitch (cond, CCList.rev cases, default)})
    | StmtWhile (cond, body) ->
        (* A loop requires a dataflow fixed point. Keep its body intact and
           treat every referenced variable as live at the loop boundary. *)
        let vars = GetVariables.in_stmts [s] in
        (Set.union live_out vars, Some {s with s= StmtWhile (cond, body)})
  and optimize_block locals live_out stmts =
    CCList.fold_right
      (fun stmt (live, result) ->
        let live, stmt = optimize_stmt locals live stmt in
        (live, Option.fold ~none:result ~some:(fun stmt -> stmt :: result) stmt) )
      stmts (live_out, [])
  in
  CCList.map
    (fun (top : top_stmt) ->
      match top.top with
      | TopFunction (def, body) ->
          let body_loc = body.loc in
          let locals = local_declarations Set.empty body in
          let _, body = optimize_stmt locals Set.empty body in
          let body = Option.value ~default:{s= StmtBlock []; loc= body_loc} body in
          let references = references_stmt Set.empty body in
          let body =
            Option.value ~default:{s= StmtBlock []; loc= body_loc} (remove_unused_declarations references body)
          in
          {top with top= TopFunction (def, body)}
      | _ ->
          top )
    prog

let run prog =
  let pure_functions = analyze_pure_functions prog in
  prog |> run_cse pure_functions |> run_dead_stores pure_functions

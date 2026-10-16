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

(* The substitution built by type inference. Inference never modifies a type: unifying two types
   binds their variables in the substitution, which is threaded through the typechecker inside the
   environment. Once the whole program has been elaborated, [zonkProgram] replaces every variable
   by its binding so that the following phases only see resolved types.

   There are two kinds of variables:
   - type variables (TEUnbound, TENoReturn and TEOption) bound to types
   - constness variables (CVar) that are constant unless they are made mutable or are linked to
     another constness.

   An unbound variable can also have a record guess: when the members of a value are used before
   its type is known, the variable keeps the record types that have those members. Every use and
   every unification narrows the candidates, and the variable is bound once only one remains. *)

open Util
open Typed
module IntMap = Map.Make (Int)

type const_binding = ConstLink of constness | ConstMut

(* A member of a record type: its name, its type and whether it is a value type (a builtin type) *)
type record_member = {member: string; member_t: type_; value_type: bool}

(* A type that a guessed value can have: a record type with its members, or an enumeration *)
type candidate = {candidate_path: path; members: record_member list}

(* A member used on a value whose type is guessed: [t] is the type of the expression that uses it
   and [owner] the type of the value. When the member is read, its constness follows the owner. *)
type member_use = {name: string; t: type_; owner: type_; read: bool}

(* Whether the candidates of a guess are records, whose members are used, or enumerations that share
   the name of a value *)
type guess_kind = RecordGuess | EnumGuess

(* The types that the variable can be and the members used on it *)
type type_guess = {kind: guess_kind; candidates: candidate list; uses: member_use list; guess_loc: Loc.t}

(* The bindings are indexed by the number of the variable. Persistent arrays keep extending and
   reading the newest substitution in constant time. [guessed] holds the variables that got a
   record guess, to resolve them at the end of the inference. [supply] numbers the new variables. *)
type t =
  { types: type_ Parray.t
  ; consts: const_binding Parray.t
  ; guesses: type_guess Parray.t
  ; guessed: int list
  ; supply: supply }

let empty () : t =
  {types= Parray.empty (); consts= Parray.empty (); guesses= Parray.empty (); guessed= []; supply= initial_supply}

(* Creates a value with fresh variables *)
let fresh (s : t) (make : 'a fresh) : t * 'a =
  let supply, value = make s.supply in
  ({s with supply}, value)

(* The binding of a variable, without following the bindings of the result *)
let boundTo (s : t) (t : type_) : type_ option =
  match t.tx with
  | TENoReturn i | TEUnbound i | TEOption (i, _) ->
      Parray.get s.types i
  | TEId _ | TESize _ | TEComposed _ | TEFunction _ ->
      None

(* Follows the bindings until reaching a type that is not a bound variable *)
let rec walk (s : t) (t : type_) : type_ = match boundTo s t with Some bound -> walk s bound | None -> t

(* ========== Constness ========== *)

(* The end of a chain of constness links: a variable that is not linked or a fixed value *)
let rec constRoot (s : t) (c : constness) : constness =
  match c with
  | CVar i -> (
    match Parray.get s.consts i with Some (ConstLink c) -> constRoot s c | Some ConstMut | None -> c )
  | CConst | CMut ->
      c

(* Whether the root of a constness chain is mutable *)
let isMutRoot (s : t) (root : constness) : bool =
  match root with
  | CVar i -> (
    match Parray.get s.consts i with Some ConstMut -> true | Some (ConstLink _) | None -> false )
  | CMut ->
      true
  | CConst ->
      false

let isConstValue (s : t) (c : constness) : bool = not (isMutRoot s (constRoot s c))

let isConst (s : t) (t : type_) : bool = isConstValue s t.const

(* Makes the type constant or mutable. Fixed constness values cannot change. *)
let setConst (s : t) (t : type_) (const : bool) : t =
  match constRoot s t.const with
  | CVar i ->
      let consts = Parray.set s.consts i (if const then None else Some ConstMut) in
      {s with consts}
  | CConst | CMut ->
      s

let setMut (s : t) (t : type_) : t = setConst s t false

(* Makes both values share their constness. A mutable value stays mutable. *)
let unifyConstnessValue (s : t) (c1 : constness) (c2 : constness) : t =
  let link i root = {s with consts= Parray.set s.consts i (Some (ConstLink root))} in
  let root1 = constRoot s c1 in
  let root2 = constRoot s c2 in
  match (root1, root2) with
  | CVar i, CVar j when i = j ->
      s
  | CVar i, _ when not (isMutRoot s root1) ->
      link i root2
  | _, CVar j when not (isMutRoot s root2) ->
      link j root1
  | (CVar _ | CConst | CMut), (CVar _ | CConst | CMut) ->
      s

(* Shares the constness of the first type, and of every type it is bound to, with the second *)
let rec unifyConstness (s : t) (t1 : type_) (t2 : type_) : t =
  let s = unifyConstnessValue s t1.const t2.const in
  match (boundTo s t1, boundTo s t2) with
  | Some t1, _ ->
      unifyConstness s t1 t2
  | None, Some t2 ->
      unifyConstness s t1 t2
  | None, None ->
      s

(* A copy of the type that does not share the constness of the original *)
let rec refreshConstness (s : t) (t : type_) : t * type_ =
  let t = walk s t in
  let s, tx =
    match t.tx with
    | TEOption (_, options) ->
        let s, options = CCList.fold_left_map refreshConstness s options in
        let s, n = fresh s C.fresh in
        (s, TEOption (n, options))
    | TEComposed (name, subs) ->
        let s, subs = CCList.fold_left_map refreshConstness s subs in
        (s, TEComposed (name, subs))
    | (TENoReturn _ | TEUnbound _ | TEId _ | TESize _ | TEFunction _) as tx ->
        (s, tx)
  in
  let s, const = fresh s C.const in
  (s, {t with tx; const})

(* ========== Resolution ========== *)

(* The constness without links. A variable that is neither linked nor mutable is kept: it is
   constant. *)
let resolveConst (s : t) (c : constness) : constness =
  match c with
  | CConst | CMut ->
      c
  | CVar i -> (
    match Parray.get s.consts i with None -> c | Some _ -> if isConstValue s c then CConst else CMut )

(* Maps the list, returning the same list when [f] returns every element unchanged *)
let rec mapSharing (f : 'a -> 'a) (l : 'a list) : 'a list =
  match l with
  | [] ->
      l
  | h :: t ->
      let h' = f h in
      let t' = mapSharing f t in
      if h' == h && t' == t then l else h' :: t'

(* Replaces every bound variable in the type by its binding and resolves the constness. The parts of
   the type that do not change are returned as they are. *)
let rec zonk (s : t) (t : type_) : type_ =
  let const = resolveConst s t.const in
  match boundTo s t with
  | Some bound ->
      {(zonk s bound) with const}
  | None ->
      let tx =
        match t.tx with
        | TEOption (i, options) ->
            let resolved = mapSharing (zonk s) options in
            if resolved == options then t.tx else TEOption (i, resolved)
        | TEComposed (name, elems) ->
            let resolved = mapSharing (zonk s) elems in
            if resolved == elems then t.tx else TEComposed (name, resolved)
        | TEFunction (args, ret) ->
            let resolved_args = mapSharing (zonk s) args in
            let resolved_ret = zonk s ret in
            if resolved_args == args && resolved_ret == ret then t.tx else TEFunction (resolved_args, resolved_ret)
        | TENoReturn _ | TEUnbound _ | TEId _ | TESize _ ->
            t.tx
      in
      if tx == t.tx && const == t.const then t else {t with tx; const}

let printType (s : t) (t : type_) : Pla.t = print_type_ (zonk s t)

(* ========== Unification ========== *)

let rec occurs (s : t) (var : int) (t : type_) : bool =
  let t = walk s t in
  match t.tx with
  | TENoReturn i | TEUnbound i ->
      i = var
  | TEOption (i, options) ->
      i = var || CCList.exists (occurs s var) options
  | TEComposed (_, elems) ->
      CCList.exists (occurs s var) elems
  | TEFunction (args, ret) ->
      CCList.exists (occurs s var) args || occurs s var ret
  | TEId _ | TESize _ ->
      false

let sameVar (t1 : type_) (t2 : type_) : bool =
  match (t1.tx, t2.tx) with
  | (TENoReturn i | TEUnbound i | TEOption (i, _)), (TENoReturn j | TEUnbound j | TEOption (j, _)) ->
      i = j
  | _ ->
      false

(* Why two types do not unify *)
type unify_error =
  | Mismatch
  | NoCommonAlternative of {message: string; loc: Loc.t}
    (* Two options without alternatives in common. The message describes them. *)
  | NoRecordType of string list
(* No record type has the used members with the types they are used with *)

let ( let* ) = Result.bind

let recordType (s : t) (loc : Loc.t) (candidate : candidate) : t * type_ =
  fresh s (C.path_t loc candidate.candidate_path)

let samePath (p1 : path) (p2 : path) : bool = Pparser.Syntax.compare_path p1 p2 = 0

(* The member names used in the guess, without repetitions *)
let usedMembers (guess : type_guess) : string list =
  CCList.sort_uniq ~cmp:String.compare (CCList.map (fun (use : member_use) -> use.name) guess.uses)

(* Unifies the types. Returns the extended substitution or why they do not unify. Unification never
   raises errors, so it can be used to try alternatives and backtrack. When [bind] is set, the types
   also share their constness. *)
let rec unifyResult ?(bind = false) (s : t) (t1 : type_) (t2 : type_) : (t, unify_error) result =
  let s = if bind then unifyConstnessValue s t1.const t2.const else s in
  unifyTypes s t1 t2

and unifyTypes (s : t) (t1 : type_) (t2 : type_) : (t, unify_error) result =
  let t1 = walk s t1 in
  let t2 = walk s t2 in
  if t1 == t2 || sameVar t1 t2 then Ok s
  else
    match (t1.tx, t2.tx) with
    | TEId p1, TEId p2 ->
        if Pparser.Syntax.compare_path p1 p2 = 0 then Ok s else Error Mismatch
    | TESize n1, TESize n2 ->
        if n1 = n2 then Ok s else Error Mismatch
    | TEFunction (args1, ret1), TEFunction (args2, ret2) ->
        let* s = unifyList s args1 args2 in
        unifyTypes s ret1 ret2
    | TEFunction _, _ | _, TEFunction _ ->
        Error Mismatch
    (* special case for arrays without dimensions *)
    | TEComposed ("array", [e1; _]), TEComposed ("array", [e2])
    | TEComposed ("array", [e1]), TEComposed ("array", [e2; _]) ->
        unifyTypes s e1 e2
    | TEComposed (n1, e1), TEComposed (n2, e2) when String.equal n1 n2 ->
        unifyList s e1 e2
    | TENoReturn i, _ ->
        bindVar s t1 i t2
    | _, TENoReturn i ->
        bindVar s t2 i t1
    | TEUnbound i, _ ->
        bindVar s t1 i t2
    | _, TEUnbound i ->
        bindVar s t2 i t1
    | TEOption (i1, l1), TEOption (i2, l2) ->
        let* t3, s = constrainOption s t2.loc l1 l2 in
        let* s = bindVar s t2 i2 t3 in
        bindVar s t1 i1 t3
    | TEOption (i, l), _ ->
        pickOption s t1 i l t2
    | _, TEOption (i, l) ->
        pickOption s t2 i l t1
    | (TEId _ | TESize _ | TEComposed _), _ ->
        Error Mismatch

and unifyList (s : t) (l1 : type_ list) (l2 : type_ list) : (t, unify_error) result =
  match (l1, l2) with
  | [], [] ->
      Ok s
  | h1 :: t1, h2 :: t2 ->
      let* s = unifyTypes s h1 h2 in
      unifyList s t1 t2
  | [], _ :: _ | _ :: _, [] ->
      Error Mismatch

(* Tries to unify the type with the elements of the set. The counter of the first element it
   unifies with is increased, otherwise the type is added to the set. *)
and pushTypeToSet (s : t) (set : (type_ * int) list) (elem : type_) : t * (type_ * int) list =
  match set with
  | [] ->
      (s, [(elem, 1)])
  | ((h, count) as first) :: rest -> (
      let pushToRest () =
        let s, rest = pushTypeToSet s rest elem in
        (s, first :: rest)
      in
      match (h.tx, elem.tx) with
      (* Don't try to unify composed types with different arguments *)
      | TEComposed (n1, e1), TEComposed (n2, e2) when String.equal n1 n2 && CCList.length e1 <> CCList.length e2 ->
          pushToRest ()
      | _ -> (
        match unifyTypes s h elem with Ok s -> (s, (h, count + 1) :: rest) | Error _ -> pushToRest () ) )

(* The alternatives that two options have in common *)
and constrainOption (s : t) (loc : Loc.t) (l1 : type_ list) (l2 : type_ list) : (type_ * t, unify_error) result =
  let constrained, set =
    CCList.fold_left (fun (s, set) elem -> pushTypeToSet s set elem) (s, CCList.map (fun e -> (e, 1)) l1) l2
  in
  let final_set = CCList.filter_map (fun (e, n) -> if n > 1 then Some e else None) set in
  match final_set with
  | [] ->
      let t1 = Pla.map_sep Pla.commaspace (printType constrained) l1 in
      let t2 = Pla.map_sep Pla.commaspace (printType constrained) l2 in
      let message =
        Pla.print {%pla|None of the following types: <#t1#>, matches with any of the following types <#t2#>. |}
      in
      Error (NoCommonAlternative {message; loc})
  | [t] ->
      Ok (t, constrained)
  | l ->
      let constrained, t = fresh constrained (C.option Loc.default l) in
      Ok (t, constrained)

(* Binds the option [option] (whose number is [i]) to the type if it unifies with one of the
   alternatives *)
and pickOption (s : t) (option : type_) (i : int) (alternatives : type_ list) (t : type_) : (t, unify_error) result =
  match alternatives with
  | [] ->
      Error Mismatch
  | h :: rest -> (
    match unifyTypes s h t with Ok s -> bindVar s option i t | Error _ -> pickOption s option i rest t )

(* Binds the variable [var] (whose number is [i]) to the type. A type without location takes the
   location of the variable. When the variable has a record guess, the type must be one of the
   candidates, or the guess is passed to the variable it is bound to. *)
and bindVar (s : t) (var : type_) (i : int) (t : type_) : (t, unify_error) result =
  if occurs s i t then Error Mismatch
  else
    let t = if t.loc == Loc.default then {t with loc= var.loc} else t in
    let bound = {s with types= Parray.set s.types i (Some t)} in
    match Parray.get s.guesses i with
    | None ->
        Ok bound
    | Some guess -> (
        let bound = {bound with guesses= Parray.set bound.guesses i None} in
        let t = walk bound t in
        match t.tx with
        | TEUnbound j ->
            addGuess bound j guess
        | TEId path -> (
          match CCList.find_opt (fun candidate -> samePath candidate.candidate_path path) guess.candidates with
          | Some candidate ->
              useMembers bound candidate guess.uses
          | None ->
              Error Mismatch )
        | TENoReturn _ | TESize _ | TEOption _ | TEComposed _ | TEFunction _ ->
            Error Mismatch )

(* Unifies the types of the members used with the ones of the record *)
and useMembers (s : t) (candidate : candidate) (uses : member_use list) : (t, unify_error) result =
  let useMember s (use : member_use) =
    match CCList.find_opt (fun m -> String.equal m.member use.name) candidate.members with
    | None ->
        Error Mismatch
    | Some m ->
        let s, member_t = refreshConstness s m.member_t in
        let* s = unifyTypes s use.t member_t in
        if use.read && not m.value_type then Ok (unifyConstness s use.t use.owner) else Ok s
  in
  CCList.fold_left (fun s use -> Result.bind s (fun s -> useMember s use)) (Ok s) uses

(* Adds the guess to the unbound variable number [i], combining it with the one it has *)
and addGuess (s : t) (i : int) (guess : type_guess) : (t, unify_error) result =
  let guess =
    match Parray.get s.guesses i with
    | None ->
        guess
    | Some previous ->
        let candidates =
          CCList.filter
            (fun candidate ->
              CCList.exists (fun c -> samePath c.candidate_path candidate.candidate_path) guess.candidates )
            previous.candidates
        in
        {previous with candidates; uses= previous.uses @ guess.uses}
  in
  narrow s i guess

(* Keeps the candidates that fit the members used. The variable is bound when only one remains. *)
and narrow (s : t) (i : int) (guess : type_guess) : (t, unify_error) result =
  (* The uses of the same member have the same type *)
  let* s = unifySameMembers s guess.uses in
  let fits candidate = Result.is_ok (useMembers s candidate guess.uses) in
  match CCList.filter fits guess.candidates with
  | [] -> (
    match guess.kind with RecordGuess -> Error (NoRecordType (usedMembers guess)) | EnumGuess -> Error Mismatch )
  | [candidate] ->
      let s, record_t = recordType s guess.guess_loc candidate in
      let s = {s with types= Parray.set s.types i (Some record_t)} in
      let s = {s with guesses= Parray.set s.guesses i None} in
      useMembers s candidate guess.uses
  | candidates ->
      Ok {s with guesses= Parray.set s.guesses i (Some {guess with candidates}); guessed= i :: s.guessed}

and unifySameMembers (s : t) (uses : member_use list) : (t, unify_error) result =
  match uses with
  | [] ->
      Ok s
  | use :: rest ->
      let same = CCList.filter (fun (other : member_use) -> String.equal other.name use.name) rest in
      let unifyWith s (other : member_use) = Result.bind s (fun s -> unifyTypes s use.t other.t) in
      let* s = CCList.fold_left unifyWith (Ok s) same in
      unifySameMembers s rest

(* Gives the unbound variable [var] the candidates as possible types and uses the members on it *)
let guessType (s : t) (var : type_) (kind : guess_kind) (candidates : candidate list) (uses : member_use list)
    (loc : Loc.t) : (t, unify_error) result =
  match (walk s var).tx with
  | TEUnbound i ->
      addGuess s i {kind; candidates; uses; guess_loc= loc}
  | TENoReturn _ | TEId _ | TESize _ | TEOption _ | TEComposed _ | TEFunction _ ->
      Error Mismatch

(* Why the type of a guessed record could not be decided *)
type guess_failure =
  | AmbiguousType of {kind: guess_kind; candidates: path list; loc: Loc.t}
  | UndeterminedRecord of {members: string list; loc: Loc.t}

(* Decides the remaining record guesses: the ones that fit only one record are bound to it. Binding
   a variable can narrow other guesses, so it is repeated until nothing changes. *)
let resolveGuesses (s : t) : t * guess_failure list =
  let pending s =
    CCList.sort_uniq ~cmp:compare s.guessed
    |> CCList.filter_map (fun i ->
        match (Parray.get s.types i, Parray.get s.guesses i) with
        | None, Some guess ->
            Some (i, guess)
        | Some _, _ | None, None ->
            None )
  in
  let rec loop s =
    let before = pending s in
    let s, failed =
      CCList.fold_left
        (fun (s, failed) (i, guess) ->
          match narrow s i guess with Ok s -> (s, failed) | Error _ -> (s, (i, guess) :: failed) )
        (s, []) before
    in
    let after = pending s in
    if failed = [] && CCList.length after < CCList.length before then loop s
    else
      let undetermined =
        CCList.rev_map (fun (_, guess) -> UndeterminedRecord {members= usedMembers guess; loc= guess.guess_loc}) failed
      in
      let ambiguous =
        CCList.filter_map
          (fun (i, guess) ->
            if CCList.mem_assoc ~eq:Int.equal i failed then None
            else
              Some
                (AmbiguousType
                   { kind= guess.kind
                   ; candidates= CCList.map (fun c -> c.candidate_path) guess.candidates
                   ; loc= guess.guess_loc } ) )
          after
      in
      (s, undetermined @ ambiguous)
  in
  loop {s with guessed= CCList.sort_uniq ~cmp:compare s.guessed}

(* Unifies the types, returning None when they do not unify *)
let unify ?(bind = false) (s : t) (t1 : type_) (t2 : type_) : t option = Result.to_option (unifyResult ~bind s t1 t2)

(* ========== Generic instantiation ========== *)

(* Copies of types with fresh variables, used to instantiate generic functions. The copies made
   with the same mapping share their variables: an unbound variable that appears in several of the
   copied types is replaced by the same fresh variable everywhere. The mapping goes from the number
   of the original unbound variable to its copy. *)
type copy_mapping = type_ IntMap.t

let emptyCopyMapping : copy_mapping = IntMap.empty

let rec copyType ((s, mapping) : t * copy_mapping) (t : type_) : (t * copy_mapping) * type_ =
  let t = walk s t in
  (* A copy that does not depend on the copies of the inner types *)
  let copy make =
    let s, copy = fresh s make in
    ((s, mapping), copy)
  in
  match t.tx with
  | TEUnbound i -> (
    match IntMap.find_opt i mapping with
    | Some copy ->
        ((s, mapping), copy)
    | None ->
        let s, copy = fresh s (C.unbound t.loc) in
        ((s, IntMap.add i copy mapping), copy) )
  | TENoReturn _ ->
      copy (C.noreturn t.loc)
  | TEId path ->
      copy (C.withConst t.loc (TEId path))
  | TESize n ->
      copy (C.withConst t.loc (TESize n))
  | TEOption (_, options) ->
      let (s, mapping), options = copyTypes (s, mapping) options in
      let s, copy = fresh s (C.option t.loc options) in
      ((s, mapping), copy)
  | TEComposed (name, elems) ->
      let (s, mapping), elems = copyTypes (s, mapping) elems in
      let s, copy = fresh s (C.withConst t.loc (TEComposed (name, elems))) in
      ((s, mapping), copy)
  | TEFunction (args, ret) ->
      let state, args = copyTypes (s, mapping) args in
      let (s, mapping), ret = copyType state ret in
      let s, copy = fresh s (C.withConst t.loc (TEFunction (args, ret))) in
      ((s, mapping), copy)

and copyTypes (state : t * copy_mapping) (types : type_ list) : (t * copy_mapping) * type_ list =
  CCList.fold_left_map copyType state types

(* The copy of the type in the mapping, when the type is an unbound variable that was copied *)
let copyOf (s : t) (mapping : copy_mapping) (t : type_) : type_ option =
  match (walk s t).tx with TEUnbound i -> IntMap.find_opt i mapping | _ -> None

(* ========== Resolution of programs ========== *)

let zonkArg (s : t) (arg : arg) : arg = {arg with t= zonk s arg.t}

let zonkGenericParam (s : t) (param : generic_param) : generic_param =
  match param with
  | GParamFunction (name, t) ->
      GParamFunction (name, Option.map (zonk s) t)
  | GParamType _ ->
      param
  | GParamConstant (name, t, loc) ->
      GParamConstant (name, zonk s t, loc)

let rec zonkExp (s : t) (e : exp) : exp =
  let zexp = zonkExp s in
  let e_ =
    match e.e with
    | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ | EConst _ | ETypeIntrinsic _ | EEnum _ ->
        e.e
    | EIndex {e; index} ->
        EIndex {e= zexp e; index= zexp index}
    | EArray elems ->
        EArray (CCList.map zexp elems)
    | ECall {instance; path; args} ->
        ECall {instance; path; args= CCList.map zexp args}
    | EUnOp (op, e) ->
        EUnOp (op, zexp e)
    | EOp (op, e1, e2) ->
        EOp (op, zexp e1, zexp e2)
    | EIf {cond; then_; else_} ->
        EIf {cond= zexp cond; then_= zexp then_; else_= zexp else_}
    | ETuple elems ->
        ETuple (CCList.map zexp elems)
    | EMember (e, m) ->
        EMember (zexp e, m)
    | ERecord {elems} ->
        ERecord {elems= CCList.map (fun (n, v) -> (n, zexp v)) elems}
    | EGenCall {instance; generic_path; params} ->
        EGenCall {instance; generic_path; params= CCList.map (zonkCallParam s) params}
    | EGenCompanionCall {instance; companion_name; parent_generic_path; args} ->
        EGenCompanionCall {instance; companion_name; parent_generic_path; args= CCList.map zexp args}
  in
  {e with e= e_; t= zonk s e.t}

and zonkCallParam (s : t) (param : call_param) : call_param =
  match param with
  | CallArg {arg; value} ->
      CallArg {arg= zonkArg s arg; value= zonkExp s value}
  | CallGeneric {param; value} ->
      CallGeneric {param= zonkGenericParam s param; value= zonkExp s value}

let rec zonkLexp (s : t) (l : lexp) : lexp =
  let l_ =
    match l.l with
    | LWild | LId _ ->
        l.l
    | LMember (e, m) ->
        LMember (zonkLexp s e, m)
    | LIndex {e; index} ->
        LIndex {e= zonkLexp s e; index= zonkExp s index}
    | LTuple elems ->
        LTuple (CCList.map (zonkLexp s) elems)
  in
  {l with l= l_; t= zonk s l.t}

let rec zonkDexp (s : t) (d : dexp) : dexp =
  let d_ = match d.d with DWild | DId _ -> d.d | DTuple elems -> DTuple (CCList.map (zonkDexp s) elems) in
  {d with d= d_; t= zonk s d.t}

let rec zonkStmt (s : t) (st : stmt) : stmt =
  let s_ =
    match st.s with
    | StmtVal d ->
        StmtVal (zonkDexp s d)
    | StmtMem (d, tags) ->
        StmtMem (zonkDexp s d, tags)
    | StmtBind (l, e) ->
        StmtBind (zonkLexp s l, zonkExp s e)
    | StmtReturn e ->
        StmtReturn (zonkExp s e)
    | StmtBlock stmts ->
        StmtBlock (CCList.map (zonkStmt s) stmts)
    | StmtIf (cond, then_, else_) ->
        StmtIf (zonkExp s cond, zonkStmt s then_, Option.map (zonkStmt s) else_)
    | StmtWhile (cond, body) ->
        StmtWhile (zonkExp s cond, zonkStmt s body)
  in
  {st with s= s_}

let zonkFunType (s : t) ((args, ret) : fun_type) : fun_type = (CCList.map (zonk s) args, zonk s ret)

let rec zonkFunctionDef (s : t) (def : function_def) : function_def =
  let next = Option.map (fun (def, body) -> (zonkFunctionDef s def, zonkStmt s body)) def.next in
  {def with args= CCList.map (zonkArg s) def.args; t= zonkFunType s def.t; next}

let zonkTopStmt (s : t) (top : top_stmt) : top_stmt =
  let top_ =
    match top.top with
    | TopExternal (def, link) ->
        TopExternal (zonkFunctionDef s def, link)
    | TopFunction (def, body) ->
        TopFunction (zonkFunctionDef s def, zonkStmt s body)
    | TopType {path; members} ->
        TopType {path; members= CCList.map (fun (n, t, tags, loc) -> (n, zonk s t, tags, loc)) members}
    | TopConstant (path, dims, t, e, tags) ->
        TopConstant (path, dims, zonk s t, zonkExp s e, tags)
    | TopGenericPlaceholder _ | TopAlias _ | TopEnum _ ->
        top.top
  in
  {top with top= top_}

let zonkProgram (s : t) (prog : top_stmt list) : top_stmt list = CCList.map (zonkTopStmt s) prog

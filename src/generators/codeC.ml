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

open Code
open Config

let rec getBodyCost body =
   match body with
   | CSVar _ -> 1
   | CSConst _ -> 1
   | CSBind _ -> 1
   | CSFunction (_, _, _, body, _) -> 1 + getBodyCost body
   | CSReturn _ -> 1
   | CSWhile (_, body) -> 1 + getBodyCost body
   | CSBlock body -> List.fold_left (fun acc e -> acc + getBodyCost e) 1 body
   | CSIf (_, t, None) -> 1 + getBodyCost t
   | CSIf (_, t, Some e) -> 1 + getBodyCost t + getBodyCost e
   | CSType _ -> 0
   | CSExtFunc _ -> 0
   | CSAlias _ -> 0
   | CSEmpty -> 0
   | CSSwitch (_, cases, Some d) -> List.fold_left (fun acc (_, e) -> acc + getBodyCost e) (1 + getBodyCost d) cases
   | CSSwitch (_, cases, None) -> List.fold_left (fun acc (_, e) -> acc + getBodyCost e) 1 cases
;;

let dot = Pla.map_sep (Pla.string ".") Pla.string

(** Returns true if the expression is simple and does not need parenthesis *)
let isSimple (e : cexp) : bool =
   match e with
   | CEInt _ | CEFloat _ | CEBool _ | CEString _ | CECall _ | CEIndex _ | CEAccess _ | CEVar _ -> true
   | _ -> false
;;

(** Returns the base type name and a list of its sizes *)
let rec simplifyArray (typ : type_descr) : string * string list =
   match typ with
   | CTSimple name -> name, []
   | CTArray (sub, size) ->
      let name, sub_size = simplifyArray sub in
      name, sub_size @ [ string_of_int size ]
;;

(** Returns the representation of a type description *)
let printTypeDescr (typ : type_descr) : Pla.t =
   let kind, sizes = simplifyArray typ in
   match sizes with
   | [] -> Pla.string kind
   | _ ->
      let tsize = Pla.map_sep Pla.comma Pla.string sizes in
      [%pla {|<#kind#s>[<#tsize#>]|}]
;;

(** Used to print declarations and rebindings of lhs variables *)
let printTypeAndName (is_decl : bool) (typ : type_descr) (name : string list) : Pla.t =
   match typ, name with
   | typ, [ name ] ->
      let kind, sizes = simplifyArray typ in
      (match is_decl, sizes with
       (* Simple varible declaration (no sizes) *)
       | true, [] -> [%pla {|<#kind#s> <#name#s>|}]
       (* Array declarations (with sizes) *)
       | true, _ ->
          let t_sizes = Pla.map_sep Pla.comma Pla.string sizes in
          [%pla {|<#kind#s> <#name#s>[<#t_sizes#>]|}]
       (* Simple rebinding (no declaration) *)
       | _, _ -> [%pla {|<#name#s>|}])
   | _ -> failwith "CodeC.printTypeAndName: invalid input"
;;

(** Used to print assignments of a tuple field to a variable *)
let printLhsExpTuple (var : Pla.t) (is_var : bool) (i : int) (e : clhsexp) : Pla.t =
   match e with
   (* Assigning to a simple variable *)
   | CLId (CTSimple typ, name) ->
      let name_ = dot name in
      if is_var
      then (* with declaration *)
         [%pla {|<#typ#s> <#name_#> = <#var#>.field_<#i#i>;|}]
      else (* with no declaration *)
         [%pla {|<#name_#> = <#var#>.field_<#i#i>;|}]
   | CLId (typ, name) ->
      let tdecl = printTypeAndName is_var typ name in
      [%pla {|<#tdecl#> = <#var#>.field_<#i#i>;|}]
   | CLWild -> Pla.unit
   | _ -> failwith ("printLhsExpTuple: All other cases should be already covered\n" ^ Code.show_clhsexp e)
;;

let splitArray into elems =
   let rec loop current acc count elems =
      match elems with
      | [] -> List.rev (List.rev current :: acc)
      | h :: t when count < into -> loop (h :: current) acc (count + 1) t
      | h :: t -> loop [ h ] (List.rev current :: acc) 0 t
   in
   loop [] [] 0 elems
;;

(** Returns a template the print the expression *)
let rec printExp (params : params) (e : cexp) : Pla.t =
   match e with
   | CEEmpty -> Pla.unit
   | CEFloat (s, _) -> Pla.string s
   | CEInt n ->
      (* Parenthesize if it has a unary minus *)
      if n < 0 then Pla.parenthesize (Pla.int n) else Pla.int n
   | CEBool v -> Pla.string (if v then "true" else "false")
   | CEString s -> Pla.string_quoted s
   | CEArray (elems, _) ->
      let elems = splitArray 50 elems in
      let telems =
         Pla.map_sep Pla.comma (fun elems -> Pla.append (Pla.map_sep Pla.comma (printExp params) elems) Pla.newline) elems
      in
      [%pla {|{<#telems#>}|}]
   | CECall (name, args, _) ->
      let targs = Pla.map_sep Pla.comma (printExp params) args in
      [%pla {|<#name#s>(<#targs#>)|}]
   | CEUnOp (op, e, _) ->
      let te = printExp params e in
      [%pla {|(<#op#s> <#te#>)|}]
   | CEOp (op, elems, _) ->
      let sop = [%pla {| <#op#s> |}] in
      let telems = Pla.map_sep sop (printExp params) elems in
      [%pla {|(<#telems#>)|}]
   | CEVar (name, _) -> Pla.string name
   | CEIndex (e, index, _) when params.template = "arduino" ->
      let index = printExp params index in
      let e = printExp params e in
      [%pla {|pgm_read_word_near(<#e#> + <#index#>)|}]
   | CEIndex (e, index, _) ->
      let index = printExp params index in
      let e = printExp params e in
      [%pla {|<#e#>[<#index#>]|}]
   | CEIf (cond, then_, else_, _) ->
      let tcond = printExp params cond in
      let tthen = printExp params then_ in
      let telse = printExp params else_ in
      [%pla {|(<#tcond#>?<#tthen#>:<#telse#>)|}]
   | CETuple (elems, _) ->
      let telems = Pla.map_sep Pla.comma (printChField params) elems in
      [%pla {|{ <#telems#> }|}]
   | CEAccess (((CEVar _ | CEAccess _) as e), n) ->
      let e = printExp params e in
      [%pla {|<#e#>.<#n#s>|}]
   | CEAccess (e, n) ->
      let e = printExp params e in
      [%pla {|(<#e#>).<#n#s>|}]

(** Used to print the elements of a tuple *)
and printChField (params : params) ((name : string), (value : cexp)) =
   let tval = printExp params value in
   [%pla {|.<#name#s> = <#tval#>|}]

(** Prints lhs values with and without declaration *)
and printLhsExp params (is_var : bool) (e : clhsexp) : Pla.t =
   match e with
   | CLId (typ, name) -> printTypeAndName is_var typ name
   (* if it was an '_' do not print anything *)
   | CLWild -> Pla.unit
   | CLIndex (CTSimple typ, [ name ], index) when is_var ->
      let index = printExp params index in
      [%pla {|<#typ#s> <#name#s>[<#index#>]|}]
   | CLIndex (typ, name, _) when is_var ->
      let name = dot name in
      let typ, sizes = simplifyArray typ in
      let sizes_t = Pla.map_join (fun i -> [%pla {|[<#i#s>]|}]) sizes in
      [%pla {|<#typ#s> <#name#><#sizes_t#>|}]
   | CLIndex (CTSimple _, [ name ], index) ->
      let index = printExp params index in
      [%pla {|<#name#s>[<#index#>]|}]
   | _ -> failwith "uncovered case"
;;

(** Used to print assignments on to an array element *)
let printArrayBinding params (var : string list) (i : int) (e : cexp) : Pla.t =
   let te = printExp params e in
   let var = dot var in
   [%pla {|<#var#>[<#i#i>] = <#te#>; |}]
;;

(** Prints arguments to functions either pass by value or reference *)
let printFunArg (ntype, name) : Pla.t =
   match ntype with
   | Var typ ->
      let tdescr = printTypeDescr typ in
      [%pla {|<#tdescr#> <#name#s>|}]
   | Ref (CTArray (typ, size)) ->
      let tdescr = printTypeDescr typ in
      [%pla {|<#tdescr#> (&<#name#s>)[<#size#i>]|}]
   | Ref typ ->
      let tdescr = printTypeDescr typ in
      [%pla {|<#tdescr#> &<#name#s>|}]
;;

let rec printSwitchStmt params e cases def =
   let e_t = printExp params e in
   let cases_t =
      Pla.map_sep_all
         Pla.newline
         (fun (v, stmt) ->
             let v_t = printExp params v in
             let stmt_t = CCOpt.get_or ~default:Pla.unit (printStmt params stmt) in
             [%pla {|case <#v_t#>:<#stmt_t#+><#>break;|}])
         cases
   in
   let def_t =
      match def with
      | None -> Pla.unit
      | Some s ->
         (match printStmt params s with
          | None -> Pla.unit
          | Some s -> [%pla {|default: <#s#+>|}])
   in
   Some [%pla {|switch(<#e_t#>) {<#cases_t#+> <#def_t#><#>}|}]

(** Print a statement *)
and printStmt (params : params) (stmt : cstmt) : Pla.t option =
   match stmt with
   (* Strange case '_' *)
   | CSVar (CLWild, None) -> None
   (* Prints type x; *)
   | CSVar (((CLId _ | CLIndex _) as lhs), None) ->
      let tlhs = printLhsExp params true lhs in
      Some [%pla {|<#tlhs#>;|}]
   (* All other cases of assigning tuples will be wrong *)
   | CSVar (CLTuple _, None) -> failwith "CodeC.printStmt: invalid tuple assign"
   | CSVar (_, _) -> failwith "printStmt: in c code generation there should not be initializations"
   (* Prints _ = ... *)
   | CSBind (CLWild, value) ->
      let te = printExp params value in
      Some [%pla {|<#te#>;|}]
   (* Print (x, y, z) = ... *)
   | CSBind (CLTuple elems, ((CEVar _ | CEAccess _) as rhs)) ->
      let rhs = printExp params rhs in
      let t = List.mapi (printLhsExpTuple rhs false) elems |> Pla.join in
      Some t
   (* All other cases of assigning tuples will be wrong *)
   | CSBind (CLTuple _, _) -> failwith "CodeC.printStmt: invalid tuple assign"
   (* Prints x = [ ... ] *)
   | CSBind (CLId (_, name), CEArray (elems, _)) ->
      let t = List.mapi (printArrayBinding params name) elems |> Pla.join in
      Some t
   (* Prints x = ... *)
   | CSBind (CLId (_, name), value) ->
      let te = printExp params value in
      let name = dot name in
      Some [%pla {|<#name#> = <#te#>;|}]
   | CSBind (CLIndex (_, name, index), value) ->
      let te = printExp params value in
      let name = dot name in
      let index = printExp params index in
      Some [%pla {|<#name#>[<#index#>] = <#te#>;|}]
   (* Prints const x = ... *)
   | CSConst (lhs, ((CEInt _ | CEFloat _ | CEBool _ | CEArray _) as value)) ->
      if params.target_file = Tables
      then (
         let tlhs = printLhsExp params true lhs in
         let te = printExp params value in
         let progmem = if params.template = "arduino" then Pla.string "PROGMEM " else Pla.unit in
         Some [%pla {|static const <#tlhs#> <#progmem#>= <#te#>;|}])
      else None
   (* All other cases should be errors *)
   | CSConst _ -> failwith "printStmt: invalid constant declaration"
   (* Function declarations cotaining more than one statement *)
   | CSFunction (ntype, name, args, (CSBlock _ as body), _) ->
      let ret = printTypeDescr ntype in
      let targs = Pla.map_sep Pla.commaspace printFunArg args in
      let cost = getBodyCost body in
      (* if we are printing a header, skip the body *)
      if cost < 6
      then
         if params.target_file = Header
         then (
            match printStmt params body with
            | Some tbody -> Some [%pla {|static_inline <#ret#> <#name#s>(<#targs#>)<#tbody#><#>|}]
            (* Covers the case when the body is empty *)
            | None -> Some [%pla {|static_inline <#ret#> <#name#s>(<#targs#>){};<#>|}])
         else None
      else if params.target_file = Header
      then Some [%pla {|<#ret#> <#name#s>(<#targs#>);<#>|}]
      else if params.target_file = Implementation
      then (
         match printStmt params body with
         | Some tbody -> Some [%pla {|<#ret#> <#name#s>(<#targs#>)<#tbody#><#>|}]
         (* Covers the case when the body is empty *)
         | None -> Some [%pla {|<#ret#> <#name#s>(<#targs#>){};<#>|}])
      else None
   (* Function declarations cotaining a single return *)
   | CSFunction (ntype, name, args, (CSReturn _ as body), _) ->
      let ret = printTypeDescr ntype in
      let targs = Pla.map_sep Pla.commaspace printFunArg args in
      (* if we are printing a header, skip the body *)
      if params.target_file = Header
      then (
         let tbody = CCOpt.get_or ~default:Pla.unit (printStmt params body) in
         Some [%pla {|static_inline <#ret#> <#name#s>(<#targs#>){<#tbody#+><#>};<#>|}])
      else None
   (* Function declarations cotaining a single statement *)
   | CSFunction (ntype, name, args, body, _) ->
      let ret = printTypeDescr ntype in
      let targs = Pla.map_sep Pla.commaspace printFunArg args in
      (* if we are printing a header, skip the body *)
      let cost = getBodyCost body in
      if cost < 6
      then
         if params.target_file = Header
         then (
            let tbody = CCOpt.get_or ~default:Pla.unit (printStmt params body) in
            Some [%pla {|static_inline <#ret#> <#name#s>(<#targs#>){<#tbody#+><#>};<#>|}])
         else None
      else if params.target_file = Header
      then Some [%pla {|<#ret#> <#name#s>(<#targs#>);<#>|}]
      else if params.target_file = Implementation
      then (
         let tbody = CCOpt.get_or ~default:Pla.unit (printStmt params body) in
         Some [%pla {|<#ret#> <#name#s>(<#targs#>){<#tbody#+><#>}<#>|}])
      else None
   (* Prints return x *)
   | CSReturn e1 ->
      let te = printExp params e1 in
      Some [%pla {|return <#te#>;|}]
   (* Printf while(cond) ... *)
   | CSWhile (cond, body) ->
      let tcond = printExp params cond in
      let tcond = if isSimple cond then Pla.parenthesize tcond else tcond in
      let tbody = CCOpt.get_or ~default:Pla.semi (wrapStmtIfNotBlock params body) in
      Some [%pla {|while<#tcond#><#tbody#>|}]
   (* Prints a block of statements*)
   | CSBlock elems ->
      let telems = printStmtList params elems in
      Some [%pla {|{<#telems#+>}|}]
   (* If-statement without an else*)
   | CSIf (cond, then_, None) ->
      let tcond = printExp params cond in
      let tcond = if isSimple cond then Pla.wrap (Pla.string "(") (Pla.string ")") tcond else tcond in
      let tthen = CCOpt.get_or ~default:Pla.semi (wrapStmtIfNotBlock params then_) in
      Some [%pla {|if<#tcond#><#tthen#>|}]
   (* If-statement with else*)
   | CSIf (cond, then_, Some else_) ->
      let tcond = printExp params cond in
      let tcond = if isSimple cond then Pla.wrap (Pla.string "(") (Pla.string ")") tcond else tcond in
      let tthen = CCOpt.get_or ~default:Pla.semi (wrapStmtIfNotBlock params then_) in
      let telse = CCOpt.get_or ~default:Pla.semi (wrapStmtIfNotBlock params else_) in
      Some [%pla {|if<#tcond#><#tthen#><#>else<#><#telse#>|}]
   (* Type declaration (only in headers) *)
   | CSType (name, members, _) when params.target_file = Header ->
      let tmembers =
         Pla.map_sep_all
            Pla.newline
            (fun (typ, name) ->
                let tmember = printTypeAndName true typ [ name ] in
                [%pla {|<#tmember#>;|}])
            members
      in
      Some [%pla {|typedef struct <#name#s> {<#tmembers#+>} <#name#s>;<#>|}]
   (* Do not print type delcarations in implementation file *)
   | CSType (_, _, _) -> None
   (* Type declaration aliases (only in headers) *)
   | CSAlias (t1, t2) when params.target_file = Header ->
      let tdescr = printTypeDescr t2 in
      Some [%pla {|typedef <#t1#s> <#tdescr#>;<#>|}]
   (* Do not print type delcarations in implementation file *)
   | CSAlias (_, _) -> None
   (* External function definitions (only in headers) *)
   | CSExtFunc (ntype, name, args) when params.target_file = Header ->
      let ret = printTypeDescr ntype in
      let targs = Pla.map_sep Pla.commaspace printFunArg args in
      Some [%pla {|extern "C" <#ret#> <#name#s>(<#targs#>);|}]
   (* Do not print external function delcarations in implementation file *)
   | CSExtFunc _ -> None
   | CSEmpty -> None
   | CSSwitch (e, cases, def) -> printSwitchStmt params e cases def

and printStmtList (params : params) (stmts : cstmt list) : Pla.t =
   (* Prints the statements and removes all elements that are None *)
   let tstmts = CCList.filter_map (printStmt params) stmts in
   Pla.map_sep_all Pla.newline (fun a -> a) tstmts

and wrapStmtIfNotBlock params stmt =
   match stmt with
   | CSBlock _ -> printStmt params stmt
   | _ ->
      (match printStmt params stmt with
       | Some t -> Some [%pla {|{<#t#+><#>}|}]
       | _ -> None)
;;

(** Generates the .c and .h file contents for the given parsed files *)
let print (params : params) (stmts : Code.cstmt list) : (Pla.t * FileKind.t) list =
   let h = printStmtList { params with target_file = Header } stmts in
   let cpp = printStmtList { params with target_file = Implementation } stmts in
   let tables = printStmtList { params with target_file = Tables } stmts in
   Templates.apply params h cpp tables
;;

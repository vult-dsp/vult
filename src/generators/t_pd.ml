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

(* Pure Data externals.

   The objects of the library are selected with tags:
   - [fun foo(...) @[pdtilde]] generates the signal object "foo~": the signature inputs are
     signal inlets and the outputs signal outlets, called once per sample.
   - [fun bar(...) @[pd]] generates the control object "bar": one inlet per argument, the
     right inlets store their value and the leftmost inlet triggers the call (a bang
     re-triggers with the stored values); one outlet per output.
   - [and myMsg(...) @[pdmessage]] chained to a tagged function adds the message
     [myMsg arg1 arg2( to the owning object.

   Every value received as a Pure Data atom is validated before the call: floats are
   accepted for numeric parameters and symbols for string parameters. *)
open Core.Prog
module Tags = Pparser.Ptags

type obj_kind = Tilde | Control

type message = {m_fname: string; m_selector: string; m_args: param list; m_loc: Util.Loc.t}

type obj =
  { kind: obj_kind
  ; fname: string (* prefixed function name: used for the internal C symbols *)
  ; class_name: string (* name of the object in Pure Data, without '~' *)
  ; setup_name: string (* exported setup symbol, matches what the pd loader derives *)
  ; ctx: string option (* path of the context struct shared with the messages *)
  ; inputs: param list
  ; outputs: type_ list
  ; messages: message list
  ; loc: Util.Loc.t }

(* The unprefixed source name of a function, e.g. "process" for "Synth_process". *)
let originalBaseName (def : function_def) =
  match def.info.original_name with
  | Some name -> (
    match CCString.split_on_char '.' name with [] -> def.name | parts -> CCList.hd (CCList.rev parts) )
  | None ->
      def.name

let contextPath (def : function_def) =
  match def.args with {name= "_ctx"; t= {t= TStruct {path; _}; _}; _} :: _ -> Some path | _ -> None

let nonCtxArgs (def : function_def) =
  match def.args with {name= "_ctx"; t= {t= TStruct _; _}; _} :: args -> args | args -> args

let outputTypes (def : function_def) =
  match def.t with
  | _, {t= TTuple elems; _} ->
      elems
  | _, {t= TVoid (Some elems); _} ->
      elems
  | _, {t= TVoid None; _} ->
      []
  | _, t ->
      [t]

let isNumericType (t : type_) = match t.t with TReal | TInt | TBool | TFix16 -> true | _ -> false

let isStringType (t : type_) = match t.t with TString -> true | _ -> false

let typeName (t : type_) =
  match t.t with
  | TReal ->
      "real"
  | TInt ->
      "int"
  | TBool ->
      "bool"
  | TFix16 ->
      "fix16"
  | TString ->
      "string"
  | _ ->
      "unsupported"

let typeString (t : type_) =
  match t.t with
  | TReal ->
      "float"
  | TInt ->
      "int"
  | TBool ->
      "bool"
  | TFix16 ->
      "fix16_t"
  | _ ->
      failwith "Pd.typeString: not a numeric type"

let isValidCIdentifier (name : string) =
  let validChar i c = match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | '0' .. '9' -> i > 0 | _ -> false in
  String.length name > 0 && CCString.foldi (fun acc i c -> acc && validChar i c) true name

(* Selectors that Pure Data reserves or that the generated objects register themselves. *)
let reservedSelectors (kind : obj_kind) =
  match kind with Tilde -> ["dsp"] | Control -> ["bang"; "float"; "symbol"; "list"; "anything"]

let castInput (typ : type_) (value : Pla.t) : Pla.t = Common.cast ~from:Core.Prog.C.real_t ~to_:typ value

let castOutput (typ : type_) (value : Pla.t) : Pla.t = Common.cast ~from:typ ~to_:Core.Prog.C.real_t value

(* ==== Collection of the tagged functions ==== *)

let getClassName (tag : string) (def : function_def) =
  let name =
    match Tags.getParameterList def.tags tag [("name", Tags.TypeString)] with
    | [name_param] ->
        Tags.getStringValueOr ~default:(originalBaseName def) name_param
    | _ ->
        originalBaseName def
  in
  if isValidCIdentifier name then name
  else Util.Error.raiseError ("The Pure Data object name '" ^ name ^ "' is not a valid C identifier") def.loc

let checkSingleTag (def : function_def) =
  let present = CCList.filter (fun tag -> Tags.has def.tags tag) ["pdtilde"; "pd"; "pdmessage"] in
  match present with
  | [] | [_] ->
      ()
  | _ ->
      Util.Error.raiseError
        ( "The function '" ^ originalBaseName def ^ "' has more than one Pure Data tag: "
        ^ String.concat ", " (CCList.map (fun t -> "@[" ^ t ^ "]") present) )
        def.loc

let checkArgType ~(strings_allowed : bool) (context : string) (loc : Util.Loc.t) (p : param) =
  if isNumericType p.t || (strings_allowed && isStringType p.t) then ()
  else
    let expected = if strings_allowed then "a numeric or string type" else "a numeric type" in
    Util.Error.raiseError
      (context ^ ": the parameter '" ^ p.name ^ "' has type '" ^ typeName p.t ^ "' but " ^ expected ^ " is required")
      loc

let checkOutputType ~(strings_allowed : bool) (context : string) (loc : Util.Loc.t) (t : type_) =
  if isNumericType t || (strings_allowed && isStringType t) then ()
  else
    let expected = if strings_allowed then "numeric or string outputs" else "numeric outputs" in
    Util.Error.raiseError
      (context ^ ": has an output of type '" ^ typeName t ^ "' but only " ^ expected ^ " are supported")
      loc

let makeObject (kind : obj_kind) (tag : string) (def : function_def) : obj =
  let class_name = getClassName tag def in
  let inputs = nonCtxArgs def in
  let outputs = outputTypes def in
  let context = "The Pure Data object '" ^ class_name ^ "'" in
  let () =
    match kind with
    | Tilde ->
        if inputs = [] && outputs = [] then
          Util.Error.raiseError (context ^ ": a signal object requires at least one input or output") def.loc ;
        CCList.iter (checkArgType ~strings_allowed:false context def.loc) inputs ;
        CCList.iter (checkOutputType ~strings_allowed:false context def.loc) outputs
    | Control ->
        CCList.iter (checkArgType ~strings_allowed:true context def.loc) inputs ;
        CCList.iter (checkOutputType ~strings_allowed:true context def.loc) outputs
  in
  let setup_name = match kind with Tilde -> class_name ^ "_tilde_setup" | Control -> class_name ^ "_setup" in
  {kind; fname= def.name; class_name; setup_name; ctx= contextPath def; inputs; outputs; messages= []; loc= def.loc}

let makeMessage (def : function_def) : string * message =
  let selector = originalBaseName def in
  let context = "The Pure Data message '" ^ selector ^ "'" in
  let () =
    match def.t with
    | _, {t= TVoid None; _} ->
        ()
    | _ ->
        Util.Error.raiseError (context ^ ": message functions must not return a value") def.loc
  in
  let args = nonCtxArgs def in
  let () = CCList.iter (checkArgType ~strings_allowed:true context def.loc) args in
  match contextPath def with
  | Some ctx ->
      (ctx, {m_fname= def.name; m_selector= selector; m_args= args; m_loc= def.loc})
  | None ->
      Util.Error.raiseError
        ( context
        ^ ": @[pdmessage] requires sharing state with a @[pdtilde] or @[pd] function. Declare the message with 'and' \
           next to a function that has 'mem' state." )
        def.loc

let attachMessage (objects : obj list) (ctx : string) (msg : message) : obj list =
  let attached = ref false in
  let objects =
    CCList.map
      (fun (o : obj) ->
        if o.ctx = Some ctx && not !attached then (
          attached := true ;
          {o with messages= o.messages @ [msg]} )
        else o )
      objects
  in
  if !attached then objects
  else
    Util.Error.raiseError
      ( "The Pure Data message '" ^ msg.m_selector
      ^ "' does not belong to any @[pdtilde] or @[pd] function. Declare it with 'and' in the same function group." )
      msg.m_loc

let checkSelectors (o : obj) =
  let reserved = reservedSelectors o.kind in
  let rec loop seen (messages : message list) =
    match messages with
    | [] ->
        ()
    | m :: rest ->
        if CCList.mem m.m_selector reserved then
          Util.Error.raiseError
            ("The Pure Data message '" ^ m.m_selector ^ "' uses a selector reserved by the object")
            m.m_loc ;
        if CCList.mem m.m_selector seen then
          Util.Error.raiseError ("The Pure Data message '" ^ m.m_selector ^ "' is declared more than once") m.m_loc ;
        loop (m.m_selector :: seen) rest
  in
  loop [] o.messages

let checkUniqueNames (lib_name : string) (objects : obj list) =
  let rec loop seen_classes seen_setups (objects : obj list) =
    match objects with
    | [] ->
        ()
    | o :: rest ->
        let visible = match o.kind with Tilde -> o.class_name ^ "~" | Control -> o.class_name in
        if CCList.mem visible seen_classes then
          Util.Error.raiseError ("The Pure Data object '" ^ visible ^ "' is defined more than once") o.loc ;
        if CCList.mem o.setup_name seen_setups || o.setup_name = lib_name ^ "_setup" then
          Util.Error.raiseError
            ( "The Pure Data object '" ^ visible ^ "' produces the setup symbol '" ^ o.setup_name
            ^ "' which collides with another object or with the library" )
            o.loc ;
        loop (visible :: seen_classes) (o.setup_name :: seen_setups) rest
  in
  loop [] [] objects

let collectObjects (lib_name : string) (stmts : top_stmt list) : obj list =
  let defs =
    CCList.filter_map (fun (s : top_stmt) -> match s.top with TopFunction (def, _) -> Some def | _ -> None) stmts
  in
  let () = CCList.iter checkSingleTag defs in
  let objects =
    CCList.filter_map
      (fun def ->
        if Tags.has def.tags "pdtilde" then Some (makeObject Tilde "pdtilde" def)
        else if Tags.has def.tags "pd" then Some (makeObject Control "pd" def)
        else None )
      defs
  in
  let objects =
    CCList.fold_left
      (fun objects def ->
        if Tags.has def.tags "pdmessage" then
          let ctx, msg = makeMessage def in
          attachMessage objects ctx msg
        else objects )
      objects defs
  in
  let () = CCList.iter checkSelectors objects in
  let () = checkUniqueNames lib_name objects in
  objects

(* ==== Shared pieces of the generated code ==== *)

(* The struct type used by the object: the context of the function when it has one.
   Pure Data hands the object memory as raw bytes, so the context is constructed in
   place (its members can be C++ types like std::string) and destroyed on delete. *)
let dataType (o : obj) =
  match o.ctx with
  | Some ctx ->
      (Pla.string ctx, {%pla|new (&x->data) <#ctx#s>();
   <#ctx#s>_init(x->data);|}, {%pla|x->data.~<#ctx#s>();|})
  | None ->
      (Pla.string "float", Pla.unit, Pla.unit)

let structName (o : obj) =
  match o.kind with Tilde -> "t_" ^ o.fname ^ "_tilde" | Control -> "t_" ^ o.fname ^ "_normal"

(* One validated local per message argument, then the call. Every atom is checked before
   the function runs: a failure reports the error to Pure Data and skips the call. *)
let messageHandler (o : obj) (msg : message) : Pla.t =
  let fname = msg.m_fname in
  let selector = msg.m_selector in
  let struct_name = structName o in
  let n_args = CCList.length msg.m_args in
  let read_args =
    CCList.mapi
      (fun i (p : param) ->
        if isStringType p.t then
          {%pla|t_symbol *a<#i#i>; if (!vult_get_symbol(x, "<#selector#s>", <#i#i>, argv, &a<#i#i>)) return;|}
        else {%pla|t_float a<#i#i>; if (!vult_get_float(x, "<#selector#s>", <#i#i>, argv, &a<#i#i>)) return;|} )
      msg.m_args
    |> Pla.join_sep Pla.newline
  in
  let call_args =
    CCList.mapi
      (fun i (p : param) ->
        if isStringType p.t then {%pla|std::string(a<#i#i>->s_name)|} else castInput p.t {%pla|a<#i#i>|} )
      msg.m_args
  in
  let call_args = Pla.join_sep Pla.commaspace (Pla.string "x->data" :: call_args) in
  {%pla|
void <#fname#s>_msg(<#struct_name#s> *x, t_symbol *s, int argc, t_atom *argv)
{
   (void)s;
   if (!vult_check_argc(x, "<#selector#s>", <#n_args#i>, argc)) return;
<#read_args#+>
   <#fname#s>(<#call_args#>);
}
|}

let messageHandlers (o : obj) : Pla.t = Pla.map_join (messageHandler o) o.messages

let messageRegistrations (o : obj) (class_var : string) : Pla.t =
  o.messages
  |> CCList.map (fun (m : message) ->
      let fname = m.m_fname in
      let selector = m.m_selector in
      {%pla|class_addmethod(<#class_var#s>, (t_method)<#fname#s>_msg, gensym("<#selector#s>"), A_GIMME, A_NULL);|} )
  |> Pla.join_sep Pla.newline |> Pla.indent

(* ==== Signal objects ==== *)

(* Extra signal inlets: the first input arrives through the main signal inlet. *)
let addInlets (inputs : param list) =
  match inputs with
  | [] | [_] ->
      Pla.unit
  | _ :: t ->
      CCList.map (fun _ -> Pla.string "inlet_new(&x->x_obj, &x->x_obj.ob_pd, &s_signal, &s_signal);") t
      |> Pla.join_sep Pla.newline |> Pla.indent

let addOutlets (o : obj) =
  o.outputs
  |> CCList.map (fun _ -> Pla.string "outlet_new(&x->x_obj, &s_signal);")
  |> Pla.join_sep Pla.newline |> Pla.indent

let tildeNewFunction (o : obj) : int * Pla.t =
  let dsp_nargs = CCList.length o.inputs + CCList.length o.outputs in
  let vec_decl =
    CCList.init dsp_nargs (fun i -> {%pla|sp[<#i#i>]->s_vec|}) |> Pla.join_sep_all {%pla|,<#>|} |> Pla.indent
  in
  (dsp_nargs + 2, vec_decl)

let inputName (i, acc) (p : param) = (i + 1, castInput p.t {%pla|*(in_<#i#i>++)|} :: acc)

let tildePerformFunctionCall (o : obj) =
  let fname = o.fname in
  (* generates the arguments for the process call *)
  let args = CCList.fold_left inputName (0, []) o.inputs |> snd |> CCList.rev in
  let args = Pla.join_sep Pla.commaspace (if o.ctx <> None then Pla.string "x->data" :: args else args) in
  (* declares the return variable and copies the values to the output buffers *)
  let ret, copy =
    match o.outputs with
    | [] ->
        (Pla.unit, Pla.unit)
    | [t] ->
        let current_typ = typeString t in
        let decl = {%pla|<#current_typ#s> ret = |} in
        let value = castOutput t (Pla.string "ret") in
        let copy = {%pla|*(out_0++) = <#value#>;|} in
        (decl, copy)
    | outputs ->
        let copy =
          CCList.mapi
            (fun i t ->
              let value = castOutput t {%pla|x->data.<#fname#s>_ret_<#i#i>|} in
              {%pla|*(out_<#i#i>++) = <#value#>;|} )
            outputs
          |> Pla.join_sep_all Pla.newline
        in
        (Pla.unit, copy)
  in
  {%pla|<#ret#> <#fname#s>(<#args#>);<#><#copy#>|}

(* Buffer access of the _tilde_perform function *)
let tildePerformFunctionVector (o : obj) : int * Pla.t =
  let decl_templ io index count = {%pla|t_sample *<#io#s>_<#index#i> = (t_sample *)(w[<#count#i>]);|} in
  (* First the inputs. We start with count=2 for accessing the vector 'w' *)
  let decl1, count, _ =
    CCList.fold_left
      (fun (s, count, index) _ ->
        let t = decl_templ "in" index count in
        (t :: s, count + 1, index + 1) )
      ([], 2, 0) o.inputs
  in
  (* now for the outputs, we continue counting with the last value of count *)
  let decl2, count, _ =
    CCList.fold_left
      (fun (s, count, index) _ ->
        let t = decl_templ "out" index count in
        (t :: s, count + 1, index + 1) )
      (decl1, count, 0) o.outputs
  in
  (* the number of samples is in the next index *)
  let n = {%pla|<#>int n = (int)(w[<#count#i>]);|} in
  let decl = CCList.rev (n :: decl2) |> Pla.join_sep Pla.newline |> Pla.indent in
  (* we return the number of buffers used *)
  (count + 1, decl)

let tildeObject (o : obj) : Pla.t =
  let fname = o.fname in
  let class_name = o.class_name in
  let setup_name = o.setup_name in
  let struct_name = structName o in
  let inlets = addInlets o.inputs in
  let outlets = addOutlets o in
  let dsp_nargs, vec_decl = tildeNewFunction o in
  let last_w, io_decl = tildePerformFunctionVector o in
  let process_call = tildePerformFunctionCall o in
  let main_type, init_call, destroy_call = dataType o in
  let handlers = messageHandlers o in
  let registrations = messageRegistrations o (fname ^ "_tilde_class") in
  (* The main signal inlet only exists when the object has signal inputs: a pure generator
     declares no signal inlet, and sp[] then starts directly at the outlets. *)
  let main_signal_in =
    if o.inputs = [] then Pla.unit else {%pla|CLASS_MAINSIGNALIN(<#fname#s>_tilde_class, <#struct_name#s>, dummy);|}
  in
  {%pla|
extern "C" {

static t_class *<#fname#s>_tilde_class;

typedef struct _<#fname#s>_tilde {
   t_object  x_obj;
   float dummy;
   <#main_type#> data;
} <#struct_name#s>;
<#handlers#>
t_int *<#fname#s>_tilde_perform(t_int *w)
{
   <#struct_name#s> *x = (<#struct_name#s> *)(w[1]);
<#io_decl#>

   while (n--) {<#process_call#+>
   }

   return (w+<#last_w#i>);
}

void <#fname#s>_tilde_dsp(<#struct_name#s> *x, t_signal **sp)
{
   dsp_add(<#fname#s>_tilde_perform, <#dsp_nargs#i>,
   x,<#vec_decl#>
   sp[0]->s_n);
}

void *<#fname#s>_tilde_new()
{
   <#struct_name#s> *x = (<#struct_name#s> *)pd_new(<#fname#s>_tilde_class);

   <#init_call#>
<#inlets#>
<#outlets#>

   return (void *)x;
}

void <#fname#s>_tilde_delete(<#struct_name#s> *x){
   (void)x;
   <#destroy_call#>
}

EXPORT void <#setup_name#s>(void) {
   <#fname#s>_tilde_class = class_new(gensym("<#class_name#s>~"),
      (t_newmethod)<#fname#s>_tilde_new, // constructor function
      (t_method)<#fname#s>_tilde_delete, // destructor function
      sizeof(<#struct_name#s>), // size of the object
      CLASS_DEFAULT, // type of object
      A_NULL); // arguments passed

   class_addmethod(<#fname#s>_tilde_class, (t_method)<#fname#s>_tilde_dsp, gensym("dsp"), A_NULL);
   <#main_signal_in#>
<#registrations#>
}

} // extern "C"
|}

(* ==== Control objects ==== *)

let controlArgField (p : param) : Pla.t =
  let name = p.name in
  if isStringType p.t then {%pla|t_symbol *arg_<#name#s>;|} else {%pla|t_float arg_<#name#s>;|}

let controlArgInit (p : param) : Pla.t =
  let name = p.name in
  if isStringType p.t then {%pla|x->arg_<#name#s> = gensym("");|} else {%pla|x->arg_<#name#s> = 0;|}

(* Passive inlets for the arguments after the first one: they only store the value. *)
let controlInlets (inputs : param list) =
  match inputs with
  | [] ->
      Pla.unit
  | _ :: t ->
      CCList.map
        (fun (p : param) ->
          let name = p.name in
          if isStringType p.t then {%pla|symbolinlet_new(&x->x_obj, &x->arg_<#name#s>);|}
          else {%pla|floatinlet_new(&x->x_obj, &x->arg_<#name#s>);|} )
        t
      |> Pla.join_sep Pla.newline |> Pla.indent

let controlOutlets (o : obj) =
  o.outputs
  |> CCList.mapi (fun i t ->
      let sel = if isStringType t then "s_symbol" else "s_float" in
      {%pla|x->out_<#i#i> = outlet_new(&x->x_obj, &<#sel#s>);|} )
  |> Pla.join_sep Pla.newline |> Pla.indent

let controlOutletVars (o : obj) =
  o.outputs |> CCList.mapi (fun i _ -> {%pla|t_outlet *out_<#i#i>;|}) |> Pla.join_sep Pla.newline |> Pla.indent

(* Sends one output value: symbols are interned by Pure Data and never released, which is
   fine for parameter-like values but grows with the number of distinct strings. *)
let controlSendOutput (o : obj) (i : int) (t : type_) : Pla.t =
  let value =
    match o.outputs with
    | [_] ->
        Pla.string "ret"
    | _ ->
        let fname = o.fname in
        {%pla|x->data.<#fname#s>_ret_<#i#i>|}
  in
  if isStringType t then {%pla|outlet_symbol(x->out_<#i#i>, gensym(<#value#>.c_str()));|}
  else
    let value = castOutput t value in
    {%pla|outlet_float(x->out_<#i#i>, <#value#>);|}

let controlCall (o : obj) : Pla.t =
  let fname = o.fname in
  let struct_name = structName o in
  let args =
    CCList.map
      (fun (p : param) ->
        let name = p.name in
        if isStringType p.t then {%pla|std::string(x->arg_<#name#s>->s_name)|}
        else castInput p.t {%pla|x->arg_<#name#s>|} )
      o.inputs
  in
  let args = Pla.join_sep Pla.commaspace (if o.ctx <> None then Pla.string "x->data" :: args else args) in
  let ret =
    match o.outputs with
    | [t] when not (isStringType t) ->
        let current_typ = typeString t in
        {%pla|<#current_typ#s> ret = |}
    | [_] ->
        Pla.string "std::string ret = "
    | _ ->
        Pla.unit
  in
  (* Pure Data convention: outputs are sent right to left *)
  let sends =
    CCList.mapi (fun i t -> (i, t)) o.outputs
    |> CCList.rev
    |> CCList.map (fun (i, t) -> controlSendOutput o i t)
    |> Pla.join_sep Pla.newline |> Pla.indent
  in
  {%pla|
static void <#fname#s>_call(<#struct_name#s> *x)
{
   <#ret#><#fname#s>(<#args#>);
<#sends#>
}
|}

(* The leftmost inlet triggers the object. A float or symbol stores the value of the first
   argument and calls the function; a bang re-triggers with the stored values; a list sets
   the leading arguments (the rest keep their stored value) and triggers. *)
let controlTriggers (o : obj) : Pla.t =
  let fname = o.fname in
  let struct_name = structName o in
  let bang = {%pla|
void <#fname#s>_bang(<#struct_name#s> *x)
{
   <#fname#s>_call(x);
}
|} in
  let first =
    match o.inputs with
    | [] ->
        Pla.unit
    | p :: _ when isStringType p.t ->
        let name = p.name in
        {%pla|
void <#fname#s>_symbol(<#struct_name#s> *x, t_symbol *s)
{
   x->arg_<#name#s> = s;
   <#fname#s>_call(x);
}
|}
    | p :: _ ->
        let name = p.name in
        {%pla|
void <#fname#s>_float(<#struct_name#s> *x, t_float f)
{
   x->arg_<#name#s> = f;
   <#fname#s>_call(x);
}
|}
  in
  let list_handler =
    match o.inputs with
    | [] ->
        Pla.unit
    | inputs ->
        let n = CCList.length inputs in
        let class_name = o.class_name in
        let stores =
          CCList.mapi
            (fun i (p : param) ->
              let name = p.name in
              if isStringType p.t then
                {%pla|if (argc > <#i#i>) { t_symbol *v; if (!vult_get_symbol(x, "<#class_name#s>", <#i#i>, argv, &v)) return; x->arg_<#name#s> = v; }|}
              else
                {%pla|if (argc > <#i#i>) { t_float v; if (!vult_get_float(x, "<#class_name#s>", <#i#i>, argv, &v)) return; x->arg_<#name#s> = v; }|} )
            inputs
          |> Pla.join_sep Pla.newline |> Pla.indent
        in
        {%pla|
void <#fname#s>_list(<#struct_name#s> *x, t_symbol *s, int argc, t_atom *argv)
{
   (void)s;
   if (argc > <#n#i>) {
      pd_error(x, "<#class_name#s>: too many arguments (%d given, %d expected)", argc, <#n#i>);
      return;
   }
<#stores#>
   <#fname#s>_call(x);
}
|}
  in
  Pla.join [bang; first; list_handler]

let controlRegistrations (o : obj) : Pla.t =
  let fname = o.fname in
  let class_var = fname ^ "_normal_class" in
  let bang = {%pla|class_addbang(<#class_var#s>, (t_method)<#fname#s>_bang);|} in
  let first =
    match o.inputs with
    | [] ->
        Pla.unit
    | p :: _ when isStringType p.t ->
        {%pla|<#>class_addsymbol(<#class_var#s>, (t_method)<#fname#s>_symbol);|}
    | _ ->
        {%pla|<#>class_addfloat(<#class_var#s>, (t_method)<#fname#s>_float);|}
  in
  let list_reg =
    if o.inputs = [] then Pla.unit else {%pla|<#>class_addlist(<#class_var#s>, (t_method)<#fname#s>_list);|}
  in
  Pla.join [bang; first; list_reg] |> Pla.indent

let controlObject (o : obj) : Pla.t =
  let fname = o.fname in
  let class_name = o.class_name in
  let setup_name = o.setup_name in
  let struct_name = structName o in
  let arg_fields = CCList.map controlArgField o.inputs |> Pla.join_sep Pla.newline |> Pla.indent in
  let arg_inits = CCList.map controlArgInit o.inputs |> Pla.join_sep Pla.newline |> Pla.indent in
  let outlet_vars = controlOutletVars o in
  let inlets = controlInlets o.inputs in
  let outlets = controlOutlets o in
  let main_type, init_call, destroy_call = dataType o in
  let call = controlCall o in
  let triggers = controlTriggers o in
  let handlers = messageHandlers o in
  let trigger_registrations = controlRegistrations o in
  let message_registrations = messageRegistrations o (fname ^ "_normal_class") in
  {%pla|
extern "C" {

static t_class *<#fname#s>_normal_class;

typedef struct _<#fname#s>_normal {
   t_object  x_obj;
<#arg_fields#>
<#outlet_vars#>
   <#main_type#> data;
} <#struct_name#s>;
<#call#><#triggers#><#handlers#>
void *<#fname#s>_normal_new()
{
   <#struct_name#s> *x = (<#struct_name#s> *)pd_new(<#fname#s>_normal_class);
   <#init_call#>
<#arg_inits#>
<#inlets#>
<#outlets#>

   return (void *)x;
}

void <#fname#s>_normal_delete(<#struct_name#s> *x){
   (void)x;
   <#destroy_call#>
}

EXPORT void <#setup_name#s>(void) {
   <#fname#s>_normal_class = class_new(gensym("<#class_name#s>"),
      (t_newmethod)<#fname#s>_normal_new, // constructor function
      (t_method)<#fname#s>_normal_delete, // destructor function
      sizeof(<#struct_name#s>), // size of the object
      CLASS_DEFAULT, // type of object
      A_NULL); // arguments passed

<#trigger_registrations#>
<#message_registrations#>
}

} // extern "C"
|}

let objectCode (o : obj) : Pla.t = match o.kind with Tilde -> tildeObject o | Control -> controlObject o

(* ==== Library level code ==== *)

(* The samplerate functions are declared by vultin.hpp with C++ linkage, so they are
   defined outside the extern "C" blocks. The atom helpers validate every incoming value
   before the object function is called. *)
let implPrelude =
  {%pla|
float float_samplerate() { return sys_getsr(); }
fix16_t fix_samplerate() { return float_to_fix(sys_getsr()); }

static bool vult_check_argc(void *x, const char *msg, int expected, int argc) {
   if (argc != expected) {
      pd_error(x, "%s: wrong number of arguments (%d given, %d expected)", msg, argc, expected);
      return false;
   }
   return true;
}

static bool vult_get_float(void *x, const char *msg, int i, t_atom *argv, t_float *value) {
   if (argv[i].a_type != A_FLOAT) {
      pd_error(x, "%s: argument %d must be a number", msg, i + 1);
      return false;
   }
   *value = argv[i].a_w.w_float;
   return true;
}

static bool vult_get_symbol(void *x, const char *msg, int i, t_atom *argv, t_symbol **value) {
   if (argv[i].a_type != A_SYMBOL) {
      pd_error(x, "%s: argument %d must be a symbol", msg, i + 1);
      return false;
   }
   *value = argv[i].a_w.w_symbol;
   return true;
}
|}

let libImpl lib_name (objects : obj list) =
  let calls =
    Pla.map_sep Pla.newline
      (fun (o : obj) ->
        let setup_name = o.setup_name in
        {%pla|<#setup_name#s>();|} )
      objects
  in
  {%pla|void <#lib_name#s>_setup() {
<#calls#+>
}|}

let libHeader lib_name (objects : obj list) : Pla.t =
  let setups =
    Pla.map_sep_all Pla.newline
      (fun (o : obj) ->
        let setup_name = o.setup_name in
        {%pla|EXPORT void <#setup_name#s>(void);|} )
      objects
  in
  {%pla|
 #include <stdint.h>
 #include <math.h>
 #include <new>
 #include <m_pd.h>

 #if defined(_MSC_VER)
     //  Microsoft VC++
     #define EXPORT __declspec(dllexport)
 #else
     //  GCC
     #define EXPORT __attribute__((visibility("default")))
 #endif

 extern "C" {
 EXPORT void <#lib_name#s>_setup(void);
 <#setups#>}

 |}

let generate prefix (stmts : top_stmt list) =
  let objects = collectObjects prefix stmts in
  let impl = Pla.join [implPrelude; Pla.map_join objectCode objects] in
  let lib = libImpl prefix objects in
  let header = libHeader prefix objects in
  ((impl, lib), (header, Pla.unit))

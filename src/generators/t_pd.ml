type function_info =
  { name : string
  ; class_name : string
  ; has_ctx : bool
  ; inputs : Code.param list
  ; outputs : Code.type_ list
  ; is_dsp : bool
  }

let getFunctionInfo (f : Code.function_def) =
  let outputs =
    match f.t with
    | _, Tuple elems -> elems
    | _, Void (Some elems) -> elems
    | _, Void None -> []
    | _, t -> [ t ]
  in
  let has_ctx, inputs =
    match f.args with
    | ("_ctx", Struct _) :: inputs -> true, inputs
    | inputs -> false, inputs
  in
  let class_name, is_dsp =
    let open Pparser.Ptags in
    match getParameterList f.tags "pd" [ "name", TypeString; "dsp", TypeBool ] with
    | [ name_param; dsp_param ] -> getStringValueOr ~default:f.name name_param, getBoolValueOr ~default:true dsp_param
    | _ -> f.name, true
  in
  if inputs <> [] || outputs <> [] then Some { name = f.name; class_name; has_ctx; inputs; outputs; is_dsp } else None
;;

let typeString (t : Code.type_) =
  match t with
  | Real -> "float"
  | Int -> "int"
  | Bool -> "bool"
  | _ -> failwith "Pd.typeString: not a numeric type"
;;

let addInlets (inputs : Code.param list) =
  match inputs with
  | [] | [ _ ] -> Pla.unit
  | _ :: t ->
    List.map (fun _ -> Pla.string "inlet_new(&x->x_obj, &x->x_obj.ob_pd, &s_signal, &s_signal);") t
    |> Pla.join_sep Pla.newline
    |> Pla.indent
;;

let addNormalInlets (inputs : Code.param list) =
  match inputs with
  | [] -> Pla.unit
  | _ :: t ->
    List.map (fun (n, _) -> {%pla|floatinlet_new(&x->x_obj, &x-><#n#s>);|}) t |> Pla.join_sep Pla.newline |> Pla.indent
;;

let addInletsVars (inputs : Code.param list) =
  match inputs with
  | [] -> Pla.unit
  | _ -> List.map (fun (n, _) -> {%pla|float <#n#s>;|}) inputs |> Pla.join_sep Pla.newline |> Pla.indent
;;

let addOutletsVars (outputs : Code.type_ list) =
  match outputs with
  | [] -> Pla.unit
  | _ -> List.mapi (fun i _ -> {%pla|t_outlet *out_<#i#i>;|}) outputs |> Pla.join_sep Pla.newline |> Pla.indent
;;

(** Add the outlets *)
let addOutlets (f : function_info) =
  f.outputs
  |> List.map (fun _ -> Pla.string "outlet_new(&x->x_obj, &s_signal);")
  |> Pla.join_sep Pla.newline
  |> Pla.indent
;;

let addNormalOutlets (f : function_info) =
  f.outputs
  |> List.mapi (fun i _ -> {%pla|x->out_<#i#i> = outlet_new(&x->x_obj, &s_float);|})
  |> Pla.join_sep Pla.newline
  |> Pla.indent
;;

let tildeNewFunction (f : function_info) : int * Pla.t =
  let dsp_nargs = List.length f.inputs + List.length f.outputs in
  let vec_decl =
    CCList.init dsp_nargs (fun i -> [%pla {|sp[<#i#i>]->s_vec|}]) |> Pla.join_sep_all [%pla {|,<#>|}] |> Pla.indent
  in
  dsp_nargs + 2, vec_decl
;;

let castInput (typ : Code.type_) (value : Pla.t) : Pla.t = Common.cast ~from:Code.Real ~to_:typ value
let castOutput (typ : Code.type_) (value : Pla.t) : Pla.t = Common.cast ~from:typ ~to_:Code.Real value
let inputName (i, acc) (_, t) = i + 1, castInput t [%pla {|*(in_<#i#i>++)|}] :: acc

let tildePerformFunctionCall (f : function_info) =
  let fname = f.name in
  (* generates the aguments for the process call *)
  let args = List.fold_left inputName (0, []) f.inputs |> snd |> List.rev in
  let args = Pla.join_sep Pla.commaspace (if f.has_ctx then Pla.string "x->data" :: args else args) in
  (* declares the return variable and copies the values to the output buffers *)
  let ret, copy =
    match f.outputs with
    | [] -> Pla.unit, Pla.unit
    | [ o ] ->
      let current_typ = typeString o in
      let decl = [%pla {|<#current_typ#s> ret = |}] in
      let value = castOutput o (Pla.string "ret") in
      let copy = [%pla {|*(out_0++) = <#value#>;|}] in
      decl, copy
    | o ->
      let copy =
        List.mapi
          (fun i o ->
            let value = castOutput o [%pla {|x->data.<#fname#s>_ret_<#i#i>|}] in
            [%pla {|*(out_<#i#i>++) = <#value#>;|}])
          o
        |> Pla.join_sep_all Pla.newline
      in
      Pla.unit, copy
  in
  [%pla {|<#ret#> <#fname#s>(<#args#>);<#><#copy#>|}]
;;

let normalInputName (i, acc) (s, t) = i + 1, castInput t [%pla {|x-><#s#s>|}] :: acc

let normalPerformFunctionCall (f : function_info) =
  let fname = f.name in
  (* generates the aguments for the process call *)
  let args = List.fold_left normalInputName (0, []) f.inputs |> snd |> List.rev in
  let args = if args = [] then [] else Pla.string "in1" :: List.tl args in
  let args = Pla.join_sep Pla.commaspace (if f.has_ctx then Pla.string "x->data" :: args else args) in
  (* declares the return variable and copies the values to the output buffers *)
  let ret, copy =
    match f.outputs with
    | [] -> Pla.unit, Pla.unit
    | [ o ] ->
      let current_typ = typeString o in
      let decl = [%pla {|<#current_typ#s> ret = |}] in
      let value = castOutput o (Pla.string "ret") in
      let copy = [%pla {|   outlet_float(x->out_0, <#value#>);|}] in
      decl, copy
    | o ->
      let copy =
        List.mapi
          (fun i o ->
            let value = castOutput o [%pla {|x->data.<#fname#s>_ret_<#i#i>|}] in
            [%pla {|   outlet_float(x->out_<#i#i>, <#value#>);|}])
          o
        |> Pla.join_sep_all Pla.newline
      in
      Pla.unit, copy
  in
  [%pla {|<#ret#> <#fname#s>(<#args#>);<#><#copy#>|}]
;;

(** Generates the buffer access of _tilde_perform function *)
let tildePerformFunctionVector (f : function_info) : int * Pla.t =
  (* we use this template to acces the buffers of inputs and outputs *)
  let decl_templ io index count = [%pla {|t_sample *<#io#s>_<#index#i> = (t_sample *)(w[<#count#i>]);|}] in
  (* First the inputs. We start with count=2 for accessing the vector 'w' *)
  let decl1, count, _ =
    List.fold_left
      (fun (s, count, index) _ ->
        let t = decl_templ "in" index count in
        t :: s, count + 1, index + 1)
      ([], 2, 0)
      f.inputs
  in
  (* now for the outputs, we continue counting with the last value of count *)
  let decl2, count, _ =
    List.fold_left
      (fun (s, count, index) _ ->
        let t = decl_templ "out" index count in
        t :: s, count + 1, index + 1)
      (decl1, count, 0)
      f.outputs
  in
  (* the number of samples is in the next index *)
  let n = [%pla {|<#>int n = (int)(w[<#count#i>]);|}] in
  (* appends all the declarations *)
  let decl = List.rev (n :: decl2) |> Pla.join_sep Pla.newline |> Pla.indent in
  (* we return the number of buffers used *)
  count + 1, decl
;;

let getInitDefaultCalls (f : function_info) =
  if f.has_ctx
  then (
    let fname = f.name in
    [%pla {|<#fname#s>_type|}], [%pla {|<#fname#s>_type_init(x->data);|}])
  else Pla.string "float", Pla.unit
;;

let tilde_func_imp (f : function_info) : Pla.t =
  let fname = f.name in
  let class_name = f.class_name in
  (* Generate extra inlets *)
  let inlets = addInlets f.inputs in
  (* Generates the outlets*)
  let outlets = addOutlets f in
  let dsp_nargs, vec_decl = tildeNewFunction f in
  let last_w, io_decl = tildePerformFunctionVector f in
  let process_call = tildePerformFunctionCall f in
  let main_type, init_call = getInitDefaultCalls f in
  [%pla
    {|
extern "C" {

static t_class *<#fname#s>_tilde_class;

typedef struct _<#fname#s>_tilde {
   t_object  x_obj;
   float dummy;
   <#main_type#> data;
} t_<#fname#s>_tilde;

t_int *<#fname#s>_tilde_perform(t_int *w)
{
   t_<#fname#s>_tilde *x = (t_<#fname#s>_tilde *)(w[1]);
<#io_decl#>

   while (n--) {<#process_call#+>
   }

   return (w+<#last_w#i>);
}

void <#fname#s>_tilde_dsp(t_<#fname#s>_tilde *x, t_signal **sp)
{
   dsp_add(<#fname#s>_tilde_perform, <#dsp_nargs#i>,
   x,<#vec_decl#>
   sp[0]->s_n);
}

void *<#fname#s>_tilde_new()
{
   t_<#fname#s>_tilde *x = (t_<#fname#s>_tilde *)pd_new(<#fname#s>_tilde_class);

   <#init_call#>
<#inlets#>
<#outlets#>

   return (void *)x;
}

void <#fname#s>_tilde_delete(t_<#fname#s>_tilde *x){

}

void <#fname#s>_tilde_setup(void) {
   <#fname#s>_tilde_class = class_new(gensym("<#class_name#s>~"),
      (t_newmethod)<#fname#s>_tilde_new, // constructor function
      (t_method)<#fname#s>_tilde_delete, // destructor function
      sizeof(t_<#fname#s>_tilde), // size of the object
      CLASS_DEFAULT, // type of object
      A_NULL); // arguments passed

   class_addmethod(<#fname#s>_tilde_class, (t_method)<#fname#s>_tilde_dsp, gensym("dsp"), A_NULL);
   CLASS_MAINSIGNALIN(<#fname#s>_tilde_class, t_<#fname#s>_tilde, dummy);
}

} // extern "C"
|}]
;;

let normal_func_imp (f : function_info) : Pla.t =
  let fname = f.name in
  let class_name = f.class_name in
  (* Generate extra inlets *)
  let inlets = addNormalInlets f.inputs in
  let inlet_vars = addInletsVars f.inputs in
  (* Generates the outlets*)
  let outlets = addNormalOutlets f in
  let outlet_vars = addOutletsVars f.outputs in
  let process_call = normalPerformFunctionCall f in
  let main_type, init_call = getInitDefaultCalls f in
  [%pla
    {|
extern "C" {

static t_class *<#fname#s>_normal_class;

typedef struct _<#fname#s>_normal {
   t_object  x_obj;
   float dummy;
   <#inlet_vars#>
   <#outlet_vars#>
   <#main_type#> data;
} t_<#fname#s>_normal;

void <#fname#s>_normal_bang(t_<#fname#s>_normal *x, t_float in1)
{
   <#process_call#>
}

void *<#fname#s>_normal_new()
{
   t_<#fname#s>_normal *x = (t_<#fname#s>_normal *)pd_new(<#fname#s>_normal_class);
   <#init_call#>

<#inlets#>
<#outlets#>

   return (void *)x;
}

void <#fname#s>_normal_delete(t_<#fname#s>_normal *x){

}

void <#fname#s>_normal_setup(void) {
   <#fname#s>_normal_class = class_new(gensym("<#class_name#s>"),
      (t_newmethod)<#fname#s>_normal_new, // constructor function
      (t_method)<#fname#s>_normal_delete, // destructor function
      sizeof(t_<#fname#s>_normal), // size of the object
      CLASS_DEFAULT, // type of object
      A_NULL); // arguments passed

   class_addbang(<#fname#s>_normal_class, (t_method)<#fname#s>_normal_bang);
   class_addfloat(<#fname#s>_normal_class, (t_method)<#fname#s>_normal_bang);
}

} // extern "C"
|}]
;;

let func_imp (f : function_info) : Pla.t = if f.is_dsp then tilde_func_imp f else normal_func_imp f

let func_header (f : function_info) : Pla.t =
  let fname = f.name in
  [%pla {|extern "C" void <#fname#s>_tilde_setup(void);|}]
;;

let lib_impl lib_name (functions : function_info list) =
  let calls =
    Pla.map_sep
      Pla.newline
      (fun f ->
        let fname = f.name in
        if f.is_dsp then [%pla {|<#fname#s>_tilde_setup();|}] else [%pla {|<#fname#s>_normal_setup();|}])
      functions
  in
  [%pla {|void <#lib_name#s>_setup() {
<#calls#+>
}|}]
;;

let lib_header lib_name : Pla.t =
  [%pla
    {|
 #include <stdint.h>
 #include <math.h>
 #include <m_pd.h>

static inline float samplerate() { return sys_getsr(); }

 #if defined(_MSC_VER)
     //  Microsoft VC++
     #define EXPORT __declspec(dllexport)
 #else
     //  GCC
     #define EXPORT __attribute__((visibility("default")))
 #endif

 extern "C" {
 EXPORT void <#lib_name#s>_setup(void);
 }

 |}]
;;

let getStmtInfo (s : Code.top_stmt) =
  match s with
  | TopFunction (def, _) ->
    (match getFunctionInfo def with
    | Some f when def.info.is_root -> Some f
    | _ -> None)
  | _ -> None
;;

let generate prefix code_impl code_head (stmts : Code.top_stmt list) =
  let functions = List.filter_map getStmtInfo stmts in
  let impl = List.map func_imp functions in
  let headers = List.map func_header functions in
  let f_impl = Pla.join_sep_all Pla.newline impl in
  let f_header = Pla.join_sep_all Pla.newline headers in
  let header = lib_header prefix in
  let lib = lib_impl prefix functions in
  Pla.(join [ f_impl; code_impl; lib ], join [ header; f_header; code_head ])
;;

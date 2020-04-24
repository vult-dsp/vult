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

(** Template for the Teensy Audio library *)
open Config

let tables (params : params) (code : Pla.t) : Pla.t =
   let file = String.uppercase params.output in
   {pla|
/* Code automatically generated by Vult https://github.com/modlfo/vult */
#ifndef <#file#s>_TABLES_H
#define <#file#s>_TABLES_H


<#code#>

#endif // <#file#s>_TABLES_H
|pla}


(** Header function *)
let header (params : params) (code : Pla.t) : Pla.t =
   let file = String.uppercase params.output in
   let tables = params.output in
   let output = params.output in
   {pla|
/* Code automatically generated by Vult https://github.com/modlfo/vult */
#ifndef <#file#s>_H
#define <#file#s>_H

#include <stdint.h>
#include <math.h>
#include "vultin.h"
#include <m_pd.h>
#include "<#tables#s>.tables.h"

<#code#>

#if defined(_MSC_VER)
    //  Microsoft VC++
    #define EXPORT __declspec(dllexport)
#else
    //  GCC
    #define EXPORT __attribute__((visibility("default")))
#endif

extern "C" {
EXPORT void <#output#s>_tilde_setup(void);
}

#endif // <#file#s>_H
|pla}


let rec removeContext inputs =
   match inputs with
   | IContext :: t -> removeContext t
   | _ -> inputs


(** Add extra inlets if the object requires more than one *)
let rec addInlets inputs =
   match inputs with
   | IContext :: t -> addInlets t
   | []
   |[ _ ] ->
      Pla.unit
   | _ :: t ->
      List.map (fun _ -> Pla.string "inlet_new(&x->x_obj, &x->x_obj.ob_pd, &s_signal, &s_signal); ") t
      |> Pla.join_sep Pla.newline
      |> Pla.indent


(** Add the outlets *)
let addOutlets (config : config) =
   config.process_outputs
   |> List.map (fun _ -> Pla.string "outlet_new(&x->x_obj, &s_signal); ")
   |> Pla.join_sep Pla.newline
   |> Pla.indent


(** Generates contents for the _tilde_new function *)
let tildeNewFunction (config : config) : int * Pla.t =
   let dsp_nargs = List.length (removeContext config.process_inputs) + List.length config.process_outputs in
   let vec_decl =
      CCList.init dsp_nargs (fun i -> {pla|sp[<#i#i>]->s_vec|pla}) |> Pla.join_sep_all {pla|, <#>|pla} |> Pla.indent
   in
   dsp_nargs + 2, vec_decl


let castType (cast : string) (value : Pla.t) : Pla.t =
   match cast with
   | "float" -> {pla|(float) <#value#>|pla}
   | "int" -> {pla|(int) <#value#>|pla}
   | "bool" -> {pla|(bool) <#value#>|pla}
   | _ -> {pla|<#cast#s>(<#value#>)|pla}


let castInput (params : params) (typ : input) (value : Pla.t) : Pla.t =
   let current_typ = Replacements.getType params.repl (Config.inputTypeString typ) in
   let cast = Replacements.getCast params.repl "float" current_typ in
   castType cast value


let castOutput (params : params) (typ : output) (value : Pla.t) : Pla.t =
   let current_typ = Replacements.getType params.repl (Config.outputTypeString typ) in
   let cast = Replacements.getCast params.repl current_typ "float" in
   castType cast value


let inputName params (i, acc) s =
   match s with
   | IContext -> i, Pla.string "x->data" :: acc
   | _ -> i + 1, castInput params s {pla|*(in_<#i#i>++)|pla} :: acc


let tildePerformFunctionCall module_name (params : params) (config : config) =
   (* generates the aguments for the process call *)
   let args =
      List.fold_left (inputName params) (0, []) config.process_inputs |> snd |> List.rev |> Pla.join_sep Pla.comma
   in
   (* declares the return variable and copies the values to the output buffers *)
   let ret, copy =
      match config.process_outputs with
      | [] -> Pla.unit, Pla.unit
      | [ o ] ->
         let current_typ = Replacements.getType params.repl (Config.outputTypeString o) in
         let decl = {pla|<#current_typ#s> ret = |pla} in
         let value = castOutput params o (Pla.string "ret") in
         let copy = {pla|*(out_0++) = <#value#>; |pla} in
         decl, copy
      | o ->
         let copy =
            List.mapi
               (fun i o ->
                   let value = castOutput params o {pla|<#module_name#s>_process_ret_<#i#i>(x->data)|pla} in
                   {pla|*(out_<#i#i>++) = <#value#>; |pla})
               o
            |> Pla.join_sep_all Pla.newline
         in
         Pla.unit, copy
   in
   {pla|<#ret#> <#module_name#s>_process(<#args#>); <#><#copy#>|pla}


(** Generates the buffer access of _tilde_perform function *)
let tildePerformFunctionVector (config : config) : int * Pla.t =
   (* we use this template to acces the buffers of inputs and outputs *)
   let decl_templ io index count = {pla|t_sample *<#io#s>_<#index#i> = (t_sample *)(w[<#count#i>]); |pla} in
   (* First the inputs. We start with count=2 for accessing the vector 'w' *)
   let decl1, count, _ =
      List.fold_left
         (fun (s, count, index) _ ->
             let t = decl_templ "in" index count in
             t :: s, count + 1, index + 1)
         ([], 2, 0)
         (removeContext config.process_inputs)
   in
   (* now for the outputs, we continue counting with the last value of count *)
   let decl2, count, _ =
      List.fold_left
         (fun (s, count, index) _ ->
             let t = decl_templ "out" index count in
             t :: s, count + 1, index + 1)
         (decl1, count, 0)
         config.process_outputs
   in
   (* the number of samples is in the next index *)
   let n = {pla|<#>int n = (int)(w[<#count#i>]); |pla} in
   (* appends all the declarations *)
   let decl = List.rev (n :: decl2) |> Pla.join_sep Pla.newline |> Pla.indent in
   (* we return the number of buffers used *)
   count + 1, decl


let getInitDefaultCalls module_name params =
   if List.exists (fun a -> a = IContext) params.config.process_inputs then
      ( {pla|<#module_name#s>_process_type|pla}
      , {pla|<#module_name#s>_process_init(x->data); |pla}
      , {pla|<#module_name#s>_default(x->data); |pla} )
   else
      Pla.string "float", Pla.unit, Pla.unit


let functionInput i =
   match i with
   | IContext -> Pla.string "x->data"
   | IReal name
   |IInt name
   |IBool name ->
      {pla|(int)<#name#s>|pla}


let noteFunctions (params : params) =
   let output = params.output in
   let module_name = params.module_name in
   let on_args = Pla.map_sep Pla.comma functionInput params.config.noteon_inputs in
   let off_args = Pla.map_sep Pla.comma functionInput params.config.noteoff_inputs in
   ( {pla|
void <#output#s>_noteOn(t_<#output#s>_tilde *x, t_floatarg note, t_floatarg velocity, t_floatarg channel){
   if((int)velocity) <#module_name#s>_noteOn(<#on_args#>);
   else <#module_name#s>_noteOff(<#off_args#>);
}
|pla}
   , {pla|
void <#output#s>_noteOff(t_<#output#s>_tilde *x, t_floatarg note, t_floatarg channel) {
   <#module_name#s>_noteOff(<#off_args#>);
}
|pla}
   )


let controlChangeFunction (params : params) =
   let output = params.output in
   let module_name = params.module_name in
   let ctrl_args = Pla.map_sep Pla.comma functionInput params.config.controlchange_inputs in
   {pla|
void <#output#s>_controlChange(t_<#output#s>_tilde *x, t_floatarg control, t_floatarg value, t_floatarg channel) {
   <#module_name#s>_controlChange(<#ctrl_args#>);
}
|pla}


(** Implementation function *)
let implementation (params : params) (code : Pla.t) : Pla.t =
   let output = params.output in
   let module_name = params.module_name in
   (* Generate extra inlets *)
   let inlets = addInlets params.config.process_inputs in
   (* Generates the outlets*)
   let outlets = addOutlets params.config in

   let dsp_nargs, vec_decl = tildeNewFunction params.config in
   let last_w, io_decl = tildePerformFunctionVector params.config in
   let process_call = tildePerformFunctionCall module_name params params.config in
   let main_type, init_call, default_call = getInitDefaultCalls module_name params in
   let note_on, note_off = noteFunctions params in
   let ctr_change = controlChangeFunction params in
   {pla|
/* Code automatically generated by Vult https://github.com/modlfo/vult */
#include "<#output#s>.h"

<#code#>

extern "C" {

static t_class *<#output#s>_tilde_class;

typedef struct _<#output#s>_tilde {
   t_object  x_obj;
   float dummy;
   <#main_type#> data;
} t_<#output#s>_tilde;

t_int *<#output#s>_tilde_perform(t_int *w)
{
   t_<#output#s>_tilde *x = (t_<#output#s>_tilde *)(w[1]);
<#io_decl#>

   while (n--) {<#process_call#+>
   }

   return (w+<#last_w#i>);
}

void <#output#s>_tilde_dsp(t_<#output#s>_tilde *x, t_signal **sp)
{
   dsp_add(<#output#s>_tilde_perform, <#dsp_nargs#i>,
   x, <#vec_decl#>
   sp[0]->s_n);
}

void *<#output#s>_tilde_new()
{
   t_<#output#s>_tilde *x = (t_<#output#s>_tilde *)pd_new(<#output#s>_tilde_class);

   <#init_call#>
   <#default_call#>
<#inlets#>
<#outlets#>

   return (void *)x;
}

void <#output#s>_tilde_delete(t_<#output#s>_tilde *x){

}
<#note_on#>
<#note_off#>
<#ctr_change#>

void <#output#s>_tilde_setup(void) {
   <#output#s>_tilde_class = class_new(gensym("<#output#s>~"),
      (t_newmethod)<#output#s>_tilde_new, // constructor function
      (t_method)<#output#s>_tilde_delete, // destructor function
      sizeof(t_<#output#s>_tilde), // size of the object
      CLASS_DEFAULT, // type of object
      A_NULL); // arguments passed

   class_addmethod(<#output#s>_tilde_class, (t_method)<#output#s>_tilde_dsp, gensym("dsp"), A_NULL);
   CLASS_MAINSIGNALIN(<#output#s>_tilde_class, t_<#output#s>_tilde, dummy);

   class_addmethod(<#output#s>_tilde_class, (t_method)<#output#s>_noteOn,        gensym("noteOn"),        A_DEFFLOAT, A_DEFFLOAT, A_DEFFLOAT, A_NULL);
   class_addmethod(<#output#s>_tilde_class, (t_method)<#output#s>_noteOff,       gensym("noteOff"),       A_DEFFLOAT, A_DEFFLOAT, A_NULL);
   class_addmethod(<#output#s>_tilde_class, (t_method)<#output#s>_controlChange, gensym("controlChange"), A_DEFFLOAT, A_DEFFLOAT, A_DEFFLOAT, A_NULL);
}

} // extern "C"
|pla}


let get (params : params) (header_code : Pla.t) (impl_code : Pla.t) (tables_code : Pla.t) : (Pla.t * FileKind.t) list =
   [ header params header_code, FileKind.ExtOnly "h"
   ; tables params tables_code, FileKind.ExtOnly "tables.h"
   ; implementation params impl_code, FileKind.ExtOnly "cpp"
   ]

open TypesVult
open GenerateParams

class type error = object
   method msg  : string Js.prop
   method file : string Js.prop
   method line : int Js.prop
   method col  : int Js.prop
end

class type js_file_code = object
   method code : Js.js_string Js.t Js.optdef_prop
   method file : Js.js_string Js.t Js.optdef_prop
end

class type js_args = object

   method version : bool Js.optdef_prop
   method dparse : bool Js.optdef_prop
   method deps : bool Js.optdef_prop
   method ccode : bool Js.optdef_prop
   method check : bool Js.optdef_prop
   method jscode : bool Js.optdef_prop
   method luacode : bool Js.optdef_prop
   method llvm : bool Js.optdef_prop
   method eval : bool Js.optdef_prop
   method output : string Js.optdef_prop
   method real : string Js.optdef_prop
   method template : string Js.optdef_prop
   method includes : string array Js.optdef_prop
   method files : ((js_file_code Js.t) Js.js_array Js.t) Js.optdef_prop
end

let from_file_code (i:js_file_code Js.t) : input list =
   match Js.Optdef.to_option i##.file, Js.Optdef.to_option i##.code with
   | Some(file), Some(code) ->
      [Code(Js.to_string file, (Js.to_string code))]
   | Some(file),None ->
      [File((Js.to_string file))]
   | None, Some(code) ->
      [Code("live.vult", (Js.to_string code))]
   | None, None -> []

let resultObject () =
   Js.Unsafe.coerce
      (object%js
      end)

let to_file_code (file:string) (code:string) : js_file_code Js.t =
   let obj = resultObject () in
   obj##.file := Js.string file;
   obj##.code := Js.string code;
   obj

let set value fset =
   if Js.Optdef.test value then
      Js.Optdef.iter value (fun a -> fset a)

let getArguments (obj:js_args Js.t) =
   let args = { default_arguments with files = [] } in
   set (obj##.dparse)   (fun v -> args.dparse <- v);
   set (obj##.deps)     (fun v -> args.deps <- v);
   set (obj##.ccode)    (fun v -> args.ccode <- v);
   set (obj##.check)    (fun v -> args.check <- v);
   set (obj##.jscode)   (fun v -> args.jscode <- v);
   set (obj##.luacode)  (fun v -> args.luacode <- v);
   set (obj##.llvm)     (fun v -> args.llvm <- v);
   set (obj##.eval)     (fun v -> args.eval <- v);
   set (obj##.version)  (fun v -> args.show_version <- v);
   set (obj##.output)   (fun v -> args.output <- v);
   set (obj##.real)     (fun v -> args.real <- v);
   set (obj##.template) (fun v -> args.template <- v);
   set (obj##.includes) (fun v -> args.includes <- Array.to_list v);
   set (obj##.files)    (fun v -> args.files <- Array.map from_file_code (Js.to_array v) |> Array.to_list |> List.flatten);
   args

let getFile (args:arguments) (ext:filename) : string =
   match ext with
   | ExtOnly(e) -> args.output^"."^e
   | FullName(n) -> Filename.concat (Filename.dirname args.output) n

let showResult (args:arguments) (output:output) : 'a Js.t =
   match output with
   | Version v ->
      let obj = resultObject () in
      obj##.version := Js.string v;
      obj
   | Message v ->
      let obj = resultObject () in
      obj##.message := Js.string v;
      obj
   | Dependencies deps ->
      let obj = resultObject () in
      obj##.dependencies := Js.array (List.map Js.string deps |> Array.of_list);
      obj
   | ParsedCode v ->
      let obj = resultObject () in
      obj##.parsedCode := Js.string v;
      obj
   | GeneratedCode files->
      let elems =
         List.map (fun (text,file) -> to_file_code (getFile args file) (Pla.print text)) files
         |> Array.of_list |> Js.array
      in
      let obj = resultObject () in
      obj##.generatedCode := elems;
      obj
   | Interpret v ->
      let obj = resultObject () in
      obj##.interpret := Js.string v;
      obj
   | CheckOk ->
      let obj = resultObject () in
      obj##.check := Js.bool true;
      obj
   | Errors errors ->
      let makeErrorObject error : error Js.t =
         let msg, file, line, col = Error.reportErrorStringNoLoc error in
         let obj : error Js.t = Js.Unsafe.js_expr "error" in
         obj##.msg := msg;
         obj##.file := file;
         obj##.line :=  line;
         obj##.col := col;
         obj
      in
      let errors_array = List.map makeErrorObject errors |> Array.of_list in
      let obj = resultObject () in
      obj##.errors := Js.array errors_array;
      obj


let runDriver args =
   let results =
      Driver.main args
      |> List.map (showResult args)
      |> Array.of_list
   in
   Js.array results

let main (input:js_args Js.t) =
   let args = getArguments input in
   runDriver args


let version =
   let args = { default_arguments with show_version = true } in
   match Driver.main args with
   | [Version v ] -> Js.string v
   | _ -> failwith "unknown error"

let _ =
   Js.export_all
      (object%js
         method main = main
         method version = version
      end)


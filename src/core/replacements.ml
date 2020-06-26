open Prog

module C = struct
  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    match path, args, ret with
    (* builtins *)
    | "random", [], { t = TReal; _ } -> Some "float_random"
    (* cast *)
    | "int", [ { t = TReal; _ } ], _ -> Some "float_to_int"
    | _ -> None
end

let getFunToFun code =
  let open Util.Args in
  match code with
  | CCode -> C.fun_to_fun
  | _ -> fun _ _ _ -> None

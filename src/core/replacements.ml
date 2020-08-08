open Prog

module C = struct
  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    match path, args, ret with
    (* builtins *)
    | "random", [], { t = TReal; _ } -> Some "float_random"
    (* cast *)
    | "int", [ { t = TReal; _ } ], _ -> Some "float_to_int"
    | "real", [ { t = TInt; _ } ], _ -> Some "int_to_float"
    | "real", [ { t = TBool; _ } ], _ -> Some "bool_to_float"
    | _ -> None
end

let getFunToFun code =
  let open Util.Args in
  match code with
  | CCode -> C.fun_to_fun
  | _ -> fun _ _ _ -> None

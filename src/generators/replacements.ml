open Code

module C = struct
  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    match path, args, ret with
    (* builtins *)
    | "random", [], Real -> Some "float_random"
    | "clip", [ Real; _; _ ], Real -> Some "float_clip"
    | "clip", [ Int; _; _ ], Int -> Some "int_clip"
    | "clip", [ Fixed; _; _ ], Int -> Some "fix_clip"
    | "pi", [], Real -> Some "float_pi"
    | "pi", [], Fixed -> Some "fix_pi"
    | "eps", [], Real -> Some "float_eps"
    | "eps", [], Fixed -> Some "fix_eps"
    (* cast *)
    | "int", [ Real ], _ -> Some "float_to_int"
    | "real", [ Int ], _ -> Some "int_to_float"
    | "real", [ Bool ], _ -> Some "bool_to_float"
    | "real", [ Fixed ], _ -> Some "fix_to_float"
    | "fix16", [ Real ], _ -> Some "float_to_fix"
    | "fix16", [ Int ], _ -> Some "int_to_fix"
    | _ -> None


  let op_to_fun (op : operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match op, e1, e2, ret with
    | Mod, Real, Real, Real -> Some "fmodf"
    | _ -> None
end

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
    (* get *)
    | "get", [ Array (_, Real); Int ], Real -> Some "float_get"
    | "get", [ Array (_, Fixed); Int ], Real -> Some "fix_get"
    | "get", [ Array (_, Int); Int ], Int -> Some "int_get"
    (* set *)
    | "set", [ Array (_, Real); Int; Real ], Void None -> Some "float_set"
    | "set", [ Array (_, Fixed); Int; Fixed ], Void None -> Some "fix_set"
    | "set", [ Array (_, Int); Int; Int ], Void None -> Some "int_set"
    | _ -> None


  let op_to_fun (op : operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match op, e1, e2, ret with
    | Mod, Real, Real, Real -> Some "fmodf"
    | Mul, Fixed, Fixed, Fixed -> Some "fix_mul"
    | Add, Fixed, Fixed, Fixed -> Some "fix_add"
    | Sub, Fixed, Fixed, Fixed -> Some "fix_sub"
    | Div, Fixed, Fixed, Fixed -> Some "fix_div"
    | _ -> None
end

module Lua = struct
  let keywords =
    [ "and"
    ; "break"
    ; "do"
    ; "elseif"
    ; "end"
    ; "for"
    ; "function"
    ; "in"
    ; "local"
    ; "nil"
    ; "or"
    ; "repeat"
    ; "return"
    ; "until"
    ]
    |> Util.Maps.Set.of_list
end

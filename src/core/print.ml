open Typed

let path (p : path) =
  match p with
  | { id; n = None } -> Pla.string id
  | { id; n = Some m } -> {pla|<#m#s>.<#id#s>|pla}


let rec ext_type (t : ext_type) : Pla.t =
  match t.tx with
  | TELink t -> ext_type t
  | TEUnbound _ -> Pla.string "'unbound"
  | TEId p -> path p
  | TESize n -> Pla.int n
  | TEOption alt -> Pla.parenthesize @@ Pla.map_sep (Pla.string "|") ext_type alt
  | TEComposed (name, elems) ->
      let elems = Pla.map_sep Pla.commaspace ext_type elems in
      {pla|<#name#s>(<#elems#>)|pla}

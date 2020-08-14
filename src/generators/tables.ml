open Code
module Tags = Parser.Tags

let replaceFunction stmt =
  match stmt with
  | TopFunction ({ name = _; args = _; tags; _ }, _) when Tags.has tags "table" ->
      let params =
        Tags.[ "size", TypeInt; "min", TypeReal; "max", TypeReal; "order", TypeInt; "bound_check", TypeBool ]
      in
      let _msg =
        "The attribute 'table' requires specific parameters. e.g. 'table(size=128,min=0.0,max=1.0,[order=2])'"
      in
      ( match Tags.getParameterList tags "table" params with
      | _ -> failwith "" )
  | _ -> stmt


let create _vm stmts = stmts

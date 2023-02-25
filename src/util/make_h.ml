let main () =
   let input_file = Sys.argv.(1) in
   let output_file = Sys.argv.(2) in
   match FileIO.read input_file with
   | None ->
      print_endline "Cannot open input file";
      exit (-1)
   | Some text ->
      let text = String.escaped text in
      let buffer = Buffer.create (String.length text + 100) in
      let name =
         Filename.basename output_file
         |> CCString.map (fun c ->
                     match c with
                     | '.' | '/' | '\\' | ' ' -> '_'
                     | _ -> c)
      in
      Buffer.add_string buffer "static const size_t ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer "_size = ";
      Buffer.add_string buffer (string_of_int (String.length text));
      Buffer.add_string buffer ";\n";
      Buffer.add_string buffer "static const char ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer "[] = \"";
      Buffer.add_string buffer text;
      Buffer.add_string buffer "\";";
      if FileIO.write output_file (Buffer.contents buffer)
      then exit 0
      else print_endline "Failed to write the output file";
      exit (-1)
;;

main ()

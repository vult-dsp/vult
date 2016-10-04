+++
date = "2016-10-04T19:22:30+03:00"
title = "Now Vult can be installed from npm"
description = ""
tags = []
topics = []
layout = "post"
+++

In the last months I have been making many improvements to the Vult compiler. These improvements cannot be easily tried by the users due to the fact that for me creating binaries for all the platforms is very time consuming. Therefore, in order for the users to try the latests changes they need to compile Vult from the source code. Even though the process is well documented in the repository, this can be time consuming and difficult for people unfamiliar with the OCaml tools.

I have been using `js_of_ocaml` for quite some time to convert parts of the Vult compiler to JavaScript that runs in the live demo page http://modlfo.github.io/vult/demo/ however the code there does not perform any I/O to disk since it's intended to run in the web browser.

I tried to convert the whole application to JavaScript to see if it could run (as it is) with the latest improvements of `js_of_ocaml`. That way I can distribute a JavaScript version that can be run with node.js. Node.js has many advantages, for example:

- runs in most platforms
- easy to install
- it has a good package manager

Fortunately, the only thing that did not work was the file I/O (which seems reasonable). In order to make it work I made a small hack that allowed me to switch the functions that read and write files. That way I can declare bindings for the node.js functions and replace them. Here's the code:

```ocaml
(** Declares a 'buffer' object *)
class type buffer = object
   method toString : js_string t meth
end

(** Declares a 'fs' object *)
class type fs = object
   method readFileSync : js_string t -> buffer t meth
   method writeFileSync : js_string t -> js_string t -> unit t meth
end

(* var fs = require('fs') *)
let fs : fs t =
   Unsafe.fun_call (Unsafe.js_expr "require")
      [|Unsafe.inject (string "fs")|]

let read_fn (path:string) : string option =
   let buffer   = fs##readFileSync (string path) in
   let contents = to_string (buffer##toString ()) in
   Some(contents)

let write_fn (path:string) (text:string) : bool =
   let _ = fs##writeFileSync ((string path),(string text)) in
   true
;;
```

Then I replace the functions as follows:

``` ocaml
FileIO.setRead read_fn ;;
FileIO.setWrite write_fn ;;

```

With those changes it's possible to compile the bytecode and convert it to JavaScript. Now it's possible to run the Vult compiler as follows:

```
$ node vultjs.js arguments
```

Thanks to the npm it is even easier to install Vult an run it.

```
$ npm install vult -g
```

This command will give you an "executable" that can be called simply as `vultc`, for example:

```
$ vultc -ccode infile.vult -o outfile
```

This is the repository for the package https://github.com/modlfo/vultjs and this is the main page of the npm package https://www.npmjs.com/package/vult

I will update the source code every time I think the changes in trunk are useful.




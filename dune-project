(lang dune 3.7)

(generate_opam_files true)

(source
 (github vult-dsp/vult))

(authors "Leonardo Laguna Ruiz")

(maintainers "Leonardo Laguna Ruiz")

(license MIT)


(package
 (name vult)
 (synopsis "Transcompiler for DSP audio")
 (depends ocaml dune containers pla js_of_ocaml js_of_ocaml-ppx ounit)
)

; removes the Dune__exe prefix

(wrapped_executables false)

(map_workspace_root false)

(name vult)

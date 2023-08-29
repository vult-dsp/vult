(lang dune 2.4)

(generate_opam_files true)

(source
 (github vult-dsp/vult))

(authors "Leonardo Laguna Ruiz")

(maintainers "Leonardo Laguna Ruiz")

(license MIT)


(package
 (name vult)
 (synopsis "Transcompiler for DSP audio")
 (depends ocaml dune containers pla js_of_ocaml ounit)
)

; removes the Dune__exe prefix

(wrapped_executables false)

(name vult)

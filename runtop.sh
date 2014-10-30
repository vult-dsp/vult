ocamlbuild vultc.byte
echo "#load \"lexerVult.cmo\";;" > toplevel.init
ocaml -I _build -init toplevel.init

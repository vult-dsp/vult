ocamlbuild vultc.byte
echo "\
#load \"printTypes.cmo\";;\
#load \"lexerVult.cmo\";;\
#load \"parserVult.cmo\";;\
" > toplevel.init
ocaml -I _build -init toplevel.init

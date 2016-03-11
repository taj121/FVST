# Formal Verification of Session Types 

##Running instructions

$ ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core main.native

then

$ ./main.native tests/swapService.ml
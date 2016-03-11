# Formal Verification of Session Types 

##Running instructions
<pre>
 ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core main.native
</pre>
then
<pre>
 ./main.native tests/swapService.ml
 </pre>

# Formal Verification of Session Types 

Full details of the motivation behind this project can be found in the report. 

##Pre Requisits 
Ocaml 

<pre>
sudo apt-get install ocaml ocaml-native-compilers ocaml-doc tuareg-mode ocaml-findlib oasis libpre-ocaml-dev 
</pre>

##Running instructions
<pre>
 ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core main.native
</pre>
then
<pre>
 ./main.native tests/<testname.ml>
</pre>

##Code
The main.ml file contains the code to run the project. This calls the relevant methods from the parser.mly and lexer.mll (the parser and lexer files) as well as the Behaviour.ml file. 

All code for the parser is in the parser.mly file. 

All code for the lexer is in the lexer.mll file.

The Behaviour.ml file contains the type declarations, the to_string methods, the constraint storage functions and the behaviour checker functions.

##Tests

The test for this system are included in the test folder. The subfolders include one named "from start" The tests in this folder are written to be run through the first level of the system associated with this behaviour checker. The tests in the "behaviour checker tests" folder includes test that will pass and fail. Some of these tests will fail due to the session endpoint types not having been substituted for into the code.

Parser tests are test that are not designed to be checked by the behaviour checker but only to ensure that the lexer and parser work correctly together.

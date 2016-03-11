open Behaviour
open Core.Std
open Lexer
open Lexing

(*Code from examples supplied with real world OCaml Chanpter 16*)
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.parse_behaviour Lexer.lex lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    (* (None,None) *)(*attempt at printing behaviours and constraints*) 
    (None)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  (* | (valueb ,valuec) ->  printf "%a\n\n\n%a\n" Behaviour.output_b valueb Behaviour.output_con; *) (*attempt at printing behaviours and constraints*) 
  | (*Some*) (valueb ) -> 
    printf "%a\n" Behaviour.output_b valueb ;
    parse_and_print lexbuf
  (* | (None,None) -> () *) (*attempt at printing behaviours and constraints*) 
  | (None) -> ()

 let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx 

 let () =
  Command.basic ~summary:"Parse and display behaviours"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop 
  |> Command.run  

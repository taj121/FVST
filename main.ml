open Behaviour
open Core.Std
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.parse_behaviour Lexer.lex lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

(* part 1 *)
let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | (*Some*) value ->
    printf "%a\n" Behaviour.output_value value;
    parse_and_print lexbuf
  | None -> ()

 let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx 

(* part 2 *)
 let () =
  Command.basic ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop 
  |> Command.run  

(* let process_line (line : string) =
  let linebuf = Lexing.from_string line in
  try
    (* Run the parser on this line of input. *)
    (* Printf.printf "%d\n%!" (Parser.parse_behaviour Lexer.lex linebuf) *)
    try Parser.parse_behaviour Lexer.lex lexbuf
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)

let process_op_line (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process_line line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process_op_line optional_line;
  if continue then
    repeat channel
  
let () =
  repeat (Lexing.from_channel stdin) *)
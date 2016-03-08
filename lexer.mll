{
(*type token =
  | BEVAR of (string)
  | SESSTYPE of (string)
  | LABLE of (string)
  | TAU of (string)
  | REG of (string)
  | CHOICE
  | REC
  | SPAWN
  | PUSH 
  | SND
  | RECI
  | NULL
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACE_SQ
  | RIGHT_BRACE_SQ
  | LEFT_BRACK
  | RIGHT_BRACK
  | COLON
  | COMMA
  | OPTION
  | EOF
  | TYPE of (string)*)
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = 	[' ' '\t']+
let newline = 	'\r' | '\n' | "\r\n"
let tab   = '\009'
let lf    = '\010'
let cr    = '\013'
let eol   = cr | lf | cr lf

(*rule line = parse
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }*)

rule lex = parse
(*and lex = parse*)
  | white    		{ lex lexbuf }
  | newline  		{ next_line lexbuf; lex lexbuf }
  (*| eol 			{ EOL }*)
  | ";"           	{ COLON }
  | ","           	{ COMMA }
  | "("             { LEFT_BRACE }
  | ")"             { RIGHT_BRACE }
  | "["             { LEFT_BRACE_SQ }
  | "]"             { RIGHT_BRACE_SQ }
  | "chc"			{ CHOICE }
  | "spn"			{ SPAWN }
  | "psh"			{ PUSH }
  | "rec"			{ REC }
  | "!"				{ SND }
  | "?"				{ RECI }
  | "optn"			{ OPTION } 
  | "tau"			{ TAU }
  | "$"				{ read_label (Buffer.create 17) lexbuf }
  | "unit"			{ UNIT }
  | "bool"			{ BOOL }
  | "int"			{ INT }
  | "pair"			{ PAIR }
  | "funct"			{ FUNCT }
  | "ses"			{ SES }
  | "->"			{ ARROW }
  | "-"				{ DASH }
  | "end" 			{ SESEND } 
  | "(+)" 			{ SCHOICE }
  | "+"				{ SECHOICE }
  | "<"				{ SUBSET }
  | "~"				{ LINK }			
  | ['C']['0'-'9' 'A'-'Z' 'a'-'z' '_']+ as s {CHANNEL (s)}
  | ['C']['N']['0'-'9' 'A'-'Z' 'a'-'z' '_']+ as s {CHANNELEND (s)}
  | ['S']['0'-'9' 'A'-'Z' 'a'-'z' '_']+ as s { SVAR (s) }
  | ['B']['0'-'9' 'A'-'Z' 'a'-'z' '_']+ as s { BEVAR (s) }
  | ['R']['0'-'9' 'A'-'Z' 'a'-'z' '_']+ as s { REG (s) }
  | ['T']['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s { TVAR (s) }
  | _ 				{ raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      		{ EOF }

and read_label buf =
  parse
  | '$'       { LABLE (Buffer.contents buf) }
  | [^ '$']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_label buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal lable character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("lable is not terminated")) }
	
{
type token =
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
  | TYPE of (string)
}

rule lex = parse
  | [' ' '\n']      { lex lexbuf }
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
  | ['B']['0'-'9' 'A'-'Z' 'a'-'z' '_']+ as s { BEVAR (s) }
  | ['T']['0'-'9' 'A'-'Z' 'a'-'z' '_']+ as s { TAU (s) }
  | ['L']['0'-'9' 'A'-'Z' 'a'-'z' '_']+ as s { LABLE (s) }
  | ['R']['0'-'9' 'A'-'Z' 'a'-'z' '_']+ as s { REG (s) }
  | ['T']['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s { TYPE (s) }
  (*| ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s { ID (s) }*)
  | eof             { EOF }
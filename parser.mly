

%token <string> BEVAR 
%token <string> SESSTYPE
%token <string> LABLE
%token <string> TAU
%token <string> REG
%token CHOICE
%token REC
%token SPAWN
%token PUSH
%token NULL
%token SND
%token RECI
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACE_SQ
%token RIGHT_BRACE_SQ
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token OPTION
%token EOF
%token <string> TYPE


%{
  open Lexer
  open Behaviour
%}
/*for the moment return string*/
%start <string> parse_behaviour 

%%

parse_behaviour:
  | EOF       { None }
  | b = behaviour { Some b};

behaviour :
  | var = BEVAR                           
      {BVar var} 
  | l = LABLE                           
      {<string> l}
  | t = TAU                                  
      {Tau t} 
  | b1 = behaviour COLON b2 = behaviour    
      {Seq (b1,b2)}
  | CHOICE LEFT_BRACE b1 = behaviour COMMA b2 = behaviour RIGHT_BRACE
      {ChoiceB (b1,b2)} 
  | REC var = BEVAR b = behaviour
      {RecB (var, b)}
  | SPAWN b = behaviour
      {Spawn (b)}
  | PUSH LEFT_BRACE l = LABLE COMMA s = SESSTYPE RIGHT_BRACE
      {Push (l,s)}
  | r = REG SND t = TYPE
      {SndType (r,t)}
  | r = REG RECI t = TYPE
      {RecType (r,t)}
  | r1 = REG SND r2 = REG
      {SndReg (r1,r2)}
  | r = REG RECI l = LABLE
      {RecLab (r,l)}
  | r = REG SND l = LABLE
      {SndChc (r,l)}
  | r = REG RECI OPTION LEFT_BRACE_SQ /*not sure how to do options*/RIGHT_BRACE_SQ
      {RecChoice (r,(*option list*))}
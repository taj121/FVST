

%token <string> BEVAR 
%token <string> SESSTYPE
%token <string> LABLE
%token TAU
%token <string> REG
%token CHOICE
%token REC
%token SPAWN
%token PUSH
%token SND
%token RECI
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACE_SQ
%token RIGHT_BRACE_SQ
%token OPTION
%token COLON
%token COMMA
%token EOF
%token <string> TYPE


%{
  (*open Lexer*)
  open Behaviour
%}
/*for the moment return string*/
%start <Behaviour.b> parse_behaviour 

%%

/*parse_behaviour:
  | EOF       {  None }
  | b = behaviour {b}
  ;*/
parse_behaviour: b = behaviour EOF {b}


behaviour :
  | var = BEVAR                           
      {BVar var} 
  /*| l = LABLE                           
      {Lab l}*/
  | TAU                                  
      {Tau} 
  | b1 = behaviour COLON b2 = behaviour    
      {Seq {b1=b1;b2=b2}}
  | CHOICE LEFT_BRACE b1 = behaviour COMMA b2 = behaviour RIGHT_BRACE
      {ChoiceB {opt1=b1;opt2=b2}} 
  | REC var = BEVAR b = behaviour
      {RecB {behaVar=var;behaviour=b}}
  | SPAWN LEFT_BRACE b = behaviour RIGHT_BRACE
      {Spawn {spawned=b}}
  | PUSH LEFT_BRACE l = LABLE COMMA s = SESSTYPE RIGHT_BRACE
      {Push {toPush={label=l;sessType=s}}}
  | r = REG SND t = TYPE
      {SndType {regionS=r;outTypeS=t}}
  | r = REG RECI t = TYPE
      {RecType {regionR=r;outTypeR=t}}
  | r1 = REG SND r2 = REG
      {SndReg {reg1=r1;reg2=r2}}
  | r = REG RECI l = LABLE
      {RecLab {regL=r;label=l}}
  | r = REG SND l = LABLE
      {SndChc {regCa=r;labl=l}};
 /* | r = REG RECI OPTION LEFT_BRACE_SQ RIGHT_BRACE_SQ
      {RecChoice (r,(*option list*))}*/ 
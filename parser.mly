

%token <string> BEVAR 
%token SESSTYPE
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
%token EOL
(*type tokens*)
%token UNIT
%token BOOL
%token INT
%token PAIR
%token FUNCT
%token SES
%token <string> TVAR
%token ARROW
%token DASH
(*session type tokens*)
%token SESEND
%token SCHOICE
%token SECHOICE
%token <string> SVAR

%{
  open Behaviour
%}

%start <Behaviour.b> parse_behaviour 

%%

/*parse_behaviour:
  | EOF       {  None }
  | b = behaviour {b}
  ;*/
parse_behaviour: 
  | b = behaviour EOF {b}
  /*| EOF {None}*/ ;


behaviour :
  | var = BEVAR                           
      {BVar var} 
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
  /*| PUSH LEFT_BRACE l = LABLE COMMA s = session RIGHT_BRACE
      {Push {toPush={label=l;sessType=s}}}*/
  | PUSH LEFT_BRACE l = LABLE COMMA s=sessionType RIGHT_BRACE
      {Push {toPush={label=l; sessType=s}}}
  | r = REG SND t = bType
      {SndType {regionS=r;outTypeS=t}}
  | r = REG RECI t = bType
      {RecType {regionR=r;outTypeR=t}}
  | r1 = REG SND r2 = REG
      {SndReg {reg1=r1;reg2=r2}}
  | r = REG RECI l = LABLE
      {RecLab {regL=r;label=l}}
  | r = REG SND l = LABLE
      {SndChc {regCa=r;labl=l}}
 /* | r = REG RECI OPTION LEFT_BRACE_SQ RIGHT_BRACE_SQ
      {RecChoice (r,(*option list*))}*/ 

/*need to parse session types. think the way to do this is
to have another thing like behaviour... */

sessionType:
  | SESEND
    { EndTag }
  | SND t=bType s=sessionType
    {InputConfinded { inValue=t; sTypeIn=s} }
  | RECI t=bType s=sessionType
    {OutputConfinded { outValue=t; sTypeOut=s} }
  | SND s1=sessionType s2=sessionType
    {Delegation { sTypeD=s1; sTypeD2=s2}}
  | RECI s1=sessionType s2=sessionType
    {Resumption { sTypeR=s1; sTypeR2=s2} }
  | s=SVAR
    {SVar s}

  bType:
  | UNIT
    {Unit }
  | BOOL
    {Bool }
  | INT
    {Int }
  | PAIR LEFT_BRACE t1=bType COLON t2=bType RIGHT_BRACE 
    {Pair {type1=t1 ; type2=t2 }}
  | FUNCT t1=bType ARROW t2=bType DASH b=BEVAR
    {Funct {inType=t1; outType=t2; behav=b}}
  | SES r=REG
    {Ses {rVar=r}}
  | t=TVAR
    {TVar t}
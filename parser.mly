

%token <string> BEVAR 
%token <string> LABLE
%token <string> REG
%token TAU
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
(*constraint tokens*)
%token SUBSET
%token LINK
%token <string> CHANNEL
%token <string> CHANNELEND

%right COLON
%right COMMA

%{
  open Behaviour
%}

%start parse_behaviour /*parse_constraint*/
/*%type <Behaviour.b * Behaviour.con> parse_behaviour*/  /*best attempt so far at printing both the behaviours and the constraints from file*/
%type <Behaviour.b> parse_behaviour  /*working version of above with only behaviours printed*/
/*%type <Behaviour.con> parse_constraint */

%%


parse_behaviour: 
  /*| EOF {(None,None)}*/
 /* | b = behaviour c = constr EOF {(b,c)}*/ /*best attempt so far at printing both the behaviours and the constraints from file*/
  | b = behaviour c = constr EOF {(b)} /*working version with only behaviours printed*/
  ;

/*parse_constraint:
  | c = constr EOF {c}
  ;*/


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
  | PUSH LEFT_BRACE l=LABLE COMMA s=sessionType RIGHT_BRACE
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
  | r = REG RECI OPTION LEFT_BRACE_SQ o=oplist RIGHT_BRACE_SQ
      {RecChoice {regCb=r;cList=o}};

oplist:
  opt = separated_list(COMMA, opt_field)    
    { opt } ;

opt_field:
  LEFT_BRACE l=LABLE COLON b=behaviour RIGHT_BRACE
    {(l,b)};

sessionType:
  | SESEND
    { EndTag }
  | SND t=bType s=sessionType
    {InputConfinded { inValue=t; sTypeIn=s} }
  | RECI t=bType LEFT_BRACE s=sessionType RIGHT_BRACE
    {OutputConfinded { outValue=t; sTypeOut=s} }
  | SND s1=sessionType s2=sessionType
    {Delegation { sTypeD=s1; sTypeD2=s2}}
  | RECI s1=sessionType s2=sessionType
    {Resumption { sTypeR=s1; sTypeR2=s2} }
  | SCHOICE LEFT_BRACE_SQ s=sesOpL RIGHT_BRACE_SQ LEFT_BRACE l=LABLE COLON t=sessionType RIGHT_BRACE
    {ChoiceS {opList=s; sent=(l,t)}}
  | SECHOICE LEFT_BRACE_SQ s1=sesOpL RIGHT_BRACE_SQ LEFT_BRACE_SQ s2=sesOpL RIGHT_BRACE_SQ
    {ExtChoicS {opList1=s1;opList2=s2}}
  | s=SVAR
    {SVar s};

sesOpL:
  opt = separated_list(COMMA, ses_opt_field)    
    { opt } ;

ses_opt_field:
  LEFT_BRACE l=LABLE COLON b=sessionType RIGHT_BRACE
    {(l,b)};

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
    {TVar t};

regionVar:
  | l=LABLE
    {Label l}
  | r= REG
    {RVar r}

constr:
  | t1=bType SUBSET t2=bType
    {TCon {smlT=t1; bigT=t2}}
  | b=behaviour SUBSET beta=BEVAR
    {BCon {smlB=b;bigB=beta}}
  | r=REG LINK r1=regionVar
    {RegRel {reg=r;regLab=r1}}
  | c=CHANNEL LINK s=sessionType 
    {ConRel {chnlA=c; endptA=s}}
  | c=CHANNELEND LINK s=sessionType 
    {ConRelAlt {chnlB=c; endptB=s}}
  | c= constr COMMA c2= constr
    {ConSeq {con1=c;con2=c2}}
  |
    {None}

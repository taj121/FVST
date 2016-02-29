(* type typeVar = TVar of string ;;
type beVar = BVar of string ;;
type sesVar = SVar of string ;;
type regVar = RVar of string ;;

type region = Label of string | RVar of regVar ;;

type t = Unit of unit 
	| Bool of bool 
	| Int of int
	| Pair of pair 
	| Funct of func 
	| Ses of ses
	| TVar of typeVar 
	and ses = {rVar : regVar}
	and func = {inType : t ; outType : t ; behav : beVar}
	and pair = {type1 : t ; type2 : t} ;;


type sesType = EndTag of string 
	| InputConfinded of inConT 
	| OutputConfinded of outConT 
	| Delegation of del 
	| Resumption of res 
	| ChoiceS of choiceS
	| ExtChoicS of extChoiceS 
	| SVar of sesVar
and extChoiceS = { opList1 : option list ; opList2 : option list}
and choiceS = { opList : option list}
and option = { label : string ; sType : sesType}
and inConT = { inValue : string ; sTypeIn : sesType}
and outConT = { outValue : string ; sTypeOut : sesType}
and del = { delValue : string ; sTypeD : sesType ; sTypeD2 : sesType}
and res = { resValue : string ; sTypeR : sesType ; sTypeR2 : sesType} ;;*)

type stackFrame = {label: string ; sessType : string} ;; 

type snd = Snd ;;
type reci = Rec ;;
type chnl = Chnl ;;  

type b = BVar of string
	| Tau of string 
	| Seq of seq 
	| ChoiceB of choiceB
	| RecB of recB
	| Spawn of spawn
	| Push of push 
	| SndType of outT
	| RecType of recT
	| SndReg of sndR
	| RecLab of recL
	| SndChc of sndC
	| RecChoice of recC
and sndC = { regCa : string ; labl : string}
and recC = { regCb : string ; cList : optionB list}
(* and sndC = { regC : string ; actC : string ; cList : optionB list} *)
and optionB = { labelO : string ; beOpt : b }
and recL = { regL : string ; label : string}
(* and recL = { regL : string ; actRL : reci ; label : string} *)
and sndR = { reg1 : string ; reg2 : string}
(* and sndR = { reg1 : string ; actSR : snd ; reg2 : string} *)
and recT = { regionR : string ; outTypeR : string}
(* and recT = { regionR : string ; actR : snd ; outTypeR : string} *)
(* and outT = { regionS : string ; actS : reci ; outTypeS : string} *)
and outT = { regionS : string ; outTypeS : string}
and push = { toPush : stackFrame}
and spawn = { spawned : b}
and recB = { behaVar : string ; behaviour : b} 
and choiceB = {opt1 : b ; opt2 : b}
and seq = {b1 : b ; b2 : b} ;;
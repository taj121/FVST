open Camlp4.PreCast;;
module SesTypeGram = MakeGram Lexer;;

type typeVar = TVar of string ;;
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
	| ExtChoicS of extChoiceS (*not sure how to represent the constraints on the option lists*)
	| SVar of sesVar
and extChoiceS = { opList1 : option list ; opList2 : option list}
and choiceS = { opList : option list}
and option = { label : string ; sType : sesType}
and inConT = { inValue : string ; sTypeIn : sesType}
and outConT = { outValue : string ; sTypeOut : sesType}
and del = { delValue : string ; sTypeD : sesType ; sTypeD2 : sesType}
and res = { resValue : string ; sTypeR : sesType ; sTypeR2 : sesType} ;;

type stackFrame = {label: string ; sessType : sesType} ;; 

type snd = Snd ;;
type reci = Rec ;;
type chnl = Chnl ;; (* of int? string? other?*)

type b = BVar of beVar
	| Reg of regVar (*is this right? not sure i read symbol in paper right*)
	| Seq of seq 
	| ChoiceB of choiceB
	| RecB of recB
	| Spawn of spawn
	| Push of push 
	| SndType of outT
	| RecType of recT
	| SndReg of sndR
	| RecLab of recL
	| RecChoice of recC
and recC = { regC : regVar ; actC : reci ; cList : optionB }
and optionB = { labelO : string ; beOpt : b }
and recL = { regL : regVar ; actRL : reci ; label : string}
and sndR = { reg1 : regVar ; actSR : snd ; reg2 : regVar}
and recT = { regionR : regVar ; actR : snd ; outTypeR : t}
and outT = { regionS : regVar ; actS : reci ; outTypeS : t}
and push = { toPush : stackFrame}
and spawn = { spawned : b}
and recB = { behaVar : beVar ; behaviour : b} 
and choiceB = {opt1 : b ; opt2 : b}
and seq = {b1 : b ; b2 : b} ;;


type con = TCon of tCon
	| BCon of bCon
	| RegRel of regRel
	| ConRel of conRel
	| ConRelAlt of conRelAlt
	| ConSeq of conSeq
	| Nothing
and tCon = {smlT : t ; bigT : t}
and bCon = {smlB : b ; bigB : beVar}
and regRel = {reg : regVar ; regLab : region}
and conRel = {chnlA : chnl ; endptA : sesType}
and conRelAlt = {chnlB : chnl ; endptB : sesType}
(* endpt A and B to match? *)
and conSeq = {con1 : con ; con2 : con} ;;






open Core.Std


(* type typeVar = TVar of string ;;
type beVar = BVar of string ;;
type sesVar = SVar of string ;;
type regVar = RVar of string ;; 
type snd = Snd ;;
type reci = Rec ;;
type chnl = Chnl ;;
removingt this level since I cant find a way to read in from parser*)

type region = Label of string | RVar of string ;;



type t = Unit (*of unit*) 
	| Bool (*of bool *)
	| Int (*of int*)
	| Pair of pair 
	| Funct of func 
	| Ses of ses
	| TVar of string 
	and ses = {rVar : string}
	and func = {inType : t ; outType : t ; behav : string}
	and pair = {type1 : t ; type2 : t} ;;

let rec type_to_string typ = 
	match typ with 
	| Unit  -> " unit "
	| Bool  -> "bool"
	| Int  -> "int"
	| Pair {type1=a;type2=b} -> " (" ^ "type1" ^ " X " ^ "type 2" ^ ") " (*fix v*)
	| Funct {inType=a; outType=b; behav=c} -> " (" ^ "in type" ^ " -" ^ "some behaviour" ^ "->" ^ "out type" ^ ") "
	| Ses {rVar=a} -> "ses over reg" 
	| TVar t -> " type var "
;;

type sesType = 
	| EndTag (*of string*) 
	| InputConfinded of inConT 
	| OutputConfinded of outConT 
	| Delegation of del 
	| Resumption of res 
	| ChoiceS of choiceS
	| ExtChoicS of extChoiceS 
	| SVar of string
and extChoiceS = { opList1 : (string * sesType) list ; opList2 : (string * sesType) list}
and choiceS = { opList : (string * sesType) list ; sent: (string* sesType)}
(* and option = { label : string ; sType : sesType} *)
and inConT = { inValue : t ; sTypeIn : sesType}
and outConT = { outValue : t ; sTypeOut : sesType}
and del = { sTypeD : sesType ; sTypeD2 : sesType}
and res = { sTypeR : sesType ; sTypeR2 : sesType} ;; 

(* session type to string *)


let rec sess_to_string (s: sesType) = 
	match s with 
	| EndTag ->  "endSesTyp"
	| InputConfinded {inValue=a; sTypeIn=b}-> "Recieved: " ^ type_to_string a ^ (sess_to_string b)
	| OutputConfinded {outValue=a; sTypeOut=b}-> "Output: " ^ type_to_string a ^ (sess_to_string b)
	| Delegation {sTypeD=b ; sTypeD2=c} -> "Delegate " ^ (sess_to_string b) ^ " over " ^ (sess_to_string c)
	| Resumption {sTypeR=b ; sTypeR2=c} -> "Resume " ^ (sess_to_string b) ^ " from " ^ (sess_to_string c)
	| ChoiceS {opList=a} ->  "Choose: \n " ^ f a ^ "end Choice\n"
	| ExtChoicS {opList1=a ; opList2=b} -> "Must Accept: \n" ^ f a ^ "May Accept: \n" ^ f b ^ "\n"
	| SVar var -> "session variable"

and f op = 
	match op with 
	| [] -> "";
	| (a,b)::l -> "\t ("^a ^"; "^(sess_to_string (b))^" )\n" ^ f l  
;;

type stackFrame = {label: string ; sessType : sesType} ;; 
  

type b = 
	| BVar of string
	| Tau (* of string *) 
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
	| None 
	(* | Some of b  *)
and sndC = { regCa : string ; labl : string}
(* and recC = { regCb : string ; cList : optionB list} *)
and recC = { regCb : string ; cList : (string * b) list}
(* and sndC = { regC : string ; actC : string ; cList : optionB list} *)
(* and optionB = { labelO : string ; beOpt : b } *)
and recL = { regL : string ; label : string}
(* and recL = { regL : string ; actRL : reci ; label : string} *)
and sndR = { reg1 : string ; reg2 : string}
(* and sndR = { reg1 : string ; actSR : snd ; reg2 : string} *)
and recT = { regionR : string ; outTypeR : t}
(* and recT = { regionR : string ; actR : snd ; outTypeR : string} *)
(* and outT = { regionS : string ; actS : reci ; outTypeS : string} *)
and outT = { regionS : string ; outTypeS : t}
and push = { toPush : stackFrame}
and spawn = { spawned : b}
and recB = { behaVar : string ; behaviour : b} 
and choiceB = {opt1 : b ; opt2 : b}
and seq = {b1 : b ; b2 : b};;



(* print out behaviours *)
let rec output_value outc = function
	| BVar s 									-> printf "Bvar fix \n"
 	| Tau  										-> printf "Tau \n" 
 	| Seq {b1=b_1;b2=b_2} 						-> print_seq outc b_1 b_2
 	| ChoiceB {opt1=op1;opt2=op2} 				-> print_choice outc op1 op2
 	| RecB {behaVar=beta;behaviour=b} 			-> print_rec outc beta b
 	| Spawn {spawned=b} 						-> print_spwn outc b
 	| Push {toPush={label=lab;sessType=sTyp}} 	-> printf "push (%s, %s)\n" lab (sess_to_string sTyp)
 	| SndType {regionS=reg;outTypeS=typ} 		-> printf "Send %s over region tofix\n" (type_to_string typ) 
	| RecType {regionR=reg;outTypeR=typ} 		-> printf "Recive %s over region tofix\n" (type_to_string typ) 
	| SndReg {reg1=r1;reg2=r2} 					-> printf "Delegate tofix \n" 
	| RecLab {regL=r;label=lab} 				-> printf "Resume tofix %s \n"  lab
	| SndChc {regCa=reg;labl=lab} 				-> printf "Select tofix %s \n" lab
	| RecChoice {regCb=reg ; cList= lst}    	-> print_op_list outc reg lst
	(* | None 									 	-> printf "\nEOF\n" *)
	| _ 										-> printf "err\n "   
 
 and print_op_list outc reg lst=
 	output_string outc "Receive: \n[";
 	List.iter ~f:(fun (lable, behav) -> printf "(%s, %a)" lable output_value behav) lst;
 	output_string outc " over region ";
 	output_string outc reg;


 and print_seq outc b1 b2 =  
 	output_value outc b1 ;
 	output_value outc b2 ;

 and print_choice outc op1 op2 = 
 	output_string outc "choose: \n\t";
 	output_value outc op1 ;
 	output_string outc "or \n\t";
 	output_value outc op2 ;
 	output_string outc "end choice\n";

 and print_rec outc beta b =
 	output_string outc "recursive behaviour: ";
 	output_value outc b;

 and print_spwn outc b =
 	output_string outc "spawn: ";
 	output_value outc b;

type con = 
	| TCon of tCon
	| BCon of bCon
	| RegRel of regRel
	| ConRel of conRel
	| ConRelAlt of conRelAlt
	| ConSeq of conGroup
	| None
and conGroup = {con1 : con ; con2 : con}
and tCon = {smlT : t ; bigT : t}
and bCon = {smlB : b ; bigB : string}
and regRel = {reg : string ; regLab : region}
and conRel = {chnlA : string ; endptA : sesType}
and conRelAlt = {chnlB : string ; endptB : sesType};;
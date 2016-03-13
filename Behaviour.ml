

open Core.Std

(*region type*)
type region = Label of string | RVar of string ;;

(* region type to string *)
let region_to_string r=
	match r with 
	| Label l 	-> l 
	| RVar r  	-> r
;;

(*type for types*)
type t = Unit 
	| Bool 
	| Int 
	| Pair of pair 
	| Funct of func 
	| Ses of ses
	| TVar of string 
	and ses = {rVar : string}
	and func = {inType : t ; outType : t ; behav : string}
	and pair = {type1 : t ; type2 : t} ;;

(* type to string *)
let rec type_to_string typ = 
	match typ with 
	| Unit  -> " unit "
	| Bool  -> "bool"
	| Int  -> "int"
	| Pair {type1=a;type2=b} -> "Pair (" ^ type_to_string a ^ "; " ^ type_to_string b ^ ") " 
	| Funct {inType=a; outType=b; behav=c} -> "Funct " ^ type_to_string a  ^ "->" ^ type_to_string b^ " -" ^  c
	| Ses {rVar=a} -> "ses " ^ a 
	| TVar t -> t
;;

(* session types *)
type sesType = 
	| EndTag 
	| InputConfinded of inConT 
	| OutputConfinded of outConT 
	| Delegation of del 
	| Resumption of res 
	| ChoiceS of choiceS
	| ExtChoicS of extChoiceS 
	| SVar of string
and extChoiceS = { opList1 : (string * sesType) list ; opList2 : (string * sesType) list}
and choiceS = { opList : (string * sesType) list ; sent: (string* sesType)}
and inConT = { inValue : t ; sTypeIn : sesType}
and outConT = { outValue : t ; sTypeOut : sesType}
and del = { sTypeD : sesType ; sTypeD2 : sesType}
and res = { sTypeR : sesType ; sTypeR2 : sesType} ;; 

(* session type to string *)
let rec sess_to_string (s: sesType) = 
	match s with 
	| EndTag ->  "end"
	| InputConfinded {inValue=a; sTypeIn=b}-> "! " ^ type_to_string a ^ " "^(sess_to_string b)
	| OutputConfinded {outValue=a; sTypeOut=b}-> "? " ^ type_to_string a ^  " "^(sess_to_string b)
	| Delegation {sTypeD=b ; sTypeD2=c} -> "! " ^ (sess_to_string b) ^ " " ^ (sess_to_string c)
	| Resumption {sTypeR=b ; sTypeR2=c} -> "? " ^ (sess_to_string b) ^ " " ^ (sess_to_string c)
	| ChoiceS {opList=a; sent=(b,c)} ->  "(+)[" ^ f a ^ "] ("^ b ^"; " ^ sess_to_string c ^ ")"
	| ExtChoicS {opList1=a ; opList2=b} -> "+[" ^ f a ^ "][ " ^ f b ^ "] "
	| SVar var -> var
(* option list to string *)
and f op = 
	match op with 
	| [] -> "";
	| (a,b)::l -> " ("^a ^"; "^(sess_to_string (b))^" ) " ^ f l  
;;

(* stack frame *)
type stackFrame = {label: string ; sessType : sesType} ;; 
 
(* behaviours *)
type b = 
	| BVar of string
	| Tau 
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
	| None  (*None included due to requirements to run main file*)
and sndC = { regCa : string ; labl : string}
and recC = { regCb : string ; cList : (string * b) list}
and recL = { regL : string ; label : string}
and sndR = { reg1 : string ; reg2 : string}
and recT = { regionR : string ; outTypeR : t}
and outT = { regionS : string ; outTypeS : t}
and push = { toPush : stackFrame}
and spawn = { spawned : b}
and recB = { behaVar : string ; behaviour : b} 
and choiceB = {opt1 : b ; opt2 : b}
and seq = {b1 : b ; b2 : b};;

(* function for checking behaviours  *)
(* let rec check_behav b s=
match b with 
|  *)

(* behaviour to string *)
let rec behaviour_to_string (b:b) =
	match b with 
	| BVar s 									-> s
 	| Tau  										-> "Tau" 
 	| Seq {b1=b_1;b2=b_2} 						-> behaviour_to_string b_1 ^" ;\n "^ behaviour_to_string b_2 
 	| ChoiceB {opt1=op1;opt2=op2} 				-> "chc( " ^ behaviour_to_string op1 ^ ", " ^ behaviour_to_string op2 ^ ")"
 	| RecB {behaVar=beta;behaviour=b} 			-> "rec " ^ beta ^" "^ behaviour_to_string b 
 	| Spawn {spawned=b} 						-> "Spn( " ^ behaviour_to_string b ^" )"
 	| Push {toPush={label=lab;sessType=sTyp}} 	-> "Psh ( " ^ lab ^", "^  sess_to_string sTyp ^ " )" 
 	| SndType {regionS=reg;outTypeS=typ} 		-> reg ^ " ! " ^ type_to_string typ 
	| RecType {regionR=reg;outTypeR=typ} 		-> reg ^ " ? " ^ type_to_string typ   
	| SndReg {reg1=r1;reg2=r2} 					-> r1 ^ " ! "^ r2
	| RecLab {regL=r;label=lab} 				-> r ^" ? " ^ lab  
	| SndChc {regCa=reg;labl=lab} 				-> reg ^" ! "^ lab 
	| RecChoice {regCb=reg ; cList= lst}    	-> reg ^ " ? optn [" ^ p lst^ "]" 
	|  _ 										-> "\nerr\n " 

(* choice list to string *)
and p a=
	match a with 
	| [] -> ""
	| (a,b)::l -> "("^a^"; "^ behaviour_to_string b ^ ") " ^ p l 

(* constraints type *)
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

(*
idea for storing constraints for checking

hash table.

key is string version of first part

problems: two types of constrainst
first part could be really long
second parts can be different

idea: key is string of second part? less likely to be really long... still could be though
*)

(* constraints to string *)
let rec con_to_string c =
	match c with 
	| TCon {smlT=a;bigT=b}				-> type_to_string a ^ " < " ^ type_to_string b ^ " "
	| BCon {smlB=a;bigB=b}				-> behaviour_to_string a ^ " < " ^ b ^ " "
	| RegRel {reg=a;regLab=b} 			-> a ^ " ~ " ^ region_to_string b ^ " "    
	| ConRel {chnlA=a ; endptA=b}		-> a ^ " ~ " ^ sess_to_string b ^" "
	| ConRelAlt {chnlB=a ; endptB=b}	-> a ^ " ~ " ^ sess_to_string b ^" "
	| ConSeq {con1=a; con2=b}			-> con_to_string a ^"; "^con_to_string b  
	| None 								-> "empty"
;;


(* print out behaviours *)
let output_b outc input =  output_string outc (behaviour_to_string input);;
(*print out constraints*)
let output_con outc input =  output_string outc (con_to_string input);;

(* currently can only call one of these at a time from main, need to fix *)
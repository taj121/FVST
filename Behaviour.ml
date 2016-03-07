

open Core.Std


type typeVar = TVar of string ;;
type beVar = BVar of string ;;
type sesVar = SVar of string ;;
type regVar = RVar of string ;;

type region = Label of string | RVar of regVar ;;

type snd = Snd ;;
type reci = Rec ;;
type chnl = Chnl ;;

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


type sesType = 
	| EndTag (*of string*) 
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
and del = { delValue : snd ; sTypeD : sesType ; sTypeD2 : sesType}
and res = { resValue : reci ; sTypeR : sesType ; sTypeR2 : sesType} ;; 

(* session type to string *)


let rec sess_to_string (s: sesType) = 
	match s with 
	| EndTag ->  "endSesTyp"
	| InputConfinded {inValue=a; sTypeIn=b}-> "Recieved: " ^ a ^ (sess_to_string b)
	| OutputConfinded {outValue=a; sTypeOut=b}-> "Output: " ^ a ^ (sess_to_string b)
	| Delegation { delValue=a ; sTypeD=b ; sTypeD2=c} -> "Delegate " ^ (sess_to_string b) ^ " over " ^ (sess_to_string c)
	| Resumption { resValue=a ; sTypeR=b ; sTypeR2=c} -> "Resume " ^ (sess_to_string b) ^ " from " ^ (sess_to_string c)
	| ChoiceS {opList=a} ->  "Choose: \n " ^ f a ^ "end Choice\n"
	| ExtChoicS {opList1=a ; opList2=b} -> "Must Accept: \n" ^ f a ^ "May Accept: \n" ^ f b ^ "\n"
	| SVar var -> "session variable"

and f op = 
	match op with 
	| [] -> "";
	| {label=a;sType=b}::l -> "\t ("^a ^"; "^(sess_to_string (b))^" )\n" ^ f l  
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
and recC = { regCb : string ; cList : optionB list}
(* and sndC = { regC : string ; actC : string ; cList : optionB list} *)
and optionB = { labelO : string ; beOpt : b }
and recL = { regL : string ; label : string}
(* and recL = { regL : string ; actRL : reci ; label : string} *)
and sndR = { reg1 : string ; reg2 : string}
(* and sndR = { reg1 : string ; actSR : snd ; reg2 : string} *)
and recT = { regionR : string ; outTypeR : sesType}
(* and recT = { regionR : string ; actR : snd ; outTypeR : string} *)
(* and outT = { regionS : string ; actS : reci ; outTypeS : string} *)
and outT = { regionS : string ; outTypeS : sesType}
and push = { toPush : stackFrame}
and spawn = { spawned : b}
and recB = { behaVar : string ; behaviour : b} 
and choiceB = {opt1 : b ; opt2 : b}
and seq = {b1 : b ; b2 : b};;



(* print out behaviours *)
let rec output_value outc = function
	| BVar s 									-> printf "%s \n" s
 	| Tau  										-> printf "Tau \n" 
 	| Seq {b1=b_1;b2=b_2} 						-> print_seq outc b_1 b_2
 	| ChoiceB {opt1=op1;opt2=op2} 				-> print_choice outc op1 op2
 	| RecB {behaVar=beta;behaviour=b} 			-> print_rec outc beta b
 	| Spawn {spawned=b} 						-> print_spwn outc b
 	| Push {toPush={label=lab;sessType=sTyp}} 	-> printf "push (%s, %s\n" lab (sess_to_string sTyp)
 	| SndType {regionS=reg;outTypeS=typ} 		-> printf "Send %s over region %s\n" (sess_to_string typ) reg
	| RecType {regionR=reg;outTypeR=typ} 		-> printf "Recive %s over region %s\n" (sess_to_string typ) reg
	| SndReg {reg1=r1;reg2=r2} 					-> printf "Delegate %s %s \n" r1 r2
	| RecLab {regL=r;label=lab} 				-> printf "Resume %s %s \n" r lab
	| SndChc {regCa=reg;labl=lab} 				-> printf "Select %s %s \n" reg lab
	(* | None 										-> printf "\nEOF\n" *)
	  | _ 										-> printf "err\n "   

 and print_seq outc b1 b2 =  
 	output_value outc b1 ;
 	output_value outc b2 ;

 and print_choice outc op1 op2 = 
 	output_string outc "choose: \n";
 	output_value outc op1 ;
 	output_string outc " or \n";
 	output_value outc op2 ;
 	output_string outc "end choice";

 and print_rec outc beta b =
 	output_string outc "recursive behaviour: %s ";
 	output_value outc b;

 and print_spwn outc b =
 	output_string outc "spawn: ";
 	output_value outc b;

(* 	let rec output_value outc = function
  | `Assoc obj  -> print_assoc outc obj
  | `List l     -> print_list outc l
  | `String s   -> printf "\"%s\"" s
  | `Int i      -> printf "%d" i
  | `Float x    -> printf "%f" x
  | `Bool true  -> output_string outc "true"
  | `Bool false -> output_string outc "false"
  | `Null       -> output_string outc "null"

and print_assoc outc obj =
  output_string outc "{ ";
  let sep = ref "" in
  List.iter ~f:(fun (key, value) ->
      printf "%s\"%s\": %a" !sep key output_value value;
      sep := ",\n  ") obj;
  output_string outc " }"

and print_list outc arr =
  output_string outc "[";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        output_string outc ", ";
      output_value outc v) arr;
  output_string outc "]"*)
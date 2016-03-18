open Core.Std

(* TYPES *)

(*region type*)
type region = Label of string | RVar of string ;;

(*type for types*)
type t = Unit 
	| Bool 
	| Int 
	| Pair of pair 
	| Funct of func 
	| Ses of string
	| TVar of string 
	and func = {inType : t ; outType : t ; behav : string}
	and pair = {type1 : t ; type2 : t} ;;


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

(* TO STRING FUNCTIONS *)

(* region type to string *)
let region_to_string r=
	match r with 
	| Label l 	-> l 
	| RVar r  	-> r
;;

(* type to string *)
let rec type_to_string typ = 
	match typ with 
	| Unit  -> " unit "
	| Bool  -> "bool"
	| Int  -> "int"
	| Pair {type1=a;type2=b} -> "Pair (" ^ type_to_string a ^ "; " ^ type_to_string b ^ ") " 
	| Funct {inType=a; outType=b; behav=c} -> "Funct " ^ type_to_string a  ^ "->" ^ type_to_string b^ " -" ^  c
	| Ses a -> "ses " ^ a 
	| TVar t -> t
;;

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

(* TYPE CHECKING *)

(*
idea for storing constraints for checking

hash table.

key is string version of first part

problems: two types of constrainst
first part could be really long
second parts can be different

idea: key is string of second part? less likely to be really long... still could be though

new problem... not sure i can use generic stack/hash table with user defined types

storing as string in hash table for constraints since constant. 

*)



(* add constraints to the hashtable. 
Key = id^stringOf L.H.S *
Value = R.H.S  
id = B for behaviour, 
T for type 
R for reg 
C for channel
C' for dual 
*in the case of B key is beta and value is behaviour 

scrap above. New methode is key same value is actual value of con type*)
let rec con_add con (hshtbl) =
	match con with 
	| TCon {smlT=a;bigT=b} 				-> Hashtbl.add hshtbl ("T"^(type_to_string a)) (TCon {smlT=a;bigT=b} )
	| BCon {smlB=a;bigB=b}				-> Hashtbl.add hshtbl ("B"^b) (BCon {smlB=a;bigB=b}) 
	| RegRel {reg=a;regLab=b} 			-> Hashtbl.add hshtbl ("R"^a) (RegRel {reg=a;regLab=b})   
	| ConRel {chnlA=a ; endptA=b}		-> Hashtbl.add hshtbl ("C"^a) (ConRel {chnlA=a ; endptA=b}) 
	| ConRelAlt {chnlB=a ; endptB=b}	-> Hashtbl.add hshtbl ("C'"^a) (ConRelAlt {chnlB=a ; endptB=b}) 
	| ConSeq {con1=a; con2=b}			-> add_seq a b hshtbl
	| None 								-> Hashtbl.add hshtbl "none" (None)
(* 	| TCon {smlT=a;bigT=b} 				-> Hashtbl.add hshtbl ("T"^(type_to_string a)) (type_to_string b)
	| BCon {smlB=a;bigB=b}				-> Hashtbl.add hshtbl ("B"^b) (behaviour_to_string a) 
	| RegRel {reg=a;regLab=b} 			-> Hashtbl.add hshtbl ("R"^a) (region_to_string b)   
	| ConRel {chnlA=a ; endptA=b}		-> Hashtbl.add hshtbl ("C"^a) (sess_to_string b) 
	| ConRelAlt {chnlB=a ; endptB=b}	-> Hashtbl.add hshtbl ("C'"^a) (sess_to_string b) 
	| ConSeq {con1=a; con2=b}			-> add_seq a b hshtbl
	| None 								-> Hashtbl.add hshtbl "none" "" *)

and add_seq con_a con_b hshtbl = 
	con_add con_a hshtbl;
	con_add con_b hshtbl;
;;

(* function for checking behaviours  *)
let rec check_behav behaviour stack hash=
	match behaviour with 
	(* end *)
	(* Beta *)
	| BVar s 									-> const_check_b s  
	(* plus *)
 	| ChoiceB {opt1=op1;opt2=op2} 				-> (check_choice op1 stack hash) && (check_choice op2 stack hash)
	(* push *)
 	| Push {toPush=a} 							-> push_stack_frame stack a
	(* out *)
	| SndType {regionS=reg;outTypeS=typ} 		-> (const_check_t_out typ reg stack hash)
	(* in *)
	| RecType {regionR=reg;outTypeR=typ} 		-> (const_check_t_in typ reg stack hash)
	(* del *) 
	| SndReg {reg1=r1;reg2=r2} 					-> del_check (Stack.pop stack) (Stack.pop stack) r1 r2
	(* res *)
	(* ICh *) 
	(* ECh *)
	(* rec *)
	(* spn *)
	(* seq *)
 	| Seq {b1=b_1;b2=b_2} 						-> (check_behav b_1 stack hash) && (check_behav b_2 stack hash) 
	(* tau *)

	(* | Tau  										->  *)
 	(* | RecB {behaVar=beta;behaviour=b} 			->   *)
 	(* | Spawn {spawned=b} 						->   *)
 (* 	| SndType {regionS=reg;outTypeS=typ} 		->  
	| RecType {regionR=reg;outTypeR=typ} 		->    
	| SndReg {reg1=r1;reg2=r2} 					-> 
	| RecLab {regL=r;label=lab} 				->   
	| SndChc {regCa=reg;labl=lab} 				->  
	| RecChoice {regCb=reg ; cList= lst}     	->  
	|  _ 										->   *)

and del_check {label=lab_top;sessType=sess_top} {label=lab_2;sessType=sess_2} r1 r2 stack hash= 
	(*  *)
	let allowed = (check_const ("R"^r1) lab_top hash) && (check_const ("R"^r2) lab_2 hash) in
	match allowed with 
	| true 	-> 
		(
		match sess_top with 
		| Delegation {sTypeD=n1;sTypeD2=n2} -> 
			(
			match check_sess_rel sess_2 n1 with 
			| true 	-> Stack.push stack {label=lab_top;sessType=n2}; true
			| _ 	-> false
			)
		| _ 								-> false
		)
	| _ 	-> false 

(* check constraints in hash table  *)
(* TODO only checking first binding *)
and check_const lhs rhs hash =
	match Hashtbl.find hash lhs with 
	| Some rhs 	-> true
	| _			-> false

(* ses1 <: ses2 *)
and check_sess_rel ses1 ses2 = true

(* check if constraints allow region to label and T1 < T2 *)
 and const_check_t_out typ reg stack hash =
	let frame = Stack.top stack in
	match frame with 
	| Some {label=lab;sessType=sess} 	-> 
		(
		match sess with 
		| OutputConfinded {outValue=typ';sTypeOut=styp}	-> 
			(
			match check_const ("R"^reg) (RegRel {reg=a;regLab=b} ) hash with 
			| true 	-> 
				(
				match b with 
				| Label l 	-> 
					(match compare l lab with 
					| 0 	-> 
						(
						match check_type_rel typ typ' with
						| true		-> update_stack stack styp lab
						| _ 		-> false
						)
					| _ 	-> false
					)
				| _		 -> false
				)
			| _        -> false
			)
		| _ 		-> false
		)
	| None 		-> false

(* check if constraints allow region to label and T1 < T2 *)
 and const_check_t_in typ reg stack hash =
	let frame = Stack.top stack in
	match frame with 
	| Some {label=lab;sessType=sess} 	-> 
		(
		match sess with 
		| InputConfinded {inValue=typ';sTypeIn=styp}	-> 
			(
			match check_const ("R"^reg) (RegRel {reg=a;regLab=b} ) hash with 
			| true 	-> 
				(
				match b with 
				| Label l 	-> 
					(
					match compare l lab with 
					| 0 	-> 
						(
						match check_type_rel typ' typ with
						| true	-> update_stack stack styp lab
						| _ 	-> false
						)
					| _ 	-> false
					)
				| _		 -> false
				)
			| _        -> false
			)
		| _ 		-> false
		)
	| None 		-> false

and update_stack stack newSess lab =
	Stack.pop stack;
	Stack.push stack {label=lab;sessType=newSess};
	true

(* check if t1 <: t2  *)
and check_type_rel t1 t2 = true;

(* and get_list tbl key = Hashtbl.find_all tbl key    *)

(* compare two strings true if same false if not *)
and comp_string s1 s2 = 
	match (compare s1 s2) with 
	| 0 	-> true
	| _ 	-> false

(* itterate through the stack checking each frame for the label of this frame if not found push & teturn true, if found dont & return false*)
and push_stack_frame stack frame =
	match (Stack.exists stack (match_label frame)) with 
	| false 	-> Stack.push stack frame; true
	| _			-> false

(* see if labels match in stack frames *)
and match_label {label=a1;sessType=b1} {label=a2;sessType=b2}=
	let check = compare a1 a2 in
	match check with 
	| 0 -> true
	| _ -> false

(* call main function with a copy of stack *)
and check_choice op1 stack hash = 
	let s1 = Stack.copy stack in
	check_behav op1 s1 hash

(* check b < Beta	 *)
and const_check_b beta = 
	match beta with 
	|_ -> true
;;

(* print out behaviours *)
let output_b outc input =  output_string outc (behaviour_to_string input);;
(*print out constraints*)
let output_con outc input =  output_string outc (con_to_string input);;

(* currently can only call one of these at a time from main, need to fix *)
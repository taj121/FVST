module Hash = Hashtbl
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
	(* None included due to requirements to run main file *)
	| None  
and sndC = { regCa : string ; labl : string}
and recC = { regCb : string ; cList : (string * b) list } 
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
	| BVar s 											-> s
 	| Tau  												-> "Tau" 
 	| Seq {b1=b_1;b2=b_2} 								-> behaviour_to_string b_1 ^" ;\n "^ behaviour_to_string b_2 
 	| ChoiceB {opt1=op1;opt2=op2} 						-> "chc( " ^ behaviour_to_string op1 ^ ", " ^ behaviour_to_string op2 ^ ")"
 	| RecB {behaVar=beta;behaviour=b} 					-> "rec " ^ beta ^" "^ behaviour_to_string b 
 	| Spawn {spawned=b} 								-> "Spn( " ^ behaviour_to_string b ^" )"
 	| Push {toPush={label=lab;sessType=sTyp}} 			-> "Psh ( " ^ lab ^", "^  sess_to_string sTyp ^ " )" 
 	| SndType {regionS=reg;outTypeS=typ} 				-> reg ^ " ! " ^ type_to_string typ 
	| RecType {regionR=reg;outTypeR=typ} 				-> reg ^ " ? " ^ type_to_string typ   
	| SndReg {reg1=r1;reg2=r2} 							-> r1 ^ " ! "^ r2
	| RecLab {regL=r;label=lab} 						-> r ^" ? " ^ lab  
	| SndChc {regCa=reg;labl=lab} 						-> reg ^" ! "^ lab 
	| RecChoice {regCb=reg ; cList= lst}			    -> reg ^ " ? optn [" ^ p lst^ "]"
	|  _ 												-> "\nerr\n " 

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

let merge_hash t1 t2 =
	let add2t1 x y = Hash.add t1 x y in 
	Hash.iter (add2t1) t2;
	t1
;;

(* merge tuples of lists cannot have more than one label*)
let rec mergeList l =
	match l with 
	| x::[] 	-> x
	| (lab,tbl1)::(lab2,tbl2)::l 	-> (match lab with 
										| Some s 	-> mergeList ((lab,(merge_hash tbl1 tbl2))::l)
										| _			-> mergeList ((lab2,(merge_hash tbl1 tbl2))::l))
;;

(* add new region constraint to list *)
(* regList is a list of tupples of labels and hash tables consisting of regions.
the hash tables are searched for the region that is passed in. 
if regLab is a region the hash tables are searched for this region
when a match is found (in the case of two regions) the other region is added to the hash table
in the case where seperate matches are found for the regions the hash table of the second match is added to the first hash table and the label updated appropriately 
if regLab is a label then the first of the tupple that returns a match for the region is set to be that label
  *)
let add_list a b regList =
	(* find elements of the list with region a in hash table *)
	let aList = (List.filter regList (fun (lab,table) -> (Hash.mem table a) ) ) in 
	match b with 
	| Label s 	-> (let newItem = ((Some s), (Hash.create ~random:false 10)) in (*create new element with empty hash *)
					let returnList = List.filter regList (fun x -> (not (List.mem aList x )))  in
					let toMerge = aList @[newItem] in 
					let newList = mergeList  toMerge in 
					(List.append returnList [newList]))
	| RVar s 	-> 	(*create new list that is elements found appended to elements that have the second region in hash table*)
					(let fullList = (List.append (List.filter regList (fun (lab,table) -> (Hash.mem table s) ) ) aList) in
						(* filter the original list to only contain elements that are not in the full list *)
					let newList = List.filter regList (fun x -> not (List.mem fullList x))  in 
					(* tidy new list to either add the element with the two regions if no elements found with the regions or to  *)
					let newHash = (Hash.create ~random:false 10) in
					Hash.add newHash s (); 
					Hash.add newHash a ();
					(match newList with 
					| [] 	-> ( List.append (regList) [(None, newHash)])
					| _ 	-> (let finalList = List.append fullList [(None, newHash)] in
									let newItem = mergeList fullList in
										List.append [newItem] newList))
					)
;;

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
let rec con_add con bHshtbl rList =
	match con with 
	| TCon {smlT=a;bigT=b} 				-> (bHshtbl,rList)
	| BCon {smlB=a;bigB=b}				-> (Hash.add bHshtbl b a) ; (bHshtbl,rList)
	| RegRel {reg=a; regLab=b} 			-> (bHshtbl, (add_list a b rList))  
	| ConRel {chnlA=a ; endptA=b}		-> (bHshtbl,rList)
	| ConRelAlt {chnlB=a ; endptB=b}	-> (bHshtbl,rList)
	| ConSeq {con1=a; con2=b}			-> let (newb,newr) = (con_add a bHshtbl rList) in con_add b newb newr;
	| None 								-> (bHshtbl,rList)
(* 	| TCon {smlT=a;bigT=b} 				-> Hashtbl.add hshtbl ("T"^(type_to_string a)) (type_to_string b)
	| BCon {smlB=a;bigB=b}				-> Hashtbl.add hshtbl ("B"^b) (behaviour_to_string a) 
	| RegRel {reg=a;regLab=b} 			-> Hashtbl.add hshtbl ("R"^a) (region_to_string b)   
	| ConRel {chnlA=a ; endptA=b}		-> Hashtbl.add hshtbl ("C"^a) (sess_to_string b) 
	| ConRelAlt {chnlB=a ; endptB=b}	-> Hashtbl.add hshtbl ("C'"^a) (sess_to_string b) 
	| ConSeq {con1=a; con2=b}			-> add_seq a b hshtbl
	| None 								-> Hashtbl.add hshtbl "none" "" *)
;;

(* compare two strings true if same false if not *)
let comp_string s1 s2 = 
	match (compare s1 s2) with 
	| 0 	-> true
	| _ 	-> false
;;

(* ses1 <: ses2 *)
(* TODO *)
let check_sess_rel ses1 ses2 = true
;;

(* let update_stack stack newSess lab =
	Stack.pop stack;
	Stack.push stack {label=lab;sessType=newSess};
	true
;; *)

(* check if t1 <: t2  *)
(* TODO *)
let check_type_rel t1 t2 = true
;;



(* see if labels match in stack frames *)
let match_label {label=a1;sessType=b1} {label=a2;sessType=b2}=
	let check = compare a1 a2 in
	match check with 
	| 0 -> true
	| _ -> false
;;

(* itterate through the stack checking each frame for the label of this frame if not found push & teturn true, if found dont & return false*)
let push_stack_frame stack frame =
	match (Stack.exists stack (match_label frame)) with 
	| false 	-> Stack.push stack frame; true
	| _			-> false
;;

(* check b < Beta	 *)
(* TODO *)
let const_check_b beta = 
	match beta with 
	|_ -> true
;;

(* check constraints in hash table  *)
(* TODO only checking first binding *)
let check_reg_const lhs {reg=a1;regLab=b1} hash =
	(* Hashtbl.existsi hash ~key:lhs ~data:({reg=a1;regLab=b1}) *)
	match Hashtbl.find hash lhs with 
	| Some RegRel {reg=a;regLab=b} 			->  
		(match b with 
		| Label l 	-> 
			(match b1 with 
			| Label l1 ->
				(match compare l l1 with 
				| 0 -> true
				| _ -> false)
			| _ 	-> false
			)
		| _ -> false
		)
	| _										-> false
;;

(* check if constraints allow region to label and T1 < T2 *)
let const_check_t_out typ reg stack hash =
	let frame = Stack.pop stack in
	match frame with 
	| Some {label=lab;sessType=sess} 	-> 
		(
		match sess with 
		| OutputConfinded {outValue=typ';sTypeOut=styp}	-> 
			(
			match check_reg_const ("R"^reg) {reg=reg;regLab=(Label lab)} hash with 
			| true 	-> 	
				(
				match check_type_rel typ typ' with
				| true		-> Stack.push stack {label=lab;sessType=styp}; true
				| _ 		-> false
				)
			| _        -> false
			)
		| _ 		-> false
		)
	| None 		-> false
;;

(* check if constraints allow region to label and T1 < T2 *)
let const_check_t_in typ reg stack hash =
	let frame = Stack.pop stack in
	match frame with 
	| Some {label=lab;sessType=sess} 	-> 
		(
		match sess with 
		| InputConfinded {inValue=typ';sTypeIn=styp}	-> 
			(
			match check_reg_const ("R"^reg) {reg=reg;regLab=(Label lab)} hash with 
			| true 	-> 	
				(
				match check_type_rel typ' typ with
				| true		-> Stack.push stack {label=lab;sessType=styp}; true
				| _ 		-> false
				)
			| _        -> false
			)
		| _ 		-> false
		)
	| None 		-> false
;;

let res_check reg regLab stack hash =
	let frame = Stack.pop stack in 
	match frame with 
	| Some {label=l;sessType=n} ->
		(match n with 
		| Resumption {sTypeR=n1;sTypeR2=n2} -> 
			(match not((comp_string l regLab) && (check_reg_const ("R"^reg) {reg=reg;regLab=(Label l)} hash)) with 
			| true 	-> Stack.push stack {label=regLab;sessType=n1}; Stack.push stack {label=l;sessType=n2}; true
			| _ 	-> false)
		| _ -> false )
	| _ 	-> false
;;


(* check that regions and labels link, also that session types have correct relationship	 *)
let del_check r1 r2 stack hash= 
	let top_frame = Stack.pop stack in 
	match top_frame with 
	| Some {label=lab_top;sessType=sess_top} ->
		(let frame_2 = Stack.pop stack in 
		match frame_2 with 
		| Some {label=lab_2;sessType=sess_2} ->
			(let allowed = (check_reg_const ("R"^r1) {reg=r1;regLab=(Label lab_top)} hash) && (check_reg_const ("R"^r2) {reg=r2;regLab=(Label lab_2)} hash) in
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
				| _ 	-> false )
		| _ 	-> false
		)
	| _ 	-> false
;;

(* http://stackoverflow.com/questions/4473163/match-one-item-in-list-of-tuples *)
let rec find_tuple label tuples_list =
       match tuples_list with
            [] -> raise Not_found
            |(s, i)::tl -> if s = label then (s, i) 
                                  else find_tuple label tl

let ich_check sReg sLab stack hash =
	let top_frame = Stack.pop stack in 
	match top_frame with 
	| Some {label=lab_top;sessType=chctype} -> 
		(match chctype with 
		| ChoiceS {opList=iList; sent=isel} -> 
			(match find_tuple sLab iList with 
			| (lab,ses) -> 
				(match (check_reg_const ("R"^sReg) {reg=sReg;regLab=(Label sLab)} hash) with 
				|true 	-> (Stack.push stack {label=lab_top;sessType=ses}); true
				| _ 	-> false
				)
			| _ 		-> false (* TODO warning says this is unused but how come since something other than a tuple could be returned*)
			)
		| _ 	-> false)
	| _ 	-> false
;;



(* function for checking behaviours  *)
let rec check_behav behaviour stack hash=
	(* end *)
	let stack_top = Stack.top stack in 
	match stack_top with 
	| Some {label=lab_top;sessType=EndTag} -> Stack.pop stack; check_behav behaviour stack hash
	| _ 	-> 
	(match behaviour with 
			(* Beta *)
			| BVar s 									-> const_check_b s  (* TODO need to fix this since will not call again on b*)
			(* plus *)
		 	| ChoiceB {opt1=op1;opt2=op2} 				-> (check_choice op1 stack hash) && (check_choice op2 stack hash)
			(* push *)
		 	| Push {toPush=a} 							-> push_stack_frame stack a
			(* out *)
			| SndType {regionS=reg;outTypeS=typ} 		-> (const_check_t_out typ reg stack hash)
			(* in *)
			| RecType {regionR=reg;outTypeR=typ} 		-> (const_check_t_in typ reg stack hash)
			(* del *) 
			| SndReg {reg1=r1;reg2=r2} 					-> del_check r1 r2 stack hash
			(* res *)
			| RecLab {regL=r;label=lab} 				-> res_check r lab stack hash
			(* ICh *) 
			| SndChc {regCa=reg;labl=lab} 				-> ich_check reg lab stack hash
			(* ECh *)
			| RecChoice {regCb=reg ; cList= lst}     	-> ech_check reg lst stack hash
			(* rec *)
		(* 	| RecB {behaVar=beta;behaviour=b} 			-> let newStack = (Stack.create ()) in 
														 	let newHash = Hashtbl.copy hash in
														 	newHash.replace ("B"^beta) {behaVar=beta;behaviour=Tau};
														 	check_bahav b newStack newHash *)
			(* spn *)
			| Spawn {spawned=b} 						-> let newStack = (Stack.create ())in (check_behav b newStack hash)
			(* seq *)
		 	| Seq {b1=b_1;b2=b_2} 						-> (check_behav b_1 stack hash) && (check_behav b_2 stack hash) 
			(* tau *)
			| Tau  										-> true; (*TODO is this right? will move it forward in seq*)
	)
 
(* call main function with a copy of stack *)
and check_choice op1 stack hash = 
	let s1 = Stack.copy stack in
	check_behav op1 s1 hash

(* ExtChoicS { opList1 : (string * sesType) list ; opList2 : (string * sesType) list} *)

and ech_check reg lst index stack hash =
	(* check top of stack right *)
	let top_frame = Stack.pop stack in 
	match top_frame with 
	| Some {label=lab_top;sessType=chctype} -> 
		(match chctype with 
		| ExtChoicS { opList1=list1; opList2=list2} ->  
			(* check all possible J where J is member of lst (string*b) no... wrong i think*)
			(* find the label from indexed element of list then find corresponding label in stack frame lists *)
			(match find_tuple ( fst (List.nth_exn lst (int_of_string index)))  (List.append list1 list2)  with
			| (lab,sess)	-> 
				(* check constraints *)
				(match (check_reg_const ("R"^reg) {reg=reg;regLab=(Label lab_top)} hash) with 
				|true 	-> (Stack.push stack {label=lab_top;sessType=sess}); true
				| _ 	-> false 
				)
			| _		-> false) (*TODO same as above*)
		| _ 	-> false)
	| _ 	-> false

(* check if behaviour is allowed for all behaviours in the list *)
(* and check_list_b lst hash stack =
	match lst with 
	| [] 		-> true
	| (x,y)::l 	-> (check_behav y stack hash) && (check_list_b l hash stack) *)
;;

(* second attempt at behaviour checking *)

(* print out behaviours *)
let output_b outc input =  output_string outc (behaviour_to_string input);;
(*print out constraints*)
let output_con outc input =  output_string outc (con_to_string input);;

(* currently can only call one of these at a time from main, need to fix *)
module Hash = Hashtbl
open Core_kernel.Std

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
and choiceS = { opList : (string * sesType) list}
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
	| InputConfinded {inValue=a; sTypeIn=b}-> "? " ^ type_to_string a ^ " "^(sess_to_string b)
	| OutputConfinded {outValue=a; sTypeOut=b}-> "! " ^ type_to_string a ^  " "^(sess_to_string b)
	| Delegation {sTypeD=b ; sTypeD2=c} -> "! " ^ (sess_to_string b) ^ " " ^ (sess_to_string c)
	| Resumption {sTypeR=b ; sTypeR2=c} -> "? " ^ (sess_to_string b) ^ " " ^ (sess_to_string c)
	| ChoiceS {opList=a} ->  "(+)[" ^ f a ^ "] "
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
	| BCon {smlB=a;bigB=b}				-> "Start behaviour: \n" ^ behaviour_to_string a ^ "\nend behaviour \n" ^ " < " ^ b ^ " "
	| RegRel {reg=a;regLab=b} 			-> a ^ " ~ " ^ region_to_string b ^ " "    
	| ConRel {chnlA=a ; endptA=b}		-> a ^ " ~ " ^ sess_to_string b ^" "
	| ConRelAlt {chnlB=a ; endptB=b}	-> a ^ " ~ " ^ sess_to_string b ^" "
	| ConSeq {con1=a; con2=b}			-> con_to_string a ^",\n "^con_to_string b  
	| None 								-> "empty"
;;


(* list from hash table to string behaviours *)
let rec hsh_lst_to_string hshlst =
	match hshlst with
	| [] -> ""
	| (k,v)::l -> "\nPaired: \nBeta: " ^ (k) ^ " Behaviour: \n" ^ (behaviour_to_string v) ^ hsh_lst_to_string l

;;(* list from hash table to string types *)
let rec thsh_lst_to_string hshlst =
	match hshlst with
	| [] -> ""
	| (k,v)::l -> "\nPaired: \nSuper: " ^ (type_to_string k) ^ " sub: \n" ^ (type_to_string v) ^ thsh_lst_to_string l
;;

(* behaviour constraint to string*)
let behav_con_to_string bHash =
	let hshLst = Hash.fold (fun k v acc -> (k, v) :: acc) bHash [] in 
	"Behaviour constraints:\n"^hsh_lst_to_string hshLst
;;

(* list from region constraint to string *)
let rec reg_lst_to_str hshLst = 
	match hshLst with
	| [] -> ""
	| x::xs -> x ^ "\n" ^ (reg_lst_to_str xs)
;;

(* region hash table to string *)
let reg_hs_to_string rhsh =
	let hshLst = Hash.fold (fun k v acc -> k :: acc) rhsh [] in 
	reg_lst_to_str hshLst
;;

(* region constraint to string *)
let rec reg_con_to_string rlst = 
	match rlst with
	| [] -> ""
	| ((Some l),h)::ls -> "label: " ^ l ^ "\n regions:\n"^(reg_hs_to_string h) ^ (reg_con_to_string ls)
	| (None, h)::ls -> "label: None "  ^ "\n regions:\n"^(reg_hs_to_string h) ^ (reg_con_to_string ls)
;;

let type_con_to_string tHash =
	let hshLst = Hash.fold (fun k v acc -> (k, v) :: acc) tHash [] in 
	thsh_lst_to_string hshLst
;;

(* constraint set to string *)
let con_set_to_string (bHash,rLst,tHash) = 
	(behav_con_to_string bHash) ^ "\nRegion Constraints:\n " ^(reg_con_to_string rLst) ^ "\nType Constraints:\n " ^ (type_con_to_string tHash)
;;

(******************************************** TYPE CHECKING *******************************************************)


(* *****************************************constraint storage***************************************************** *)
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
	let add2t1 x y = (if not(Hash.mem t1 x) then Hash.add t1 x y) in 
	Hash.iter (add2t1) t2;
	t1
;;

(* merge tuples of lists cannot have more than one label*)
let rec mergeList l =
	match l with 
	| x::[] 	-> [x]
	| (lab,tbl1)::(lab2,tbl2)::ls 	-> (match lab with 
										| Some s 	-> mergeList ((lab,(merge_hash tbl1 tbl2))::ls)
										| _			-> mergeList ((lab2,(merge_hash tbl1 tbl2))::ls))
	| [] 	-> []
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
	| Label s 	-> (match List.filter regList (fun (lab,table) -> (lab = (Some s))) with 
					| [] 	->  (let newHash = Hash.create ~random:false 10 in
									Hash.add newHash a ();
									let toMerge = [(Some s, newHash)]@aList in
										let returnList = List.filter regList (fun x -> not(List.mem aList x)) in
											(mergeList toMerge)@returnList
								)
					| (l,t)::[] 	-> (Hash.add t a ();
										let toMerge = aList@[(l,t)] in
											let returnList = List.filter regList (fun x -> not(List.mem toMerge x)) in
												(mergeList toMerge)@returnList
								  		)
					| _		-> regList
					)

	| RVar s 	-> 	(* create new list that is elements found appended to elements that have the second region in hash table *)
					(let fullList = (List.append (List.filter regList (fun (lab,table) -> (Hash.mem table s) ) ) aList) in
						(* filter the original list to only contain elements that are not in the full list *)
						let newList = List.filter regList (fun x -> not (List.mem fullList x))  in 
							(* tidy new list to either add the element with the two regions if no elements found with the regions or to  *)
							let newHash = (Hash.create ~random:false 10) in
								Hash.add newHash s (); 
								Hash.add newHash a ();
								let finalList = List.append fullList [(None, newHash)] in
									let newItem = mergeList finalList in
										List.append newItem newList 
					)
;;

(* constraints added. Currently only concerned with type, behaviour and region constraints. behaviour constraints added to a hashtable region to a list of label*hashtable tupples. *)
let rec con_add con bHshtbl rList tHshtbl=
	match con with 
	| TCon {smlT=a;bigT=b} 				-> Hash.add tHshtbl b a;(bHshtbl,rList,tHshtbl)
	| BCon {smlB=a;bigB=b}				-> (Hash.add bHshtbl b a) ; (bHshtbl,rList,tHshtbl)
	| RegRel {reg=a; regLab=b} 			-> (bHshtbl, (add_list a b rList),tHshtbl) 
	| ConRel {chnlA=a ; endptA=b}		-> (bHshtbl,rList,tHshtbl)
	| ConRelAlt {chnlB=a ; endptB=b}	-> (bHshtbl,rList,tHshtbl) 
	| ConSeq {con1=a; con2=b}			-> let (newb,newr, newt) = (con_add a bHshtbl rList tHshtbl) in con_add b newb newr newt;
	| None 								-> (bHshtbl,rList,tHshtbl)
;;


(* ********************************************behaviour checker ************************************************** *)
(* and functions used for it  *)

(* takes list of tuples and returns list of first elemnets *)
let rec get_lab_lst lst =
	match lst with 
	| [] 	-> []
	| (l,x)::xs 	-> [l] @ (get_lab_lst xs)
;;

(* checks if every label from every element of sub is in a tuple in par *)
let check_subset sub par = 
	let lab_sub = get_lab_lst sub in
	let lab_par = get_lab_lst par in
	List.for_all lab_sub (fun x -> List.mem lab_par x)
;;

(* remove x binding for key from hash table *)
let rec remove_all hash key valCount = 
	match valCount with 
	| 0 	-> hash
	| _		-> Hash.remove hash key;
				remove_all hash key (valCount-1)
;;

(* add a list of bindings for a key to the hash table *)
let rec add_all hash key valList = 
	match valList with 
	| [] 	-> hash 
	| b::l 	-> Hash.add hash key b;
				add_all hash key l 
;;

(* find a tuple from a list where the first parameter matches label. returns found tuple or raises exception *)
let rec find_tuple label searchList =
	match searchList with 
	| [] 	-> raise Not_found
	| (Some l,h)::lst 	-> if l = label then (l, h) 
                                  else find_tuple label lst
	| _	-> raise Not_found
;;

(* find a tuple from a list where the first parameter matches label. returns found tuple or raises exception *)
let rec find_tuple_nop label searchList =
	match searchList with 
	| [] 	-> raise Not_found
	| (l,h)::lst 	-> if l = label then (l, h) 
                                  else find_tuple_nop label lst
;;

(* function for taking two lists of tuples with labels as the first parameter and returning a tuple of the second parameter *)
let rec match_list lst_b lst_n retLst= 
	match lst_b with 
	| [] 	-> retLst
	| (l,b)::lb 	-> match (find_tuple_nop l lst_n) with
						| (ln,n) -> retLst@[(b,n)]					
;;

(* check if label is in list. if yes check if reg is in associated hash table and reutrn true if is. Otherwise return false *)
let check_reg_const reg label regList =
	 (* printf "checking region: %s and label %s \n " (reg) label;  *)
	match (find_tuple label regList) with 
	| (lab, hashT) 	-> (* if (Hash.mem hashT reg) then printf "all good\n" else printf "failed \n"; *) (Hash.mem hashT reg)
	(* | _ 	-> false *)
;;

let rec check_t_con sub super tHash =
	let tList = Hash.find_all tHash super in 
		if List.mem tList sub then true 
			else check_t_each sub tList tHash

and check_t_each sub superList tHash =
	match superList with 
	| [] -> false
	| x::xs -> if (check_t_con sub x tHash) then true 
					else check_t_each sub xs tHash 
;;

(* find all behaviours associated with the subtype then check to see if they are all also associated with the supertype *)
let check_type_b_con sub super bHash =
	let lstSub = Hash.find_all bHash sub in 
	let lstSup = Hash.find_all bHash super in
		List.for_all lstSub (fun x -> List.mem lstSup x)
;; 

let rec check_reg_const_ses r1 r2 rlist = 
	match rlist with 
	| [] -> false
	| (l,hash)::xs -> if (Hash.mem hash r1) && (Hash.mem hash r2) then true else check_reg_const_ses r1 r2 xs
;;

(* type constraint checking function *)
(* sub super typeCon regCon behaviourCon *)
let rec check_types t1 t2 bHash rList tHash= 
	(*  printf "checking types: " ;
	printf "%s" (type_to_string t1);
	printf " and " ;
	printf "%s\n" (type_to_string t2);  *)
	if t1 = t2  (*check for reflexive*)
		then true 
		else 
		(if (check_t_con t1 t2 tHash) then true 
		else match t1 with 
		| Ses reg1 -> (match t2 with 
						| Ses reg2 -> (check_reg_const_ses reg1 reg2 rList)
						| _ -> false)
		| Pair {type1=t1_1 ; type2=t1_2} -> 
					(match t2 with
					| Pair {type1=t2_1 ; type2=t2_2} -> 
						(check_types t1_1 t2_1 bHash rList tHash) 
						&& (check_types t1_2 t2_2 bHash rList tHash)
					| _ -> false) 
		| Funct {inType=t1_1 ; outType=t1_2 ; behav=t1_b} -> 
					(match t2 with 
					| Funct {inType=t2_1 ; outType=t2_2 ; behav=t2_b} -> (*TODO think maybe rule might be mixed up*)
						(check_type_b_con t1_b t2_b bHash)
						&& (check_types t2_1 t1_1 bHash rList tHash) 
						&& (check_types t1_2 t2_2 bHash rList tHash)
					| _ -> false)
		| _ -> false)
;;

let rec check_ses_sub sub parent =
	match sub with 
	| [] -> true
	| x::xs -> (List.mem parent x) && (check_ses_sub xs parent)
;;

(* TODO check... ECH ICH might be wrong *)
(*sub super, check if sub is subsessiontype of super*)
let rec check_sess n1 n2 bHash rList tHash= 
	match n1 with 
	| EndTag -> (match n2 with 
				| EndTag -> true
				| _		-> false)
	| InputConfinded {inValue=n1_t ; sTypeIn= n1_n} -> 
		(match n2 with 
		| InputConfinded {inValue=n2_t ; sTypeIn= n2_n} -> 
				(check_types n2_t n1_t bHash rList tHash)
				&& (check_sess n1_n n2_n bHash rList tHash)
		| _	-> false)
	| OutputConfinded {outValue=n1_t ; sTypeOut= n1_n} ->
		(match n2 with 
		| OutputConfinded {outValue=n2_t ; sTypeOut= n2_n} -> 
				(check_types n1_t n2_t bHash rList tHash)
				&& (check_sess n1_n n2_n bHash rList tHash)
		| _	-> false) 
	| ChoiceS {opList=n1_lst} -> 
		(match n2 with 
		| ChoiceS {opList=n2_lst} -> (((List.length n1_lst) <= (List.length n2_lst)) 
				&& (check_each_ses2 n1_lst n2_lst bHash rList tHash ) )
		| _ -> false)
	| ExtChoicS {opList1=n1_lst1; opList2=n1_lst2} -> 
		(match n2 with 
		| ExtChoicS {opList1=n2_lst1; opList2=n2_lst2} -> 
				(let all_2 = n2_lst1@n2_lst2 in 
				let all_1 = n1_lst1@n1_lst1 in
				(check_ses_sub n1_lst1 n2_lst1) 
				&& (check_ses_sub (all_2) (all_1)) 
				&& (check_each_ses all_2 all_1  bHash rList tHash))
		| _ -> false) 
	| _ -> printf "Session type Delegate, Resume or Variable. These are incorrect\n"; false

and check_each_ses2 n1_lst n2_lst bHash rList tHash=
	let labLst = get_lab_lst n1_lst in 
	check_each_s2 labLst n1_lst n2_lst bHash rList tHash

and check_each_s2 labLst n1_lst n2_lst bHash rList tHash =
	match labLst with 
	| [] -> true
	| x::xs -> (check_sess (snd (find_tuple_nop x n1_lst)) (snd (find_tuple_nop x n2_lst))  bHash rList tHash) && (check_each_s2 xs n1_lst n2_lst bHash rList tHash)

and check_each_ses lst2 lst1  bHash rList tHash= 
	let lLst = get_lab_lst lst2 in
	(check_each_ses1 lLst lst2 lst1 bHash rList tHash)

and check_each_ses1 lab lst2 lst1  bHash rList tHash= 
	match lab with 
	| [] -> true
	| x::xs -> (check_sess (snd (find_tuple_nop x lst2)) (snd (find_tuple_nop x lst1))  bHash rList tHash) && (check_each_ses1 xs lst2 lst1 bHash rList tHash) 
;;

(* testing functions for printing call parameters *)
let rec print_list l =
	match l with 
	| [] 	-> printf "";
	| x::xs -> printf "(%s) " (behaviour_to_string x)
;;

let print_frame frame =
	match frame with  
	| Some {label=l;sessType=s} -> (printf "label: %s " l;
										let ses_str = (sess_to_string s) in 
										printf "sessType: %s\n" ses_str; )
	| None -> printf  "Empty stack\n"
;;

let rec print_stack stack = 
	let printStack = Stack.copy stack in
	match Stack.is_empty stack with 
	| false -> print_frame (Stack.pop printStack); print_stack printStack
	| true -> printf "Stack end\n";
;;

(* wrapper function *)
let rec checker behavList conSet= 
	(* printf "\ncall to checker "; *)
	match behavList with
	| [] 	-> true
	| x::l 	-> (check_step x conSet) && (checker l conSet)

(* step function *)
and check_step (behaviour, stack, slabs, continuation ) conSet = 
	(* check if stack is empty *)
	(* printf "\ncall to check_step \n Stack:";
	print_stack stack;
	let bString = behaviour_to_string behaviour in
	printf "Behaviour: %s\n" bString; *)
	let topFrame = Stack.pop stack in
	(match topFrame with 
	| Some {label=lab_top;sessType=EndTag} ->  check_step (behaviour, stack, slabs, continuation ) conSet (*pop frame and continue if end*)
	| Some stackframe	->  (Stack.push stack stackframe);check_rules (behaviour, stack, slabs, continuation ) conSet (*otherwise check if other rules apply*)
	| _					-> check_rules (behaviour, stack, slabs, continuation ) conSet)

and check_rules (behaviour, stack, slabs, continuation ) (bHash,rList,tHash) =
 	(* printf "\ncall to check_rules \n Stack:";
	print_stack stack; *) 
	match behaviour with 
	(* Beta *)
	| BVar beta 								-> (match Hash.mem bHash beta with 
													| true -> let bList = Hash.find_all bHash beta in (*get list of all behaviours with links to beta in constraints*)
														(*call check step on each behaviour in list with a copy of the stack*)
														(* print_list bList; *)
														check_each bList stack slabs continuation (bHash,rList,tHash)
													| _ -> true)
													
	(* plus *)
 	| ChoiceB {opt1=op1;opt2=op2} 				-> let choiceList = [op1]@[op2] in
 														(*check each choice with copy of stack*)
 														check_each choiceList  stack slabs continuation (bHash,rList,tHash)
	(* push *)
 	| Push {toPush=frame} 						-> (*check that label hasn't already been on stack, if not push frame*)
 														check_push frame stack slabs continuation (bHash,rList,tHash) 
	(* out *)
	| SndType send						 		-> (*check top stackframe correct type, check reg constraints, check type stuff*)
														check_out send stack slabs continuation (bHash, rList, tHash)
	(* in *)
	| RecType reci						 		-> (*same as out but different top of stack & reverse of type check*)
														check_in reci stack slabs continuation (bHash, rList, tHash)
	(* del *) 
	| SndReg  del              					-> check_del del stack slabs continuation (bHash, rList, tHash)
	(*res*)
	| RecLab res				 				-> check_res res stack slabs continuation (bHash, rList, tHash)
	(* ICh *) 
	| SndChc ich				 				-> check_ich ich stack slabs continuation (bHash, rList, tHash)
	(* ECh *)
	| RecChoice ech 					     	-> check_ech ech stack slabs continuation (bHash, rList, tHash)
	(* rec *)
	| RecB 	recurs					 			-> check_rec recurs stack slabs continuation (bHash, rList, tHash)
	(* spn *)
	| Spawn {spawned=b} 						-> let newStack = Stack.create () in
													let newSlabs = Stack.create () in
													(check_step (Tau, stack, slabs, continuation) (bHash,rList,tHash)) 
													&& (checker [(b, newStack, newSlabs, [])] (bHash, rList, tHash)) 
													(*TODO check this is right. should it be new stacks?*) 
	(* seq *)
 	| Seq {b1=b_1;b2=b_2} 						-> let newContinue = [b_2]@continuation in 
 													check_step (b_1, stack, slabs, newContinue) (bHash, rList, tHash)
	(* tau *)
	| Tau  										-> check_tau stack slabs continuation (bHash, rList, tHash)
	| None										-> printf "Empty constraint error\n";false

(* check each behaviour in list using copy of current stack *)
and check_each bList stack slabs continuation conSet =
	let newCont = continuation in (*TODO not sure this is really copying*)
	match bList with 
	| [] -> true
	| b::l 	-> (check_step (b, (Stack.copy stack), (Stack.copy slabs), newCont) conSet) 
				&& (check_each l stack slabs continuation conSet)

(* check if passed in frame label matches any label that has been on stack before. If not push frame and continue check. If yes return false *)
and check_push {label=lab;sessType=sTyp} stack slabs continuation conSet =
	match (Stack.mem slabs lab) with 
	| true 	-> printf "push rule\nlabel %s has already been pushed to stack\n" lab;false
	| false -> (Stack.push stack {label=lab;sessType=sTyp});
				(Stack.push slabs lab); 
				(* printf "before call to check_step stack is:\n";
				print_stack stack; *)
				(check_step (Tau, stack, slabs, continuation) conSet) 

(* check if top of stack correct, constraints ok and types ok. If yes continue if no return false *)
and check_out {regionS=reg;outTypeS=typ} stack slabs continuation (bHash, rList, tHash) = 
	match (Stack.pop stack) with 
	| Some {label=lab; sessType=(OutputConfinded {outValue=typSt;sTypeOut=styp})}	-> 
			(match ((check_reg_const reg lab rList) && (check_types typ typSt bHash rList tHash) ) with 
			| true 	-> (Stack.push stack {label=lab;sessType=styp}) ;
						check_step (Tau, stack, slabs, continuation) (bHash, rList, tHash)
			| _ 	-> printf "out rule\nregion constraints failed check\n"; false)
	| _		-> printf "out rule\nstack frame incorrect for current behaviour\n";false

and check_in {regionR=reg;outTypeR=typ} stack slabs continuation (bHash, rList, tHash) = 
	match (Stack.pop stack) with 
	| Some {label=lab; sessType=(InputConfinded {inValue=typSt;sTypeIn=styp})}	-> 
			(match (  (check_reg_const reg lab rList)&& (check_types typSt typ bHash rList tHash)) with 
			| true 	->  (Stack.push stack {label=lab;sessType=styp}) ;
						check_step (Tau, stack, slabs, continuation) (bHash, rList, tHash)
			| _ 	-> printf "in rule\nregion constraints or functional type constraints failed check\n";false)
	| _		-> printf "in rule\nstack frame incorrect for current behaviour\n";false

and check_del {reg1=r1;reg2=r2} stack slabs continuation (bHash, rList, tHash) = 
	match (Stack.pop stack ) with 
	| Some {label=lab; sessType=(Delegation {sTypeD=s1;sTypeD2=s2})} -> 
						(match Stack.pop stack with 
						| Some {label=lab2;sessType=sessT2} -> 
									(match ((check_reg_const r1 lab rList) && (check_reg_const r2 lab2 rList) && (check_sess s1 sessT2 bHash rList tHash)) with 
									| true 	-> (Stack.push stack {label=lab; sessType=s2});
												check_step (Tau, stack, slabs, continuation) (bHash, rList, tHash)
									| _	-> printf "del rule\nregion constraints or session constraints failed check\n";false ) 
						| _ 	-> false)
	| _		-> printf "del rule\nstack frame incorrect for current behaviour\n";false

and check_res {regL=r;label=lab} stack slabs continuation (bHash, rList, tHash) =
	match Stack.pop stack with 
	| Some {label=labSt; sessType=(Resumption {sTypeR=s1;sTypeR2=s2})} -> 
				print_stack stack;
				if Stack.is_empty stack then ( 
					(match ((( lab <> labSt)) && (check_reg_const r labSt rList)) with 
					| true 	-> (Stack.push stack {label=lab;sessType=s1});
								(Stack.push stack {label=labSt ;sessType=s2});
								check_step (Tau, stack, slabs, continuation) (bHash, rList, tHash)
					| _ 	->printf "resume rule\nregion constraints failed check or incorrect label \n"; false)) 
				else (printf "resume rule\n stack must be one frame \n"; false)
	| _		-> printf "resume rule\nstack frame incorrect for current behaviour\n";false


and check_tau stack slabs continuation conSet =
	match continuation with 
	| [] 	-> (Stack.is_empty stack)
	| b::cont 	-> check_step (b, stack, slabs, cont) conSet

and check_ich {regCa=reg;labl=lab} stack slabs continuation (bHash, rList, tHash) = 
	match Stack.pop stack with 
	| Some {label=labSt; sessType=(ChoiceS {opList=opL})} -> 
								(try((match (find_tuple_nop lab opL) with 
								| (labF, sessF) -> (match check_reg_const reg labSt rList with 
													| true 	-> (Stack.push stack {label=labSt;sessType=sessF});
																check_step (Tau, stack, slabs, continuation) (bHash, rList, tHash)
													| _		->printf "ich rule\nregion constraints failed check\n"; false)
								(*| _ 	-> false*)	)) with Not_found -> printf "ich rule \nLabel Absent from Stack\n"; false)
	| _		-> printf "ich rule\nstack frame incorrect for current behaviour\n";false

(* for each b in cList check that there is a  *)
and check_ech {regCb=reg; cList=lst} stack slabs continuation (bHash, rList, tHash) = 
	(* check top of stack *)
	match Stack.pop stack with 
	| Some {label=labSt; sessType= (ExtChoicS{opList1 = ol1;opList2=ol2})} -> 
						(match (check_reg_const reg labSt rList) && (check_subset ol1 lst) && (check_subset lst (ol1@ol2)) with 
						| true 	-> let newLst=(match_list lst ol1 []) in
									check_each_ses labSt newLst stack slabs continuation (bHash, rList, tHash)
						| _		-> printf "ech rule\nregion constraints failed check or lists structured incorrectly\n";false) 
						
	| _		-> printf "ech rule\nstack frame incorrect for current behaviour\n";false

and check_each_ses lab lst stack slabs continuation conSet = 
	match lst with
	| [] 	-> true
	| (b,n)::l 	-> let newStack = Stack.copy stack in 
					let newSlabs = Stack.copy slabs in
					Stack.push newStack {label=lab;sessType=n};
					(check_step (b, newStack, newSlabs, continuation) conSet ) && (check_each_ses lab l stack slabs continuation conSet)

and check_rec {behaVar=beta;behaviour=b} stack slabs continuation (bHash, rList, tHash) = 
	let bList = Hash.find_all bHash beta in
	match List.exists bList (fun x ->  x = (RecB { behaVar=beta ; behaviour=b} )) with 
	| true 	-> let newbList = List.filter bList  (fun x ->( x <> (RecB { behaVar=beta ; behaviour=b} ))) in
				let newbHash = Hash.copy bHash in 
				let newStack = Stack.create () in
				let newSlabs = Stack.create () in
				let hashN = remove_all newbHash beta (List.length bList) in
				let hashN1 = add_all hashN beta newbList in
				(check_step (Tau, stack, slabs, continuation) (bHash, rList, tHash)) 
				&& (checker [(b, newStack, newSlabs, continuation)] (hashN1,rList,tHash)) 
	| _		-> printf "rec ruls \nrecursion error\n";false
;;

(* *****************************************************output functions*************************************************************** *)
(* print out behaviours *)
let output_b outc input =  output_string outc (behaviour_to_string input);;
(*print out constraints*)
let output_con outc input =  output_string outc (con_to_string input);;

let out_con_set outc input = output_string outc (con_set_to_string input);;

(* currently can only call one of these at a time from main, need to fix *)
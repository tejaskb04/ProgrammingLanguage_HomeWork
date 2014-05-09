(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s : string, sl : string list) =
    case sl of
	[] => NONE 
      | x :: xs => if same_string(s, x)
		   then SOME xs 
		   else case all_except_option(s, xs) of
			  NONE => NONE
			| SOME l => SOME(x :: l)
					
fun get_substitutions1(substitutions : string list list, s : string) =
    case substitutions of
	[] => [] 
      | x :: xs => case all_except_option(s, x) of
			    NONE => get_substitutions1(xs, s)
			  | SOME l => l @ get_substitutions1(xs, s)

fun get_substitutions2(substitutions : string list list, s : string) =
    let fun aux(substitutions, s, acc) =
	    case substitutions of
		[] => acc
	      | x :: xs => case all_except_option(s, x) of
				    NONE => aux(xs, s, acc) 
				  | SOME l => aux(xs, s, l @ acc)
    in
	aux(substitutions, s, [])
    end

fun similar_names(substitutions : string list list, {first = f : string, middle = m : string, last = l : string}) =
    let fun aux(lst, acc) =
	    case lst of
		[] => {first = f, middle = m, last = l} :: acc 
	      | x :: xs => aux(xs, {first = x, middle = m, last = l} :: acc)
    in
	aux(get_substitutions2(substitutions, f), [])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(c : card) =
    let fun aux(s, r) = 
	    case s of
		Clubs => Black 
	      | Diamonds => Red 
	      | Hearts => Red
	      | Spades => Black
    in
	aux(c)
    end

fun card_value(c : card) =
    let fun aux(s, r) =
	    case r of
		Num i => i 
	      | Ace => 11 
	      | _ => 10
    in
	aux(c)
    end

fun remove_card(cs : card list, c : card, e : exn) =
    case cs of
	[] => raise e 
      | x :: xs => if x = c 
		   then xs 
		   else x :: remove_card(xs, c, e)

fun all_same_color(cs : card list) =
    case cs of
	[] => true 
      | _ :: [] => true 
      | x1 :: (x2 :: xs) => if card_color(x1) = card_color(x2) 
			    then all_same_color(x2 :: xs)
			    else false

fun sum_cards(cs : card list) =
    let fun aux(lst, acc) =
	    case lst of
		[] => acc 
	      | x :: xs => aux(xs, acc + card_value(x))
    in
	aux(cs, 0)
    end

fun score(hc : card list, goal : int) =
    let val sum = sum_cards(hc)
    in
	if all_same_color(hc)
	then 
	    if sum > goal 
	    then 3 * (sum - goal) div 2
	    else (goal -sum) div 2 
	else
	    if sum > goal
	    then 3 * (sum - goal)
	    else (goal - sum) 
    end

fun officiate(cs : card list, ms : move list, goal : int) =
    let fun aux(cs, ms, hc) =
	    case ms of 
		[] => score(hc, goal) 
	      | x :: xs => case x of
			       Discard c => aux(cs, xs, remove_card(hc, c, IllegalMove)) 
			     | Draw => case cs of
					   [] => score(hc, goal) 
					 | y :: ys => if sum_cards(y :: hc) > goal
						      then score(y :: hc, goal)
						      else aux(ys, xs, y :: hc)
    in
	aux(cs, ms, [])
    end



(*=======================Challenge Problems===============================*)
fun score_challenge(hc : card list, goal : int) = (*Bad Style! But I don't have much time optimizing it!*)
    let fun sum_and_count(hc, acc, aceno) =
	    case hc of
		[] => if acc > goal
		      then (acc, aceno, 3 * (acc - goal))
		      else (acc, aceno, (goal - acc)) 
	      | x :: xs => case x of
			       (_, Num i) => sum_and_count(xs, acc + i, aceno) 
			     | (_, Ace) => sum_and_count(xs, acc + 11, aceno + 1) 
			     | (_, _) => sum_and_count(xs, acc + 10, aceno)
	val isSameColor = all_same_color(hc)
    in
	case sum_and_count(hc, 0, 0) of
	    (_, 0, y3) => if isSameColor
			  then y3 div 2
			  else y3
	  | (y1, y2, y3) => if y1 > goal
			    then 
				if (y1 - goal) div 10 >= y2
				then 
				    if isSameColor
				    then (y3 - 30 * y2) div 2
				    else (y3 - 30 * y2)
				else
				    if (3 * (y1 - 10 * ((y1 - goal) div 10) - goal)) >= (goal - (y1 - 10 * ((y1 - goal) div 10 + 1)))
				    then 
					if isSameColor
					then (goal - (y1 - 10 * ((y1 - goal) div 10 + 1))) div 2
					else (goal - (y1 - 10 * ((y1 - goal) div 10 + 1)))
				    else
					if isSameColor
					then (3 * (y1 - 10 * ((y1 - goal) div 10) - goal)) div 2
					else (3 * (y1 - 10 * ((y1 - goal) div 10) - goal))
			    else
				if isSameColor
				then y3 div 2
				else y3
    end

fun isSumExceed(hc : card list, goal : int) =
    let fun aux(hc, acc) =
	    case hc of
		[] => acc 
	      | x :: xs => case x of
			       (_, Num i) => aux(xs, acc + i) 
			     | (_, Ace) => aux(xs, acc + 1) 
			     | (_, _) => aux(xs, acc + 10)
    in
	aux(hc, 0) > goal
    end

fun officiate_challenge(cs : card list, ms : move list, goal : int) =
    let fun aux(cs, ms, hc) =
	    case ms of 
		[] => score_challenge(hc, goal) 
	      | x :: xs => case x of
			       Discard c => aux(cs, xs, remove_card(hc, c, IllegalMove))
			     | Draw => case cs of
					   [] => score_challenge(hc, goal) 
					 | y :: ys => if isSumExceed(y :: hc, goal)
						      then score_challenge(y :: hc, goal)
						      else aux(ys, xs, y :: hc)
    in
	aux(cs, ms, [])
    end

fun discardDrawOption(hc : card list, goal) =
    let fun aux(hc, goal) =
	    case hc of
		[] => NONE 
	      | x :: [] => NONE
	      | x1 ::(x2 :: xs) => if (score_challenge(x1 :: remove_card(hc, x2, IllegalMove), goal) = 0)
			   then SOME x2
			   else aux(x1 :: xs, goal)
    in
	aux(hc, goal)
    end

fun card_value_challenge(c : card) =
    let fun aux(s, r) =
	    case r of
		Num i => i 
	      | Ace => 1
	      | _ => 10
    in
	aux(c)
    end

fun sum_cards_challenge(cs : card list) =
    let fun aux(lst, acc) =
	    case lst of
		[] => acc 
	      | x :: xs => aux(xs, acc + card_value_challenge(x))
    in
	aux(cs, 0)
    end

fun minCardOption(hc : card list) =
    let fun aux(lst, minValue, c) =
	    case lst of
		[] => if minValue = 12
		      then NONE
		      else SOME c
	      | x :: xs => if card_value_challenge(x) < minValue
			   then aux(xs, card_value_challenge(x), x)
			   else aux(xs, minValue, c)
    in
	aux(hc, 12, (Spades, Ace)) (*Spades Ace does not make sense!*)
    end

fun careful_player(cs : card list, goal : int) = 
    let fun aux(cs, hc, ms) =
	    if score_challenge(hc, goal) = 0
	    then ms
	    else
		case cs of 
		    [] => ms
		  | x :: xs => if (goal - sum_cards_challenge(hc)) > 10
			       then aux(xs, x :: hc, ms @ [Draw])
			       else
				   case discardDrawOption(x :: hc, goal) of
				       SOME c => ms @ [Discard c, Draw]
				     | NONE => if (sum_cards_challenge(x :: hc)) <= goal
					       then aux(xs, x :: hc, ms @ [Draw])
					       else 
						   case minCardOption(hc) of
						       NONE => ms 
						     | SOME mc => aux(cs, remove_card(hc, mc, IllegalMove), ms @ [Discard mc])
    in
	aux(cs, [], [])
    end


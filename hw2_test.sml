(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

use "1130379020_hw2.sml";

fun all_except_option_test () = (* correct behavior: return SOME ["Zhangqu", "Qu"] *)
    let val p1 = "Yu"
	val p2 = ["Yu", "Zhangqu", "Qu"]
    in
	all_except_option(p1, p2)
    end

fun get_substitutions1_test () = (* correct behavior: return ["Jeffrey","Geoff","Jeffrey"] *)
    let val p1 = [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]]
	val p2 = "Jeff"
    in
	get_substitutions1(p1, p2) 
    end

fun get_substitutions2_test () = (* correct behavior: return ["Geoff","Jeffrey","Jeffrey"] *)
    let val p1 = [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]]
	val p2 = "Jeff"
    in
	get_substitutions2(p1, p2) 
    end

fun similar_names_test () = (* correct behavior: return [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}] *)
    let val p1 = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]]
	val p2 = {first="Fred", middle="W", last="Smith"}
    in
	similar_names(p1, p2) 
    end

fun card_color_test () = (* correct behavior: return Black *)
    let val p1 = (Spades, Ace)
    in
	card_color(p1) 
    end

fun card_value_test () = (* correct behavior: return 11 *)
    let val p1 = (Spades, Ace)
    in
	card_value(p1) 
    end

fun remove_card_test () = (* correct behavior: [(Clubs,Ace),(Clubs,Ace),(Spades,Ace)] *)
    let val p1 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val p2 = (Spades, Ace)
    in
	remove_card(p1, p2, IllegalMove)
    end

fun all_same_color_test () = (* correct behavior: return true *)
    let val p1 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
    in
	all_same_color(p1) 
    end

fun sum_cards_test () = (* correct behavior: return 44 *)
    let val p1 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
    in
	sum_cards(p1) 
    end

fun score_test () = (* correct behavior: return 6 *)
    let val p1 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val p2 = 40
    in
	score(p1, p2) 
    end

fun officiate_test1 () = (* correct behavior: raise IllegalMove *)
    let val p1 = [(Clubs,Jack),(Spades,Num(8))]
	val p2 = [Draw,Discard(Hearts,Jack)]
    in
	officiate(p1,p2,42)
    end

fun officiate_test2 () = (* correct behavior: return 3 *)
    let val p1 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val p2 = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(p1,p2,42)
    end

fun score_challenge_test () = (* correct behavior: return 4 *)
    let val p1 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
    in
	score_challenge(p1, 1) 
    end

fun officiate_challenge_test () = (* correct behavior: return 1 *)
    let val p1 = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val p2 = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate_challenge(p1,p2,3)
    end

fun careful_player_test () = (* correct behavior: return [Draw, Draw, Draw, Discard (Clubs, Ace), Discard (Hearts, Ace), Discard (Spades, Ace), Draw] *)
    let val p1 = [(Spades,Ace),(Hearts,Ace),(Clubs,Ace),(Spades, Jack),(Diamonds,Ace)]
	val p2 = 10
    in
 	careful_player(p1,p2)
    end

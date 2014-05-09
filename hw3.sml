(* Dan Grossman, CSE341 Spring 2013, HW3 Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(*g takes two functions(one for Wildcard, one for Variable) and a pattern to calculate the value of each pattern based on the two functions.*)
fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals xs = List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs = foldl (fn (x, a) => if String.size x > String.size a then x else a) "" xs

fun longest_string2 xs = foldl (fn (x, a) => if String.size x >= String.size a then x else a) "" xs

fun longest_string_helper f = foldl (fn (x, a) => if f(String.size x, String.size a) then x else a) ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = longest_string1 o only_capitals

fun rev_string s = (implode o rev o explode) s

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME v => v
			   
fun all_answers f xs = 
    let fun aux(xs, acc) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE 
			    | SOME v => aux(xs', acc @ v)
    in
	aux(xs, [])
    end

fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) String.size p

fun count_some_var (s, p) = g (fn () => 0) (fn x => if s = x then 1 else 0) p

fun check_pat p =
    let fun aux1 p = 
	    case p of
		Variable s => [s] 
	      | TupleP ps => foldl (fn(p, a) => a @ (aux1 p)) [] ps 
	      | ConstructorP (_, p) => aux1 p
	      | _ => []
	
	fun aux2 xs =
	    case xs of
		[] => true 
	     |  x::xs'=> (not (List.exists (fn a => x = a) xs')) andalso aux2 xs'
    in
	(aux2 o aux1) p
    end

fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME [] 
      | (v, Variable s) => SOME [(s, v)] 
      | (Unit, UnitP) => SOME [] 
      | (Const i, ConstP j) => if i = j then SOME [] else NONE 
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE 
      | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2
						     then match(v, p)
						     else NONE
      | (_, _) => NONE
 
fun first_match v ps = 
    SOME(first_answer (fn x => match(v, x)) ps) handle NoAnswer => NONE

fun helper1 (typs, p) =
    case p of
        UnitP => UnitT
      | ConstP _ => IntT
      | TupleP ps => TupleT (List.map (fn x => helper1(typs, x)) ps)
      | ConstructorP(str, p) => let fun aux x =
				      case x of
					  (s, _, pp) => (s = str) andalso (helper1(typs, p) = pp orelse helper1(typs, p) = Anything)
			      in case List.find aux typs of
				     SOME (_, a, _) => Datatype a
				   | NONE => raise NoAnswer
			      end
      | _ => Anything

fun helper2 (t1, t2) =
    if t1 = t2
    then t1
    else case (t1, t2) of
             (_, Anything) => t1
           | (Anything, _) => t2
           | (TupleT ps1, TupleT ps2) => if List.length ps1 = List.length ps2
					 then TupleT(List.map helper2 (ListPair.zip(ps1, ps2)))
					 else raise NoAnswer
           | (_, _) => raise NoAnswer
			     
fun typecheck_patterns (typs, ps) =
    let val lst = List.map (fn x => helper1(typs, x)) ps
                  handle NoAnswer => []
    in
        case lst of
            [] => NONE
          | x::xs' => SOME (List.foldl helper2 x xs')
                      handle NoAnswer => NONE
    end

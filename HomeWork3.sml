 (* Coursera Programming Languages, Homework 3, Provided Code *)

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
(*String List -> String List*)
(*Pick all strings that starts with Capital Letter, put them in a list, Produce that list*)
fun only_capitals words =
   List.filter (fn word =>  Char.isUpper(String.sub(word, 0)))  words

				       
(*String List -> String*)
(*Return the longest string, or "" if list is empty.  in case you found more than a string with
		the same length just return the first longest one*)
fun longest_string1 words =
    foldl (fn (w, x) => if String.size(w) > String.size(x) then w else x) "" words

fun longest_string2 words = foldl (fn (w, x) => if String.size(w) >= String.size(x) then w else x) "" words
(*Same as longest_string1 and 2 but more general, as you pass the function to it*)
fun longest_string_helper f words =
    let fun helper (acc,ws) =
	    case ws of
                    x::xs => if f(String.size(x), String.size(acc)) then helper(x,xs) else helper(acc,xs) 
	      | []  => acc
    in helper ("",words) end
(*longest_string3 and longest_string4 are defined with val bindings and partial applications of longest_string_helper*)	
val longest_string3 =  fn words => longest_string_helper (fn (w, x) => w > x) words
val longest_string4 = fn words => longest_string_helper (fn (w,x) => w >= x) words

(*String List => String*)
(*produces the longest string that starts with a Capital letter, or "" if there is no such result;           in case you found more than a string with the same length and also capitalized, just return the first*)
val longest_capitalized =  longest_string3 o only_capitals 					

(*String -> String*)
(*produce the same String But in reverse oreder*)
val rev_string = implode o rev o explode
					
(*(a' -> b' option) -> a' List -> b'*)
(*Keep inserting each element of the list to the given function until you find the first Some value, return it. If you reached a point where the list is empty raise NoAnswer*)
fun first_answer f lst =
     if null lst then raise NoAnswer else
       if isSome(f(hd lst)) then (hd lst) else first_answer f (tl lst)

(*(a' -> b' List Option) -> a' List -> b' List Option*)
(**)
fun all_answers f lst0 =
    let fun helper (acc, lst) =
	    case lst of 
	     x::xs => let val answer = f(x) in
			    case answer of
				NONE => NONE
			        |   SOME lst => helper(lst @ acc, xs) end
	     |      []    => SOME(acc)
    in  helper([], lst0) end

(*Pattern -> Int*)
(*Counts number of wildcards in the given pattern*)
fun count_wildcards p = g (fn () => 1) (fn x  => 0) p

(*Pattern -> Int*)
(*Adds number of wildcards to the lengths of the variables and returns them*)
val count_wild_and_variable_lengths = fn p => g (fn () => 1) (fn x => String.size(x)) p

(*(String * Pattern) -> Int*)
(*returns how many times the string appeared in the pattern as a variable*)
fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

(*Pattern -> Boolean*)
(*Retruns true if and only if all the variables appearing in the pattern are distinct*)
fun check_pat p =
    let val extract_variables = fn p => foldl (fn (x, acc) => case x of Variable s => s::acc
						          | _ => acc) [] p
        fun check_repetition lst =
	    let fun check_x lst2 = case lst2 of
				       x::y::xs => if not (x = y) then check_x(x::xs) else false
				     | [x] =>   true
	    in case lst of
		   x::xs => if check_x(x::xs) then check_repetition xs else false
		 | []  => true
	    end
    in
	case p of
	    TupleP ps => check_repetition(extract_variables ps)
	 | _ => true
    end
			     
(*valu * pattern -> (string * valu) list option*)
(*Retruen NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.*)

fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
         |  (Unit, UnitP) => SOME [] 
         |  (Const i, ConstP i') => if i = i' then SOME [] else NONE
         |  (_, Variable s) =>   SOME [(s, v)] 
         |  (Tuple vs, TupleP ps)  => if  (List.length(vs) = List.length(ps)) then
			 let fun f (vs, ps, acc) =
				 case (vs, ps) of
				     (v::vs', p::ps') => if isSome(match(v, p))
			 then f(vs', ps',  valOf(match(v,p)) @ acc) else NONE
				  | ([], []) =>  SOME(acc) in f(vs, ps, []) end									 
			else NONE
	 |  (Constructor (s, v), ConstructorP (s', p)) => if s = s' andalso isSome(match(v, p))
					  then  match(v,p) else NONE
         | _ => NONE 

(*Valu * Pattern List -> (String * valu) list option*)
(*returns NONE if no pattern in the list matches the valu, SOME lst for the first pattern that mathces it*)
fun first_match (v, ps) =
    match(v,  first_answer (fn p => match(v, p)) ps handle NoAnswer => UnitP )

	    


			      


						







							    

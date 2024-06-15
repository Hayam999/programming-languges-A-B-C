(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*String String List -> String Option*)
(*return an identical list of the given one after removing given string if exist else return NONE*)
(*ASSUME the string will appear at most once*)

fun all_except_option (str, str_lst) =
    let 
        fun helper (lst, string_exist) =
            case lst of
                [] => if string_exist then SOME [] else NONE
              | x::xs => if same_string(x, str) then helper(xs, true)
                         else 
                             case helper(xs, string_exist) of
                                 NONE => NONE
                               | SOME lst' => SOME (x::lst')
    in
        helper(str_lst, false)
    end
	
fun get_substitutions1(substitutions, s) =
    case substitutions of
        x::xs => let val answer = all_except_option(s, x) in 
                                if answer = NONE then get_substitutions1(xs, s)
                                else valOf(answer) @ get_substitutions1(xs, s)
                            end
    |
        [] => []
	     

fun get_substitutions2(substitutions, s) =
    (*acc is a list accumulating the value of answer*)
    let fun helper(subs, acc) =
	    case subs of
		x::xs => let val answer = all_except_option(s, x) in
			     if answer = NONE then helper(xs, acc) else
			     helper(xs, acc @ valOf(answer)) end
	     |           [] => acc
    in
	helper(substitutions, [])
    end

(*String List List     Record -> Record List*)
(*Calls get_substitutions2 then Constructs a list of records{first= one name at a time of subs
                                                                        middle= same as param
                                                                         last= same also}*)
															    
fun similar_names(substitute, full_name) =
    case full_name of
	{first= x, middle= y, last= z} =>
	let val names = get_substitutions2(substitute, x) in
	    let fun helper(first_names, acc) =
		    case first_names of
			f::xs  => helper(xs, {first=f, middle= y, last= z } :: acc)
		     |           []     => acc
	    in
		helper(names, [full_name]) end
	end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace  | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
exception NoSuchCard	      
(* put your solutions for problem 2 here *)
(*Card -> Color*)
(*Returns the color of the card; Black for Spades&clubs, Red for diamond&hearts*)	      
fun card_color(c) =
    case c of
	(s,r) =>  if s = Spades orelse s = Clubs then Black else Red
				 
				
(*Card -> int*)
(*Return value of card; value of - int = themselves
                                       - Ace = 11
                                       - any other rank = 10*)					       
fun card_value(c) =
    case c of
	(s, r) =>  case r of
		           Ace => 11
		    |      Num i => i
		    |       _ => 10

(*Card Card -> Boolean*)
(*check if 2 cards are equal*)						
fun card_equal(x: card, c: card) =
    x = c
(*Card List   Card   Exception -> Card List  OR Exception*)
(*Remove First appearance for card and then return Card List
  If Card wasn't in Card List RAISE Exception
 *)						
fun remove_card(cs, c, e) =
    let fun helper(card_list, result, first_look) =
	    case card_list of
		x::xs => if card_equal(x, c) andalso first_look then
			     helper(xs, result, false) else
			 helper(xs, x::result, first_look)
	     |      []     => if not first_look then result else raise e
    in
	helper(cs, [], true)
    end
			    
(*val unit_test13 = remove_card([(Clubs, Jack), (Hearts, Num 9), (Spades, King)], (Clubs, Queen), NoSuchCard)*)

(*Card List  -> Boolean*)
(*True if all cards has same color false otoherwise*)		      
fun all_same_color(cs) =
    case cs of
	x::y::xs => if card_color(x) = card_color(y) then all_same_color(xs) else false
	|  [x]   =>  true 
     |      []       => true



(*Card List -> Integer*)
(*retrun sum of cards' values*)
fun sum_cards(card_list) =
    let fun helper(cs, acc) =
	    case cs of
		x::xs => helper(xs, card_value(x) + acc)
	     |      []    => acc
    in
	helper(card_list, 0)
    end

 (*Card List Int -> Int*)
(*Caluclate the Score of the held cards*)
fun score(held_cards, goal) =
    let fun pre_score() =
	    if sum_cards(held_cards) > goal then 3 * (sum_cards(held_cards) - goal) else
	    goal - sum_cards(held_cards)
    in
	if all_same_color(held_cards) then pre_score() div 2 else pre_score()
    end

(*Card List   Move List   Int -> Int*)
(*Plays the game with applying all moves and return Score at the end*)	
fun officiate (card_list0, move_list0, goal) =
    let fun play (held_cards, card_list, move_list) =
	    case (card_list, move_list) of
	           	([], _)    => score(held_cards, goal)
	     |      (_, [])    => score(held_cards, goal)
	     |      (f::fs, x::xs) => (case x of
		                             Draw => if sum_cards(f::held_cards) > goal
			                           then score(f::held_cards, goal) else
                                            play(f::held_cards, fs, xs)
					  |  Discard c => play
		 (remove_card(held_cards,c,IllegalMove) handle IllegalMove => held_cards
							      , f::fs, xs)) 
    in
        play([], card_list0, move_list0) 
    end

             

(*Card List -> Int*)
(*Return How many aces are there in the list*)
fun num_of_aces (card_list) =
    let fun helper (cs, aces) =
    case cs of
	(_, Ace)::xs => helper(xs, aces+1)
         |  (_, x):: xs    => helper(xs, aces)
         |  []       => aces
    in helper(card_list, 0) end

									   
(*Card List -> Card List*)
(*Remove all aces from the given list*)
fun remove_aces (card_list) =
    let fun helper (cs, result) =
	    case cs of
		(_, Ace)::xs => helper(xs, result)
	 |           x::xs    => helper(xs, x::result)
             |           []       => result
    in 
	helper(card_list, [])
    end
(*Card List -> Int*)
(*Same function as before but with 2 possibiltes for Ace value wheter to be 1 or 11*)
fun score_challenge (held_cards, goal) =
      let fun pre_score (sum) =
	    if sum > goal then 3 * (sum - goal)
	    else goal - sum
            fun check_color (pre_score) =
		if all_same_color(held_cards) then pre_score div 2 else pre_score
      in
	  if num_of_aces(held_cards) = 0 then
	      check_color(pre_score(sum_cards(held_cards)))
	  else
              let fun iterate_over_aces (aces0) =
		 let fun helper (aces, best_solution) =
			 case aces of
			     0 => best_solution
			  |    _ => if best_solution > goal then helper(aces-1, best_solution+1)
				 else let val ace1 = pre_score(best_solution+1)
                                                          val ace11 = pre_score(best_solution+11) in
					     if ace1 > ace11 then helper(aces-1, ace11)
					     else helper(aces-1, ace1) end
		 in helper(aces0, sum_cards(remove_aces(held_cards))) end
	in check_color(iterate_over_aces(num_of_aces(held_cards))) end 
      end

(*Card -> Int*)
(*Same as card_value with changing value of ace to be 1*)
fun card_value_challenge(c) =
    case c of
	(s, r) =>  case r of
		           Ace => 1
		    |      Num i => i
		    |       _ => 10
(*Card List -> Int*)
(*same as sum_cards with changing the value of ace to be 1 instead of 11*)
fun sum_cards_challenge(card_list) =
    let fun helper(cs, acc) =
	    case cs of
		x::xs => helper(xs, card_value_challenge(x) + acc)
	     |      []    => acc
    in
	helper(card_list, 0)
    end
								      
(*Card List   Move List   Int -> int*)
(*Plays the game with just like Officiate but with changing the possibility of Ace to be 1 or 11*)
fun officiate_challenge (card_list0, move_list0, goal) =
    let fun play (held_cards, card_list, move_list) =
	    case (card_list, move_list) of
	           	([], _)    => score_challenge(held_cards, goal)
	     |      (_, [])    => score_challenge(held_cards, goal)
	     |      (f::fs, x::xs) => (case x of
		                             Draw => if sum_cards_challenge(f::held_cards) > goal
			                           then score_challenge(f::held_cards, goal) else
                                            play(f::held_cards, fs, xs)
					  |  Discard c => play
		 (remove_card(held_cards,c,IllegalMove) handle IllegalMove => held_cards
							      , f::fs, xs)) 
    in
        play([], card_list0, move_list0) 
    end

(*Card List  Int -> Move List*)						      
(*Produce a carefull choice of moves*)
fun carefull_player (card_list0, goal) =
    let fun choose (card_list, held_cards, move_list) =
	    if score(held_cards, goal) <= 0 then move_list
	    else
	    case(card_list, held_cards) of
		(x::xs, h::hs) =>  if score(x::remove_card(held_cards, h, IllegalMove), goal) <= 0 then
				       Discard h:: Draw::move_list
				   else
				    if goal > 10 andalso goal > sum_cards(held_cards) then
			      choose(xs, x::held_cards, Draw::move_list)				    else choose(x::xs, remove_card(held_cards, h, IllegalMove), Discard h::move_list)
	     |  ([], _ )=> move_list
	     | _        => choose(card_list, held_cards, move_list)
in choose(card_list0, [], []) end

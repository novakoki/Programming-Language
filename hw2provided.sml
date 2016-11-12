(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun all_except_option (s, li) =
  case li of
      [] => NONE
    | a::b => if same_string(a,s)
	      then SOME(b)
	      else let val res = all_except_option(s,b)
		   in case res of
			  NONE => NONE
		       |  SOME(c)=> SOME(a::c)
		   end

fun get_substitutions1 (li,s) =
  case li of
      [] => []
    | a::b => let val res = all_except_option(s,a)
	      in case res of
		     NONE => get_substitutions1(b,s)
		   | SOME(c) => c @ get_substitutions1(b,s)
	      end

fun get_substitutions2 (list,string) =
  let fun get(li,s,acc) =
	  case li of
	      [] => acc
	    | a::b => let val res = all_except_option(s,a)
		      in case res of
			     NONE => get(b,s,acc)
			   | SOME(c) => get(b,s,acc@c)
		      end
  in get(list,string,[])
  end

fun similar_names (list,record) =
case list of
    [] => [record]
  | _ => case record of
		{first=f, middle=m, last=l} =>
		let val res = f::get_substitutions2(list,f)
		    fun get (li,acc) =
		      case li of
			  [] => acc
			| a::b => get(b,acc@[{first=a,middle=m,last=l}])
		in get(res,[])
		end

fun card_color (card) =
  case card of
      (Spades,rank) => Black
   |  (Clubs,rank) => Black
   |  _ => Red

fun card_value (card) =
  case card of
      (suit,Num a) => a
   |  (suit,Ace) => 11
   |  _ => 10

fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
   |  a::b =>  if a = c
	       then b
	       else a::remove_card(b,c,e)

fun all_same_color (cs) =
  case cs of
      [] => true
   |  _::[] => true
   |  head::(neck::rest) => (card_color(head) = card_color(neck)) andalso all_same_color(neck::rest)

fun sum_cards (cs) =
  let fun sum (li,acc) =
	case li of
	    [] => acc
	 |  a::b => sum(b, acc+card_value(a))
  in sum(cs,0)
  end

fun score (cs, goal) =
  let val sum = sum_cards(cs)
  in  let val pre = if sum > goal
		    then 3 * (sum - goal)
		    else goal - sum
      in if all_same_color(cs)
	 then pre div 2
	 else pre
      end
  end

fun officiate (card_list, move_list, goal) =
  let fun play (cs, hs, ms, sum) =
        case cs of
            [] => score(hs, goal)
         |  _ =>
            if sum > goal
            then sum
            else
                case ms of
	                  [] => score(hs, goal)
                 |  head::tail =>
                    case head of
		                    Discard card =>
                        let
                            val new_hs = remove_card(hs, card, IllegalMove)
                        in
                            play(cs, new_hs, tail, score(new_hs, goal))
                        end
		                 |  Draw =>
                        case cs of
                            card::remain =>
                            play(remain, card::hs, tail, score(card::hs, goal))
  in play(card_list, [], move_list, 0)
  end

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

fun only_capitals li =
    List.filter (fn s => Char.isUpper(String.sub(s,0))) li

fun longest_string1 li =
  List.foldl (fn (x,y) => if String.size(x) <= String.size(y) then y else x) "" li

fun longest_string2 li =
  List.foldl (fn (x,y) => if String.size(x) < String.size(y) then y else x) "" li

fun longest_string_helper f li =
  List.foldl(fn (x,y) => if f(String.size(x),String.size(y)) then x else y) "" li

fun longest_string3 li =
  longest_string_helper Int.> li

fun longest_string4 li =
  longest_string_helper Int.>= li

fun longest_capitalized li =
  longest_string_helper Int.> (only_capitals(li))

fun rev_string s =
  (String.implode o List.rev o String.explode) s

fun first_answer f li = 
 case li of
      a::b => let val ans = f(a)
	      in case ans of
		     NONE => first_answer(f) b
		  |  SOME(v) => v
	      end
    | [] => raise NoAnswer

fun all_answers f li =
  let fun helper(li, acc) =
	case li of
	    [] => acc
	 |  a::b =>
      let val ans = f(a)
		  in
          case ans of
			        NONE => helper(b, acc)
			     |  SOME(v) =>
              case acc of
					        NONE => helper(b, SOME(v))
				       |  SOME(u) => helper(b, SOME(u@v))
      end
  in
      case li of
          [] => SOME []
        | _ => helper(li, NONE)
  end

fun count_wildcards p =
  g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = 
  g (fn () => 1) (fn x => String.size(x)) p

fun count_some_var (s, p) = 
  g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
  let fun pattern_list p =
	      case p of
	          Variable s => [s]
	        | TupleP ps => List.foldl(fn (p, i) => pattern_list(p)@i) [] ps
          | ConstructorP (s, p1) => pattern_list(p1)
	        | _ => []
      fun is_repeat li =
	      case li of
	          [] => true
	        | a::b => not(List.exists(fn x => x = a) b)
  in is_repeat(pattern_list(p))
  end

fun match (v, p) =
  case p of
      Wildcard => SOME []
    | ConstP a =>
      let val a = a in
          case v of
              Const b => if a = b then SOME [] else NONE
            | _ => NONE
      end
    | UnitP => if v = Unit then SOME [] else NONE
    | Variable s => SOME [(s, v)]
   | TupleP ps =>
      let val ps = ps in
      case v of
          Tuple vs => if length vs = length ps then all_answers match (ListPair.zip(vs, ps)) else NONE
        | _ => NONE
      end
   | ConstructorP (s, p1) =>
      let val s1 = s in
          case v of
              Constructor (s2, v1) => if s1 = s2 then match (v1, p1) else NONE
            | _ => NONE
      end

fun first_match v li =
  SOME(first_answer (fn p => match(v, p)) li)
  handle NoAnswer => NONE

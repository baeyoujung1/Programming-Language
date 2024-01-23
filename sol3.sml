datatype pattern = Wildcard | Variable of string | UnitP 
                  | ConstP of int | TupleP of pattern list 
                  | ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list 
              | Constructor of string * valu

open Option;

fun check_pat(p) = 
    let
        fun check1(Variable v) = [v]
            | check1(TupleP ps) = List.foldl(fn(p', acc) => acc @check1(p')) [] ps
            | check1(_) = []
        fun check2([]) = false
            | check2(x::xs) = List.exists(fn x' => x = x') xs orelse check2 xs 
    in 
        let
            val result1 = check1 p
            val result2 = check2 result1
            val result3 = not result2
        in
            result3
        end
 
    end;

fun reverse [] = SOME []
  | reverse (SOME x::xs) = Option.map (fn x_ => x::x_) (reverse xs)
  | reverse (NONE :: _) = NONE;


fun match(a, b) =
    let 
        fun matches(_, Wildcard) = SOME []
            | matches(Unit, UnitP) = SOME []
            | matches(Const a, ConstP b) = 
                if a = b then SOME [] else NONE
            | matches(a, Variable b) = SOME [(b, a)]
            | matches(Constructor(q, w),ConstructorP(e, r)) =
                    if q = e then matches(w, r) else NONE
            | matches(Tuple a, TupleP b) = Option.map 
                    List.concat(reverse(List.map matches(ListPair.zip(a, b))))
            | matches _ = NONE;
    in
        if check_pat b then matches(a, b) else NONE
    end



type name = string
datatype RSP =
    ROCK
  | SCISSORS
  | PAPER
datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
    PLAYER of name * (RSP strategy ref)
  | MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)


fun next strategyRef = 
    let 
        val Cons (rsp, func) = !strategyRef 
    in 
        strategyRef := func(); rsp 
    end;


fun rsprsp(x, y) =
    let
        val PLAYER(x1, a) = x
        val PLAYER(x2, b) = y
        val xnext = next(a)
        val ynext = next(b)
    in
        case (xnext, ynext) of 
            (ROCK, SCISSORS) => x
          | (SCISSORS, PAPER) => x
          | (PAPER, ROCK) => x
          | (ROCK, PAPER) => y
          | (SCISSORS, ROCK) => y
          | (PAPER, SCISSORS) => y
          | _ => rsprsp(x, y)
    end


fun whosWinner(t) =
    case t of
        PLAYER _ => t
      | MATCH(x, y) =>
          let
            val winner = rsprsp(whosWinner(x), whosWinner(y))
          in
            case winner of
              PLAYER _ => winner
          end


        
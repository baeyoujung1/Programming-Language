datatype expr = NUM of int
             | PLUS of expr * expr
             | MINUS of expr * expr

datatype formula = TRUE
                 | FALSE
                 | NOT of formula
                 | ANDALSO of formula * formula
                 | ORELSE of formula * formula
                 | IMPLY of formula * formula
                 | LESS of expr * expr

fun eval f =
  let
    fun eval_in e = case e of
       NUM n => n
      | PLUS (e1, e2) => eval_in e1 + eval_in e2
      | MINUS (e1, e2) => eval_in e1 - eval_in e2
  in
    case f of
       TRUE => true
      | FALSE => false
      | NOT f' => not (eval f')
      | ANDALSO (f1, f2) => eval f1 andalso eval f2
      | ORELSE (f1, f2) => eval f1 orelse eval f2
      | IMPLY (f1, f2) => (not (eval f1)) orelse (eval f2)
      | LESS (e1, e2) => eval_in e1 < eval_in e2
  end

type name = string
datatype metro = STATION of name
 | AREA of name * metro
 | CONNECT of metro * metro


fun checkMetro metro =
  let
    fun checkMetro_in (name, metro2) =
      let
        fun checkStation (STATION s) = s = name
          | checkStation (AREA (name', metro2')) = checkMetro_in (name', metro2')
          | checkStation (CONNECT (metro1, metro2)) =
              checkStation metro1 andalso checkStation metro2
      in
        checkStation metro2
      end
  in
    case metro of
      STATION _ => true
    | AREA (name, metro2) => checkMetro_in (name, metro2)
    | CONNECT (metro1, metro2) => checkMetro metro1 andalso checkMetro metro2
  end

datatype 'a lazyList = nullList 
 | cons of 'a * (unit -> 'a lazyList)

fun seq(first, last) =
    if first > last then nullList
    else cons(first,fn() => seq(first+1,last))

fun infSeq(first) =
    cons(first,fn()=>infSeq(first+1))

fun firstN(lazyListVal, n) =
  let
    fun firstN_in(lazyListVal, n, acc) =
      if n = 0 then rev acc
      else
        case lazyListVal of
          nullList => rev acc
        | cons(hd, tlFunc) => firstN_in(tlFunc(), n-1, hd::acc)
  in
    firstN_in(lazyListVal, n, [])
  end

fun Nth(lazyListVal, n) =
  let
    fun Nth_in(lazyListVal, i) =
      case lazyListVal of
        nullList => NONE
      | cons(hd, tlFunc) =>
          if i = n then SOME hd
          else Nth_in(tlFunc(), i+1)
  in
    Nth_in(lazyListVal, 1)
  end

fun filterMultiples(lazyListVal, n) =
    case lazyListVal of
        nullList => nullList
      | cons(hd, tlFunc) =>
          if hd mod n = 0 then filterMultiples(tlFunc(), n)
          else cons(hd, fn () => filterMultiples(tlFunc(), n))

fun sieve(lazyListVal) =
    case lazyListVal of
        nullList => nullList
      | cons(hd, tlFunc) => 
          cons(hd, fn () => sieve(filterMultiples(tlFunc(), hd)))

fun primes() = sieve(infSeq(2))
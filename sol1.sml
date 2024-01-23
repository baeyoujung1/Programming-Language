fun merge(xs:int list, ys:int list) =

    if null xs
    then ys
    else if null ys
    then xs
    else if hd(xs)<hd(ys)
    then hd(xs)::merge(tl(xs),ys)
    else hd(ys)::merge(xs,tl(ys))


fun reverse(xs:int list) =

    let fun reverse2(xs:int list,ys:int list)=
            if null xs
            then ys
            else reverse2(tl(xs),hd(xs)::ys)
    in 
        reverse2(xs,[])
    end


fun pi(a, b, f) =

    let fun pi2(x, y) =
            if x>b
            then y
            else pi2(x+1, y*f(x))
    in
        pi2(a, 1)
    end


fun digits(x:int)=

    let fun digits2(x: int, ys: int list) =
            if x<10 
            then x::ys
            else digits2(x div 10, (x mod 10)::ys)
    in
        digits2(x, [])
    end


fun additivePersistence(x:int)=

    let fun digit(x:int)=

            if x<10 
            then x
            else (x mod 10) + digit(x div 10)

        fun digit2(x:int, y:int)=
            if x<10 
            then y 
            else digit2(digit(x), y + 1)

    in
        if x<10
        then 0
        else digit2(x,0)
    end

fun digitalRoot(x:int)=

    let fun digit(x:int)=

            if x<10 
            then x
            else (x mod 10) + digit(x div 10)
    in
        if x<10
        then x
        else digitalRoot(digit(x))
    end
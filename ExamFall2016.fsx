type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list
let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);
            ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

let rec inv = function
    |[]-> true
    |(_,_,p)::(n,e,p1)::rest when p>p1 && p1>0 ->inv ((n,e,p1)::rest)
    |(_,_,_)::rest -> false;;

////

let rec insert s sb = match s,sb with  
    |(s,[]) -> [s]
    |((n,e,p),(n1,e1,p1)::rest) when p>p1->(n,e,p)::(n1,e1,p1)::rest
    |((n,e,p),(n1,e1,p1)::rest) -> (n1,e1,p1)::(insert (n,e,p) (rest)) 
///

let rec get (n,sb) = match sb with 
    |[]->[]
    |(name,e,p)::rest when n=name -> (e,p)::get(n,rest)
    |(name,e,p)::rest -> get(n,rest)

///
let rec top k sb = match k with 
        |k when k < 0 -> None
        |K when k> List.length sb -> None
        |k -> Some (List.take k sb)
top 0 sb
////////////////////////////////////
type T<'a> = N of 'a * T<'a> list;;

let td = N("g", []);;
let tc = N("c", [N("d",[]); N("e",[td])]);;
let tb = N("b", [N("c",[])]);;
let ta = N("a", [tb; tc; N("f",[])]);;


let rec toListH tls = match  tls with
    |[]-> []
    |N(a,b)::rest -> a::(toListH rest)@(toListH b)
and toList t = match t with 
    |N(a,[]) -> [a]
    |N(a,b) -> [a]@ (toListH b)
//can use list.collect
////

let rec mapH f tls = match tls with
    |[] -> []
    |s::rest -> map f s :: mapH f rest
and map f t = match t with 
    |N(a,[]) -> N(f a , [])
    |N(a,b) -> N(f a , mapH f b)
///
type Path = int list;;

let rec isPath is t = match is,t with 
        |([],t) -> true
        |(n,N(a,[])) -> false 
        |(n::rest,N(a,b)) when n> (List.length b) || 0>n->  false
        |(n::rest,N(a,b)) -> isPath rest (List.item n b )

isPath [1;1;0;0] ta
/////

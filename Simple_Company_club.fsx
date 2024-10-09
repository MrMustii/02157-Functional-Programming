type name = string;;
type year=int;;
type themes=string list;;
type phone=int;;

type description = phone*year*themes;;
type Member = name *description;;
type Register = Member list;;

let Alice :Member = ("Alice",(1,1234,["soccer";"jazz"]));;
let Bob :Member   = ("Bob",(2,2000,["soccer"]));;
let Charlie :Member = ("Charlie",(3,10,["jazz"]));;
let Dave :Member = ("Dave",(4,1982,["fishing";"Rock"]));;
let Eric :Member = ("Eric",(5,1983,["soccer";"jazz";"Games"]));;
let Frank :Member = ("Frank",(6,1999,["fossball"]));;

let reg =[Alice;Bob;Charlie;Dave;Eric;Frank];;



let rec SorJ = function
    |[]    -> false
    |x::xs -> if x = "soccer" ||x= "jazz" then true else SorJ xs;;

let rec s =function
    |[]    -> false
    |x::xs -> if x= "jazz" then true else SorJ xs;; 

let rec j =function
    |[]    -> false
    |x::xs -> if x = "soccer"  then true else SorJ xs;; 

let SandJ x = 
            if j x && s x then true else false;;




let p1 = function
    |(no,yb,ths) when yb > 1982 && SandJ (ths)-> true 
    |(no,yb,ths) -> false;;







let rec SorJYoung = function
    |[]    -> false
    |x::xs -> if x = "soccer" ||x= "jazz" then true else SorJ xs;; 

let p2 = function
    |(no,yb,ths) when yb > 1982 && SorJYoung(ths)-> true 
    |(no,yb,ths) -> false;;

let rec extractTargetGroup p = function
    |[] ->[]
    |(name,(phone,year,themes))::xs -> if p (phone,year,themes) then (name,phone)::extractTargetGroup p xs else extractTargetGroup p xs;;

extractTargetGroup p1 reg
extractTargetGroup p2 reg

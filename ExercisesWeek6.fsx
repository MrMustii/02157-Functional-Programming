//Tast 1
let rec collect f = function
| [] -> []
| x::xs -> f x @ collect f xs;;

collect (fun (a,b) -> [a..b]) [(1,3); (4,7); (8,8)];;
collect (fun (a,b) -> [a..b]) [('a','c'); ('d','g'); ('h','l')];;
//Task 2

type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list

let lc =[("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]);
        ("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])]
// 1
let rec findRoute (lid,lc) = 
    match (lid,lc) with 
    |(lid,[]) -> []
    |(lid,(l,lc)::rest) when lid =l-> lc 
    |(lid,(l,lc)::rest) -> findRoute (lid,rest);; 
findRoute ("SK 222-142",lc)

//2

let rec inRoute f r = 
    match f,r with
    |(f,[])->false
    |(f,(f1,air)::rest) when f =f1->true 
    |(f,(f1,air)::rest) ->inRoute f rest;;

inRoute "DL 124"  [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]

//3 
let rec withFlightH f ls=
    match f,ls with
    |(f,[])->false
    |(f,((f1,_)::rest)) when f=f1-> true
    |(f,((f1,_)::rest)) -> withFlightH f rest;;


let rec withFlight f lc =
    match f,lc with
    |(f,[]) ->[]
    |(f,(l,a)::rest) when withFlightH f a-> l:: withFlight f rest
    |(f,(l,a)::rest) -> withFlight f rest;;
withFlight "SK 208" lc;;
// (Lid * (Flight * Airport) list) list


//4 
type ArrivalCatalogue = (Airport * Lid list) list;;
let ac =[("ATL", ["DL 016-914"; "SK 222-142"]);
         ("BRU", ["DL 016-914"; "SK 222-142"]);
         ("JFK", ["SK 222-142"]);
         ("CPH", ["DL 016-914"])];;


let rec extendhelper (lid,airr,ac) =match lid, airr, ac with 
    |(lid,airr,[])->((airr,lid::[]))
    |(lid,airr,(air,a)::rest)when airr<>air-> extendhelper (lid, airr, rest)
    |(lid,airr,(air,a)::rest)when airr=air->((air,lid::a));;

let rec extend (lid,r,ac) :ArrivalCatalogue= 
    match lid,r,ac with
    |(lid,[],ac) -> [] 
    |(lid,((_,air)::rest),ac) -> extendhelper(lid,(air),ac)::extend  (lid,rest,ac);;


extend ("DL 016-91400000",[("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH");("SN 733","CPHkabab")],ac);;


(*
    extendhelper (lid,r,ac1)
        lid,(f,airo),air,_ when airo =air ->air,_::lid


extend extend (lid,r,ac) 
    [] -> []
    (ac::rest) -> extend helper ac ::extend rest 

*)




let rec math n = match n with 
            | 0 -> 1
            |_ ->(3*n-1)*(3*n-2)*1/2* (math (n-1));;

math 3
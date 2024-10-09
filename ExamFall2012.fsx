// Problem 1 (Approx. 30%) 
// 1. Delare a funtion inv: Multiset<'a> -> bool suh that inv(ms) is true when ms satisfies the multiset invariant.
let inv a =
    let e = List.map (fun (x,y)->x) a 
    let n = List.map (fun (x,y)->y) a 
    List.length (List.distinct e ) =List.length a && List.forall (fun x -> x>0) n;;

inv [("b",3); ("a",5); ("d",1)];;
inv [("b",3); ("a",5); ("d",-1)];;
inv [("b",3); ("a",5); ("a",1)];;
inv [];;

// 2 
let rec insert e n ms = match  ms with
    |[] -> [(e,n)]
    |(e1,n1)::rest when e =e1 -> (e,n+n1)::rest
    |(e1,n1)::rest -> (e1,n1)::(insert e n rest);;
insert "a" 2 [("b",3); ("a",5); ("d",1)];;
insert "a" 2 [("b",3); ("e",5); ("d",1)];;
//3 
let rec numberof e ms = match ms with 
    | [] -> 0
    |(e1,n)::rest when e1 =e -> n
    |(e1,n)::rest  ->  numberof e rest;;

numberof "b" [("b",3); ("a",5); ("a",1)];;
//4
let rec delete e ms = match ms with 
    |[] -> []
    |(e1,1)::rest when e=e1 -> rest
    |(e1,n)::rest when e=e1 -> (e1,n-1)::rest
    |(e1,n)::rest -> (e1,n)::(delete e rest);;

delete "c" [("b",3); ("a",5); ("c",1)];;


//5 
let rec union mss =
    let (m1,m2)= mss
    match m1,m2 with
        |([],m2) -> m2
        |((m1e,m1n)::rest, m2) -> insert m1e m1n (union (rest,m2));;

union ([("b",3); ("a",5); ("d",1)], [("a",3); ("b",4); ("c",2)])


//6  Give new delarations for inv, insert and union on the basis of the map representation.

let inv2 a = 
    Map.forall (fun k x-> x>0) a;;


inv2 (Map.ofList ([("b",3); ("a",5); ("d",1)]));;
inv2 (Map.ofList ([("b",3); ("a",5); ("d",-1)]));;
inv2 (Map.ofList ([]));;

let insert2 e n ms =
    let a = Map.tryFind e ms
    if a=None then Map.add e n ms else Map.add e ((Option.get(a)) + n) ms;;


insert2 "a" 2 (Map.ofList ([("b",3); ("a",5); ("d",1)]));;
insert2 "e" 2 (Map.ofList ([("b",3); ("a",5); ("d",-1)]));;

let rec union2 mss= 
    let (m1,m2) = mss
    let mls = Map.toList m1
    match mls, m2 with 
        | ([],m2) ->m2
        |((e,n)::rest,m2) -> insert2 e n (union2 (Map.ofList rest,m2));;

let mm= (Map.ofList ([("b",3); ("a",5); ("d",1)]),Map.ofList ( [("a",3); ("b",4); ("c",2)]));;
union2 mm;;
let x =(Map.ofList ([("b",3); ("a",5); ("d",1)]));;
let y =(Set.ofList ([("b",3); ("a",5); ("d",1)]));;


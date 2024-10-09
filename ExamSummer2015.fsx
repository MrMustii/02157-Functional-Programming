// Problem 2 (20%)
// 1. Declare a function mixMap so that mixMap f [x0; x1; . . . ; xm] [y0; y1; . . . ; ym] = [f(x0, y0); f(x1, y1); . . . ; f(xm, ym)]
let mixMap f a b =  
    let t= List.map2 (fun x y -> (x,y)) a b
    List.map f t;;
mixMap (fun (x,y) -> x+y) [1;1;1] [0;0;0];;

// 2. Declare a function unmixMap so that
// unmixMap f g [(x0, y0); (x1, y1); . . . ; (xn, yn)] = ([f x0; f x1; . . . ; f xn], [g y0; g y1; . . . ; g yn])

let unmixMap f g a = 
    let t = List.map (fun (x,y) -> x) a 
    let b = List.map (fun (x,y) -> y) a
    let mapeda = List.map f t
    let mapedb = List.map g b
    (mapeda,mapedb);;

let f = fun x -> x+1
let g = fun x -> x+2
unmixMap f g  [(1,0);(1,0);(1,0)];;

// 3. Give the most general types for mixMap and unmixMap

//// Problem 4
//1

let isValidCoursDesc desc =
    let (t,e) =desc
    e%5=0;;
let cb =Map.ofList [1,("a",5);2,("b",10);3,("c",10);4,("d",15);5,("e",5)]
isValidCoursDesc ("a",0)

(*the problem here was 0 can not be allowed to be true since it is only positive int*)

// 2
let isValidCoursBase cb =
    Map.forall(fun k c ->isValidCoursDesc c) cb;;

isValidCoursBase cb

(*this works fine*)
//3 
let disjoint s1 s2 =
    let intersection = Set.intersect s1 s2 
    Set.count intersection =0;;
disjoint (Set.ofList [1,("a",5);2,("b",10);3,("c",10);4,("d",15);5,("e",5)]) (Set.ofList [2,("a",5);3,("b",10);4,("c",10);5,("d",15);6,("e",5)])

(*
also works
synatx cant use ||> on a let ????????????????
*)

//4 
let sumECTS  cs cb =
    Map.fold(fun s n (t,e) -> if Set.contains n cs
                              then e+s
                              else s) 0 cb;;

let cs = Set.ofList [1;2;3]
sumECTS cs cb

(*DOES NOT WORK (Expecting a 'Map<string,int>' but given a 'Map<int,(string * int)>')*)

//5 
let isValidCoursGroup cg cb =
    let (man,opt)=cg
    let summan = sumECTS man cb
    let sumopt = sumECTS opt cb
    disjoint man opt &&
    summan <= 45 && (summan<45 or Set.count opt=0) &&
    (summan + sumopt >=45);;

let cb =Map.ofList [1,("a",5);2,("b",10);3,("c",10);4,("d",15);5,("e",5)]
let cg = (Set.ofList [1;2;3],Set.ofList [4;5])
isValidCoursGroup cg cb
(*Logic issues *)
//6 
let Dishelper a =
    let (x,y) = a
    (Set.toList x)@ Set.toList y;;
let tdisjoint a b c =
    let ls = (Dishelper a)@(Dishelper b) @(Dishelper c)
    List.length (List.distinct ls) =ls.Length;;
let isValid flag cb =
    let (bns,tc,pps,ep) = flag
    let ls =(Dishelper bns)@(Dishelper tc) @(Dishelper pps)
    isValidCoursGroup bns cb && isValidCoursGroup tc cb && isValidCoursGroup pps cb &&
    tdisjoint bns tc pps &&
    List.forall ep ls;;


    
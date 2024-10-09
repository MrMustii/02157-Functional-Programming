(*             Problem 1                   *)

(*
    
*)


let rec f n = function 
    | 0 -> 1
    | k when k>0 -> n * (f n (k-1))
    | _ -> failwith "illegal argument";;

let rec g p f = function
| [] -> []
    | x::xs when p x -> f x :: g p f xs
    | _::xs -> g p f xs;;

type T = 
    | A of int
    | B of string
    | C of T*T;;

let rec h = function
    | A n -> string n
    | B s -> s
    | C(t1,t2) -> h t1 + h t2;;

let sq = Seq.initInfinite (fun i -> 3*i);;

let k j = seq {for i in sq do 
                    yield (i,i-j) };;

let xs = Seq.toList (Seq.take 4 sq);;

let ys = Seq.toList (Seq.take 4 (k 2));;

(*1. Give an example of an application of each of the functions f, g and h*)
(* f is a power function, power function on a list of ints, so you find the power if the predicate p is true and h takes the bianary tree and 
makes the tree into one long string *)

(*2. Give the (most general) types of f, g and h and describe what each of these three functions computes. 
Your description for each function should focus on what it computes, rather than on individual computation steps.*)
(*
    f: int->int
    g: (int-> bool) -> (int -> int) -> int list -> int list 
    h: T -> string
*)


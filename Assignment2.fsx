//Name : Mustafa El-Madani
//Student Number: s215225
///////////////////////////Problem 1
#r "nuget: FsCheck";;
open FsCheck;;


let rec f x = function
    | [] -> []
    | y::ys -> (x,y)::f x ys;;

let rec allPairs xs ys =
    match xs with
    | [] -> []
    | x::xrest -> f x ys @ allPairs xrest ys;;

(* 1. The declaration of f is not tail recursive. Explan briefly why this is the case. *)

(*
    The tail recursion function evaluates the result of the current step of recursion before moving on to the 
    next recursion step. Here f does all the recursion calls first then evaluate the each step 
*)

(* 2. Provide a declaration of a tail-recursive variant of f that is based on an accumulating
parameter. Your tail-recursive declaration must be based on an explicit recursion.*)

let rec fTaila x xss a = match xss, a with 
    |([],a) -> a 
    |(xs::rest,a) -> fTaila x rest (a@[x,xs]);;

f "z" ["Y";"";"true"]
fTaila"z" ["Y";"";"true"] []

(*3. Provide a declaration of a continuation-based, tail-recursive variant of f. Your tail-
recursive declaration must be based on an explicit recursion.*)
let e s=s;;
 
let rec fTailc x xss c = match xss with 
    |[] -> c []
    |xs::rest -> fTailc x rest (fun res -> c ((x,xs)::res));;

fTailc "z" ["Y";"";"true"] id

/////////////////////////Problem 2
let rec f g (h1,h2) = function
    | [] -> []
    | x::xs when g x -> h1 x :: f g (h1,h2) xs
    | x::xs -> h2 x :: f g (h1,h2) xs;;

(* 1. Give the type for f and explain the value of the expression: f g (h1, h2) [x0; ...; xn−1]. *)

(*
    Looking at the inputs we can see that there are 3, a list of elements, g which is a function that produces a bool when one element is inputted,
    and a tuple of functions, h1 and h2, that take an element and output a value of unknown type. Moreover, the output of f is a list.
    (a->bool)->((a->b)*(a->b))-> list <a> -> list <b> 

    f maps h1 on the elements that pass the predicate g and maps h2 on the elements that fail g. 
    example 
    f (fun x -> x%2=0) ((fun a -> a+1 ),(fun a -> a-1 )) [1;2;3;4]
    will return [0;3;2;5]  as all even number have incremented  and odd numbers decremented
*)

(* 2. Give another declaration of f that is based on a single higher-order function from the List library. 
    The new declaration of f should not be recursive*)

let fHO g (h1,h2) ls = List.map (fun x -> if g x then h1 x else h2 x) ls;;  
fHO (fun x -> x%2=0) ((fun a -> a+1 ),(fun a -> a-1 )) [1;2;3;4]


type A<'a> = | D of 'a * bool
            | E of A<'a> * A<'a>
let rec g acc x = match x with
        | E(y,z) -> g (g acc z) y
        | D(a,true) -> a::acc
        | _ -> acc;;

let h x = g [] x;;

(* 3. Give 3 different values of type A<string list> using all constructors.*)
 
 (*
    D(["a"],true)
    E(D(["a"],true),D(["b"],false))
    E(E(D(["a"],true),D(["b"],false)),D(["c"],true)) 
 *)

(*4. Determine the types of g and h and describe what h computes. Your description should
focus on what it computes, rather than on individual computation steps*)

(*
    the type of g is list<a> -> A<a> -> list<a>. Since the function g takes acc which is a list and A as inputs and produces a list as output.
    Where the type of list and the type of A are the same.

    h has the same output as g but one less input  A<a> -> list<a>

    h produces a list of all the a values of D(a,b) where b is true, where g traverse the tree in a post order fashion
*)


(* 5. Is g a tail-recursive function? Your answer must be accompanied with an explanation.*)

(*
    g is not tail-recursive because all the step of recursion must happen before any computation happens, meaning that the stack must grow during
    the evaluation
*)

(* 6. Provide declarations of continuation-based, tail-recursive variants of both g and h.*)
let rec gc acc x c = match x with 
        |D(a,true) -> c (a::acc)
        |E(y,z) -> gc acc y (fun l -> gc acc z (fun r -> c(l@r)))
        | _ -> c acc;;


let hc x = gc [];;
gc [] (E(E(D(["a"],true),D(["b"],false)),D(["c"],true)) ) id
////////////Problem 3

(* 1. Declare a function flip: seq<’a*’b> -> seq<’b*’a>. The function flip transformsa sequence 
(a0, b0), (a1, b1), . . . , (ai, bi), . . . to the sequence (b0, a0), (b1, a1), . . . , (bi, ai), . . .*)

let flip a = if Seq.empty=a then Seq.empty else Seq.map (fun (a,b) -> (b,a) ) a;;

(* 2. Declare a function dia n, where n is a non-negative integer, that generates the sequence of pairs 
(0, n), (1, n − 1), . . . , (n − 1, 1), (n, 0). For example, dia 0 is a sequence containing just (0, 0), 
dia 2 is the sequence (0, 2), (1, 1), (2, 0) and dia 3 is the sequence (0, 3), (1, 2), (2, 1), (3, 0)*)

let rec dia n = Seq.init (n+1) (fun v -> (v,n-v))
dia 2

(* 3. Give a declaration of allCoordinates.*)
let allCoordinates = 
        let s =Seq.initInfinite (fun x -> if x%2=0 then dia x else flip (dia x))
        Seq.collect id s


/////////Problem 4
type T = Leaf of char | Branch of T*T
(* 1. Make an F# value for the tree t0 shown above and declare a function*)

let t0 = Branch(Leaf('a'),Branch(Leaf('b'),Leaf('c')))
let rec toList =function
        |Leaf(a) -> [a]
        |Branch(a,b) -> toList a @ toList b;;

toList t0

(* 2. Declare a function legal t that can check whether a tree t is legal.*)

let legal a = List.length (toList a)=List.length(List.distinct(toList a)) && List.length (toList a) >=2 
legal t0

(*3. Declare a function encode: CodingTable -> char list -> Code that gives the code for a list of characters for a given coding table. 
The function should raise an exception if the coding table does not contain a code for some character in the list.*)

type Dir = | L // go left
           | R // go right
type Code = Dir list
type CodingTable = Map<char, Code>

let rec encode ct cl = match cl with
    |[] -> []
    |x::rest when (Map.tryFind x ct) = None->  failwith "letter not found"
    |x::rest -> Map.find x ct @ encode ct rest;;

let m = Map [('a', [L]);('b', [R;L]);('c', [R;R])]

encode m ['c';'a';'a';'b']


(*4. Declare a function ofT: T -> CodingTable that gives the coding table for a tree.*)

let rec ofTH t ls = match t with 
    |Leaf(a) -> [(a,ls)]
    |Branch(a,b) -> (ofTH a (L::ls))@(ofTH b (R::ls));;

let ofT t = ofTH t [];;

(*5. Give declarations for the functions firstCharOf and decode.*)

let rec firstCharOf t ds = match t,ds with 
        |(Branch(a,b),L::rest)->firstCharOf a rest 
        |(Branch(a,b),R::rest)->firstCharOf b rest
        |(Branch(a,b),[])->failwith "No code to decode"
        |(Leaf(a),ds) -> (a,ds);;

firstCharOf t0 [R;R;L;L;R;L]

let  restoflist (a,b) = b;

let rec decodeH tree ds  = match ds with 
        |[] -> []
        |ds ->  (firstCharOf tree ds) ::(decodeH tree (restoflist (firstCharOf tree ds)) )
decodeH t0 [R;R;L;L;R;L]

let decode tree ds =
        let a= decodeH tree ds 
        List.map (fun(c,ls) -> c) a 
decode t0 [R;R;L;L;R;L]

/// 
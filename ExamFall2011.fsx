//problem 2

// type exp = | C of int
//            | BinOp of exp * string * exp;;

(*1. Give three different values of type exp.*)
let a= C(2);;
let b = BinOp(C(2),"+",C(5));;
let c = BinOp(BinOp(C(3),"-",C(1)),"+",BinOp(C(2),"+",C(3)));;

(*Delare a funtion toString: exp -> string, that gives a string representationfor an expression. Put brakets around every
subexpression with operators, e.g. (3+(5*2)) is a string representation of the above example.
*)

let rec toString = function
            |C(a) -> string a 
            |BinOp(a,b,c)-> "("+(toString a)+b+(toString c)+")";;
toString c

(*3. Delare a funtion to extrat the set of operators from an expression.*)

let rec extrat= function
            |C(a) -> Set.empty
            |BinOp(a,b,c)-> Set.union (Set.add b (extrat a)) (extrat c);;

extrat c

(*4. The type for expressions is now extended to include identifers (constructor  Id) and local defnitions ( constructor Def)*)

type exp =  | C of int
            | BinOp of exp * string * exp
            | Id of string
            | Def of string * exp * exp;;

(*Declare a function isDef: exp -> bool that an test whether an expression is defined.*)

let rec isDefH = function 
        |C(a)->Set.empty
        |BinOp(a,b,c) -> Set.union (isDefH a) (isDefH c)
        |Id(a) -> set [a]
        |Def(a,b,c) -> Set.union (isDefH b) (isDefH c)

let rec isDefHH = function
        |C(a)->Set.empty
        |BinOp(a,b,c) -> Set.union (isDefHH a) (isDefHH c)
        |Id(a) -> Set.empty
        |Def(a,b,c) -> Set.union (set[a]) (Set.union (isDefHH b) (isDefHH c))
let isDef a = 
    let d =Set.difference  (isDefH a) (isDefHH a) 
    (Set.count d) =0

isDefH c

isDef (Def("x", C 5, BinOp(Id "x", "+", Id "x")))
isDef (Def("x", C 5, BinOp(Id "y", "+", Id "x")))

isDef (Def("x", C 5, Def("Z", C 5, BinOp(Id "x", "+", Id "x"))))







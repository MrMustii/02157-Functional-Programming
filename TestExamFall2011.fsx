type name = string;;
type phone = int;;
type level = int;;
type desription = phone * level;;
type register = (name * desription) list;;

////1
let reg1 =[("Joe",(10101010,4));
           ("Sal",(11111111,2));
           ("Sam",(12121212,7));
           ("Jane",(13131313,1))];;

///2
let rec getPhone n reg = match reg with 
    |[]-> failwith("name not found")
    |(n1,(p,_))::rest when n1=n -> p
    |_::rest -> getPhone n rest;;

getPhone "Jo2e" reg1
///3 assumeing all names are uniqe

let rec delete (n,reg)= match reg with 
    |[]->[]
    |(n1,(_,_))::rest when n1=n -> delete (n,rest)
    |r::rest ->r::delete (n,rest);;
delete ("Joe",reg1)
//4
let rec getCanididates L reg = match reg with 
    |[]-> []
    |(n,(p,L'))::rest when abs(L'-L)<3 
                                    ->(n,p)::(getCanididates L rest)
    |_::rest->getCanididates L rest
getCanididates 5 reg1
////////////

//1
// type exp = | C of int
//            | BinOp of exp * string * exp;;
type exp1 = 
    | C of int
    | BinOp of exp1 * string * exp1
    | Id of string
    | Def of string * exp1 * exp1;;

let exp1 = C(1);;
let exp2 = BinOp(C(1),"+",C(2))
let exp3 = BinOp(BinOp(C(1),"+",C(2)),"*",
                BinOp(C(2),"+",C(4)));;

//2
let rec toString = function
    |BinOp(A,B,D) -> "("+toString A + B + toString D + ")"
    |C(A) -> string A;;
toString exp3
//3
let rec extractH exp = match exp with 
    |BinOp(A,B,D)->[B]@extractH A @ 
                        extractH D
    |C(A)->[];;
let extract exp = Set.ofList (extractH exp)

extract exp3
//4


let rec Defi exp = match exp with 
    |C(A)->C(A)
    |Id(A)->Id(A)
    |BinOp(A,B,D) -> BinOp((Defi A),B,(Defi D))
    |Def (x,A,B)-> match B with 
                    |Id(y) when y=x -> A
                    |Id(y) -> Id(y)
                    |Def(i,j,k) ->Defi (Def(i,j,k))
                    |BinOp(Id(i),j,Id(k)) when x=i && x=k -> BinOp(A,j,A)
                    |BinOp(Id(i),j,k) when x=i -> BinOp(A,j,(Defi k))
                    |BinOp(i,j,Id(k)) when k=x -> BinOp((Defi i),j,A)
                    |BinOp(i,j,k) -> BinOp((Defi i),j,(Defi k))
                    |C(A)-> Defi (C(A))
                    |Def(y,j,k) -> Defi (Def(y,j,k))

let rec isDef exp =match exp with
        |Id(A) ->false
        |C(A)->true
        |BinOp(A,B,D) -> (isDef A) && (isDef D)
        |Def(A,B,D)->isDef(Defi (Def(A,B,D)))

//////3
type 'a tree = | Lf
               | Br of 'a * 'a tree * 'a tree;;
let rec f(n,t) =match t with
    | Lf -> Lf
    | Br(a, t1, t2) -> if n>0 then Br(a, f(n-1, t1), f(n-1, t2))
                       else Lf;;
let rec g p = function
    | Br(a, t1, t2) when p a -> Br(a, g p t1, g p t2)
    | _ -> Lf;;
let rec h k = function
    | Lf -> Lf
    | Br(a, t1, t2) -> Br(k a, h k t1, h k t2);;

////
let rec DefH x v exp = match exp with 
    |C(A)->C(A)
    |BinOp(A,B,D) ->BinOp((DefH x v A),B,(DefH x v D))
    |Id(y) -> if x=y then v else Id(y)
    |Def(a,b,e) -> DefH a (DefH x v b) (DefH a b e);;
    
let rec isDef exp =  
    let exp1 = DefH "" (C(0)) exp
    match exp1 with 
        |C(a) -> true
        |BinOp(A,B,D) -> isDef A && isDef D
        |Id(a) -> false


isDef (Def("x", C 5, BinOp(Id "x", "+", Id "x")))
isDef (Def("x", C 5, BinOp(Id "y", "+", Id "x")))

DefH "" (C(0)) (Def("x", C 5, Def("Z", C 5, BinOp(Id "x", "+", Id "x"))))

DefH "" (C(0)) (Def ("x" ,C(5),Def("y", C 6, Def("Z", C 5, BinOp(Id "y", "+", Id "x")))))

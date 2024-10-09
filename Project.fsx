type Poly = int list;;
/////////////Poly Part 1 
///////////////////////////////////////////////////////////////// from part  2
/// is the function legal i.e. does not end in 0 
let rec isLegal ns = match ns with
    |[] -> true
    |n::ns when ns=[] ->if n=0 then false else true
    |n::ns ->isLegal ns;;

isLegal  [2; 3; 0; 1];; 
isLegal  [2; 3; 0; 1;0;0];; 
isLegal [];;
/// make a poly legal
let rec prune ns=match ns with
    |ns when isLegal ns -> ns
    |n::ns when ns=[]-> []
    |n::ns -> prune(n::prune ns);;

let rec oflistH = function 
    |[]-> []
    |0::xs ->oflistH xs
    |xs->xs;;

let oflist xs = List.rev (oflistH (List.rev xs)) ;;

prune  [2; 3; 0; 1];; 
oflist [2;3;0;1]
prune  [2; 3; 0; 1;0;0;0;0;0];; 
oflist  [2; 3; 0; 1;0;0;0;0;0];; 
oflist  [0];; 

//////////////////////////////////////////////////////////////////////////////
/// The add function that preserves the invariant 
let rec addHelper p q = match (p,q) with
    |([],q) -> q
    |(p,[]) ->p
    |(px::p,qx::q) -> (px+qx):: (addHelper p q);;

let add p q = prune (addHelper (prune p) (prune q));;

add [1;1;-3] [1;-1;3;3];;

/// the multiply by a constant function that preserves the invariant 
let rec mulCHelper p  q = match (p,q) with
    |(p,[]) -> []
    |(p,qx::q) -> (p*qx)::(mulCHelper (p) q ) ;;

let mulC p q = prune (mulCHelper p (prune q));;

mulC 2 [1;-1;3;3;0];;
mulC 0 [1;-1;3;3;0];;
/// the subtract function that preserves the invariant 
let rec subHelper p q = match (p,q) with
    |([],q) -> mulC -1 q
    |(p,[]) ->p
    |(px::p,qx::q) -> (px-qx):: (subHelper p q);;

let sub p q= prune (subHelper (prune p)(prune q));;
sub [2;3;0;2;0] [4;2;5;2]
/// multiply by x  that preserves the invariant 
let  mulX q =  0::(prune q);;
mulX[1;0;0;9;0];;

/// multiply 2 poly function with eachother that preserves the invariant 
let rec mulHelper p q = match (p,q) with 
    |([],q) -> [0]
    |(px::p,q) -> add (mulC px q) (mulHelper p (mulX q));;
let mul p q=prune(mulHelper (prune p)(prune q))

mul [2;3;0;1] [1;2;3];;
//[2,7,12,10,2,3]
mul [2;3;0;1] [1;2;3;0];;
mul [0] [1;2;3;0];;
mul [2;3;0;1] [0];;

/// evaluate the poly by setting x=p that preserves the invariant 
let rec evalhelper p q =  
    match (p,q) with 
    |(p,[])->0 
    |(p,qx::q) -> (qx*(pown p (q.Length))) + (evalhelper p  q) ;;

let eval p q=
    evalhelper p (List.rev (prune q));;
    

eval 2 [2; 3; 0; 1;0;0];; 
//////////////////////// Poly part 2
/// write the poly as a string that preserves the invariant 
let rec tostringhlper ns = match ns with
    |[] -> ""
    |n::ns when ns<>[]&&n=0->  tostringhlper ns
    |n::ns when ns<>[] && n=1 ->  " x^" + string ns.Length + " + " + tostringhlper ns
    |n::ns when ns<>[]-> (string n) + " x^" + string ns.Length + " + " + tostringhlper ns
    |n::ns ->string n;;


let tostring ns=
    tostringhlper (List.rev (prune ns));;

tostring [1;2];;

tostring [2; 3; 0; 1;0];; 
/// returns the derivative of a poly that preserves the invariant 
let rec derivativeHelper p= match p with 
    |p::[]->[]
    |p::prest -> (p*prest.Length)::derivativeHelper prest;;

let rec derivative p=
    prune (List.rev (derivativeHelper (List.rev (prune p))));

derivative [2; 3; 0; 1;0];; 

///returns the poly to the power of p that preserves the invariant 
let rec mulPHelper p q = match (p,q) with
    |(0,q)->[1]
    |(1,q) -> q
    |(p,q) -> mul q (mulPHelper (p- 1) q);;
let mulP p q = mulPHelper p (prune q)
mulP 3 [3;3;3;0];;
mul [3;3;3] (mul [3;3;3] [3;3;3]);;
/// inserts a poly in the other so that p(q(x)) 
let rec composeHelper p q = match (p,q) with
    |([],q) ->q
    |(p,[] ) -> [List.head (List.rev p)]
    |(p::[],q::qrest) ->  [p]
    |(p::prest,q) when prest <> []-> add (mulC p (mulP (prest.Length) q)) (composeHelper prest q);;


let compose p q = composeHelper (List.rev (prune p)) (prune q);;
compose [2; 0; 0; 4] [0; 3; 2];;
compose [2;0; 3;0] [];;
compose [] [2;0; 3;0];;
//[2; 0; 0; 108; 216; 144; 32]

/////////////////////
/// 
/// 
let rec findPosA p x =
function
| y::_ when x=y -> Some p
| _::ys -> findPosA (p+1) x ys
| [] -> None;;

let findPos x ys = findPosA 0 x ys;;
/////////////////////////POLY PART 4 
/// Declare a type Degree having two kinds of values
type Degree = |MinusInf 
              |Fin of int;;

let degree = function
    |[] ->MinusInf
    |x -> Fin (x.Length-1);;


degree [1;1;1]<=degree [1;2];;


let addD a b  = match (a,b) with
    |(_,MinusInf)->MinusInf
    |(MinusInf,_)->MinusInf
    |(Fin a,Fin b)->Fin(a+b);;


addD (Fin 7) (Fin 8);;

/////////////
//Part 5
#r "nuget: FsCheck";;
open FsCheck;;
// associative add
let assAdd p1 p2 p3 = 
    add (add p1 p2 ) p3 = add p1 (add p2 p3);;

let _ = Check.Quick assAdd;;
// commutative add
let commAdd p1 p2 =
    add p1 p2 = add p2 p1;; 

let _ = Check.Quick commAdd;;
//Zero is called the additive identity
let addiden p1 =
    add p1 [] =prune p1;;

let _ = Check.Quick addiden;;

//−p is called the additive inverse of p
let addinverse p1 =
    add p1 (mulC -1 p1) =[];;
let _ = Check.Quick addinverse;;
//(mul) is associative
let assmul p1 p2 p3 =
    mul (mul p1 p2) p3 = mul p1 (mul p2 p3);;

let _ = Check.Quick assmul;;
// mul is commutative
let commmul p1 p2 =
    mul p1 p2 = mul p2 p1;;
let _ = Check.Quick commmul;;

//One is called the multiplicative identity
let muliden p1 =
    mul p1 [1] =prune p1;;
let _ = Check.Quick muliden;;

//multiplication is distributive with respect to addition 
let mulladd p1 p2 p3 =
    mul p1 (add p2 p3) = add (mul p1 p2) (mul p1 p3);;
let addmull p1 p2 p3 =
    mul (add p2 p3) p1 = add (mul p2 p1) (mul p3 p1);;

let _ = Check.Quick mulladd;;
let _ = Check.Quick addmull;;

let Zero = [];;
let One =[1];;
let addinv p=
            mul p [-1];;

//The composition function is associative
let assCompose p1 p2 p3 = 
    compose (compose p1 p2 ) p3 = compose p1 (compose p2 p3);;
    

compose (compose [0; 0; 0; 0; 4] [] ) []
compose [0; 0; 0; 0; 4] (compose [] []);;
let _ = Check.Quick assCompose
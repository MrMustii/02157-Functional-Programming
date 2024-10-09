// 4.1 Declare function upto: int -> int list such that upto n = [1; 2; ... ; n]
let upto x = [1..x];;
upto 5;;

// 4.2 Declare function downto1: int -> int list such that the value of downto1 n is the list [n; n − 1; ... ; 1].
let downto1 x = [x .. -1 .. 1];;
downto1 6;;

//4.3 Declare function evenN: int -> int list such that evenN n generates the list of the first n non-negative even numbers.

let evenN (x: int) = match x with
    |(0) ->[0]
    |(x) -> [0..2..((x*2)-2)];;

evenN 15;;
//4.5 Declare an F# function rmodd removing the odd-numbered elements from a list: rmodd [x0;x1;x2;x3; ... ] = [x0;x2; ... ]
let rec rmodd x=match x with
    |[] -> []
    |xs::x when xs%2<>0 ->  [xs]::(rmodd x)
    |xs::x ->   rmodd x;;
rmodd [1;2;3;4;5;6;7;8;9;9;9;9;9];;

// 4.6 Declare an F# function to remove even numbers occurring in an integer list.
let rec removeEven x=match x with
    |[] -> []
    |xs::x when xs%2<>0 -> xs::removeEven x
    |xs::x when xs%2=0 -> removeEven x;;
    
removeEven [1;2;3;4;5;6;7;8;9;9;9;9;9];;

// 4.8 Declare an F# function split such that: split [x0;x1;x2;x3; ... ;xn−1] = ([x0;x2; ... ], [x1;x3; ... ])
let rec split x = match x with 
    |[] ->([],[])
    |x1::[]->([x1],[])
    |x1::x2::rest ->
              let  (a,b) =split rest 
              (x1::a,x2::b);;

split [1;2;3;4;5;6;7;8;9;9;9;9;99];;

let rec sumProd = function
    | [] -> (0,1)
    | x::rest ->
        let (rSum,rProd) = sumProd rest
        (x+rSum,x*rProd);;
sumProd [1;2;3;4;5;6;7;8;9;9;9;9;9];;


//4.9 Declare an F# function zip such that: zip([x0;x1; ... ;xn−1],[y0;y1; ... ;yn−1]) = [(x0, y0);(x1, y1); ... ;(xn−1, yn−1)]
let rec zip (x, y) = match x,y with
    |[],[]->[]
    |x::[],y::[] -> [(x,y)]
    |x::xrest,y::yrest ->  (x,y)::zip (xrest,yrest);;
zip ([1;2;3;4;5],[10;9;8;7;6]);;

//4.10 Declare an F# function prefix: ’a list -> ’a list -> bool when a : equality.
//The value of the expression prefix [x0;x1; ... ;xm] [y0;y1; ... ;yn] is true if m ≤ n and xi = yi for 0 ≤ i ≤ m, and false otherwise
let rec prefix  x y = match x,y with
    |[],y -> false
    |x,[] -> true
    |x::xrest,y::yrest -> if x=y then prefix xrest yrest else false;;
prefix [1;2;3;4;5] [10;9;8;7;6];;
prefix [1;2;3;4],[10;9;8;7;6];;
prefix [1;2;3;4;5],[1;2;3;4;5;6];;




//////////////////4.11/////////////////////////////
// 1. Declare an F# function count: int list * int -> int, where count(xs, x) is the 
// number of occurrences of the integer x in the weakly ascending list xs.

let rec count (x,xs) = match x,xs with
    |(x,[]) -> 0
    |(x,xs::xrest) when x=xs->1+count (x,xrest)
    |(x,xs::xrest) ->  count (x,xrest);;

count (5,[5;5;5;5;1;1;5])



// 2. Declare an F# function insert: int list * int -> int list, where the value of
// insert(xs, x) is a weakly ascending list obtained by inserting the number x into the weakly
// ascending list xs.

let rec insert (xs: int list,x:int) = match xs,x with
    |([],x) -> [x]
    |(xs::xrest,x) when x<=xs-> x::xs::xrest
    |(xs::xrest,x) -> xs::insert(xrest,x);;
insert ([5;6;7;7;8],5);;
insert ([1;2;3;4;5;6;7;7;8],5);;
insert ([1;2;3;4],5);;

// 3. Declare an F# function intersect: int list * int list -> int list, where the
// value of intersect(xs, xs) is a weakly ascending list containing the common elements of the weakly ascending lists xs and x.
// For instance: intersect([1;1;1;2;2], [1;1;2;4]) = [1;1;2]


let rec intersect (xs,ys)=match xs,ys with
    |([],ys) -> []
    |(xs,[]) -> []
    |(xs::xrest,ys::yrest) when count(xs,ys::yrest)>0 -> xs::intersect(xrest,yrest)
    |(xs::xrest,ys::yrest) -> intersect(xrest,ys::yrest);;
intersect([1;1;1;2;2], [1;1;2;4]);;
intersect([1;1;2;4],[1;1;1;2;2]);;

// 4. Declare an F# function plus: int list * int list -> int list, where the value ofplus(xs, xs) 
//is a weakly ascending list, that is the union of the weakly ascending lists xs and xs. For instance:
//plus([1;1;2],[1;2;4]) = [1;1;1;2;2;4]

let rec plus (xs,ys)=match xs,ys with
    |(xs::[],ys::yrest)->insert(ys::yrest,xs)
    |(xs::xrest,ys::yrest)-> plus (xrest,insert(ys::yrest,xs));;

plus([1;1;2],[1;2;4]);;

// 5. Declare an F# function minus: int list * int list -> int list, where the value of minus(xs, xs) is a weakly ascending list
// obtained from the weakly ascending list xs by removing those elements, that are also found in the weakly ascending list xs. 
//For instance: minus([1;1;1;2;2],[1;1;2;3]) = [1;2] minus([1;1;2;3],[1;1;1;2;2]) = [3]

let rec remove (x,xs: int list)=match x,xs with
    |(x,[]) -> []
    |(x,xs::xrest) when xs=x -> xrest
    |(x,xs::xrest)-> xs::remove (x,xrest);;

remove (5,[5;5;6;7;5;7;8]);;
remove (5,[1;2;3;4;5;6;7;7;8]);;
remove (5,[1;2;3;4]);;


let rec minus (xs,ys)=match xs,ys with
    |(xs,[]) ->xs
    |(xs::xrest,ys::yrest) ->minus(remove(ys,xs::xrest),yrest);;

minus([1;1;1;2;2],[1;1;2;3]);;
minus([1;1;2;3],[1;1;1;2;2])

////////////////////////////////////////////////////////
// 4.12 Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list ofintegers.
// The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.Test the function on different predicates
let sum (p,xs)=





// HR 4.16, 4.17 hand
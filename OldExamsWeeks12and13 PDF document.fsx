type Title = string;;
type Section = Title * Elem list
    and Elem = Par of string | Sub of Section;;
type Chapter = Title * Section list;;
type Book = Chapter list;;

let sec11 = ("Background", [Par "bla"; Sub(("Why programming", [Par "Bla."]))]);;
let sec12 = ("An example", [Par "bla"; Sub(("Special features", [Par "Bla."]))]);;
let sec21 = ("Fundamental concepts",
                [Par "bla"; Sub(("Mathematical background", [Par "Bla."]))]);;
let sec22 = ("Operational semantics",
            [Sub(("Basics", [Par "Bla."])); Sub(("Applications", [Par "Bla."]))]);;
let sec23 = ("Further reading", [Par "bla"]);;
let sec31 = ("Overview", [Par "bla"]);;
let sec32 = ("A simple example", [Par "bla"]);;
let sec33 = ("An advanced example", [Par "bla"]);;
let sec41 = ("Status", [Par "bla"]);;
let sec42 = ("What’s next?", [Par "bla"]);;
let ch1 = ("Introduction", [sec11;sec12]);;
let ch2 = ("Basic Issues", [sec21;sec22;sec23]);;
let ch3 = ("Advanced Issues", [sec31;sec32;sec33]);;
let ch4 = ("Conclusion", [sec41;sec42]);;
let book1 = [ch1; ch2; ch3; ch4];;


(*1. Declare a function maxL to find the largest integer occurring in a list with non-negative
integers. The function must satisfy maxL [] = 0.*)

let maxL = function
    |ls when List.length ls = 0-> 0
    |ls -> List.max ls;;

(*Declare a function overview to extract the list of titles of chapters from a book. For
example, the overview for book1 is*) 

let rec overview = function
    |[] -> []
    |(a,b)::rest ->a::overview rest  

overview book1

(*
    3. Declare functions:
        depthSection: Section -> int
        depthElem: Elem -> int
        depthChapter: Chapter -> int
        depthBook: Book -> int
*)





let rec depthSectionH  a c=  match a,c with 
        |([],c) -> c
        |(ls1::rest,c) -> if (depthElemH ls1 (c))> depthSectionH rest c then depthElemH ls1 (c) else depthSectionH rest (c+1)
and depthElemH elem c = match elem,c with 
        |Par(a),c -> c
        |(Sub(a,b),c)-> depthSectionH b (c)


let depthSection a = 
        let (x,y) = a 
        depthSectionH y 0;

let depthElem elem =  
        depthSectionH elem 0;

let rec depthChapter = function 
    |(_,[])-> 0
    |(q,s::rest)-> maxL((depthSection s)+1::depthChapter (q,rest)::[]);;

let rec depthBook = function 
    |[]-> 0
    |s::rest-> maxL((depthChapter s)::depthBook (rest)::[]);;

depthBook book1
depthSection sec23
depthChapter ch3

type Numbering = int list;;
type Entry = Numbering * Title;;
type Toc = Entry list;;
(*4. Declare a function, tocB: Book → Toc, to compute the table of contents for a book.*)


let rec tocsection c sls = match sls with 
    |[] -> []
    |s::rest -> tocelement c s @ tocsection c rest
and tocelement c elm = match elm with 
    |Par(_) -> []
    |Sub(a,b)->(c@[1],a)::tocsection c b;;
//see phone


tocsection [1] [Sub(("Basics", [Par "Bla."])); Sub(("Applications", [Par "Bla."]))]
////////////////
let rec f xs ys = match (xs,ys) with
        | (x::xs1, y::ys1) -> x::y::f xs1 ys1
        | _ -> [];;
//1. Give an evaluation (using ;) for f [1;6;0;8] [0; 7; 3; 3] thereby determining the value of this expression.

(*
    =>1::0::(f [6;0;8] [7;3;3])
    =>...
    =>[1;0;6;7;0;3;8;3]
*)

(*3. The declaration of f is not tail recursive. Give a brief explanation of why this is the case and provide a declaration of a 
tail-recursive variant of f that is based on an accumulating parameter. Your tail-recursive declaration must be based on an explicit recursion*)
let rec tailf xs ys c = match (xs,ys) with 
        | (x::xs1, y::ys1) -> x::y::c@ (tailf xs1 ys1 c)
        | _ -> c;;
f [1;6;0;8] [0; 7; 3; 3]
//////////////
let rec f = function
    | 0 -> [0]
    | i when i>0 -> i::g(i-1)
    | _ -> failwith "Negative argument"
and g = function
    | 0 -> []
    | n -> f(n-1);;

let h s k = seq { for a in s do 
                    yield k a };;

(*1. Give the values of f 5 and h (seq [1;2;3;4]) (fun i -> i+10). Furthermore, give the (most general) types for f and h, and describe what each 
of these two functions computes. Your description for each function should focus on what it computes, rather than on individual computation steps*)

(* f and g gives every other number [5;3;1]. h  adds 10 to the sequance [11;12;13;14]*)

/////
type Container =
    | Tank of float * float * float // (length, width, height)
    | Ball of float // radius

(*1. Declare two F# values of type Container for a tank and a ball, respectively.*)
let b = Ball(3.4)
let t = Tank (2.0,14.8,7.9);;

(*2. A tank is called well-formed when its length, width and height are all positive and a ball is well-formed when its radius is positive. 
Declare a function isWF : Container → bool that can test whether a container is well-formed*)

let isWF =function
    |Ball(a) when a >0 -> true
    |Tank(a,b,c) when a>0 && b>0 && c>0 -> true 
    |_ -> false
isWF t

(*Declare a function volume c computing the volume of a container c. (Note that the volume of ball with radius r is 4/ 3 · π · r3.)*)

let volume =function
    |Ball(a) -> (4.0/3.0)*3.14*a**3
    |Tank(a,b,c) -> a*b*c;;

volume t

(*4. Extend the declaration of the type Container so that it also captures cylinders, and extend the functions isWF and volume accordingly. 
(Note that the volume of cylinder with radius r and height h is π · r2 · h.)*)

type newContainer =
    | Tank of float * float * float // (length, width, height)
    | Ball of float 
    |Cylinder of float*float

let isWFnew =function
    |Ball(a) when a >0 -> true
    |Tank(a,b,c) when a>0 && b>0 && c>0 -> true 
    |Cylinder(a,b) when a>0 && b>0  -> true 
    |_ -> false
isWF t


type Name = string
type Contents = string
type Storage = Map<Name, Contents*Container>

(*5. Declare a value of type Storage, containing a tank with name "tank1" and contents "oil" and a ball with name "ball1" and contents "water"*)

let m=Map ["tank1",("oil",Tank(1.2,2.2,3.2));"ball1",("water",Ball(3.5))]

(*Declare a function find : Name → Storage → Contents ∗ float, where find n stg should return the pair (cnt, vol) when cnt is the contents of a 
container with name n in storage stg, and vol is the volume of that container. A suitable exception must be raised when no container has name n in
storage stg.*)


let findh n stg= Map.tryFind n stg 

let find n stg = match stg with 
    |stg when (findh n stg) = None -> failwith "not found"
    |stg -> stg[n]

find "tank1" m
///////////////////////////////////////////////////
type T<'a> = L | N of T<'a> * 'a * T<'a>
let rec f g t1 t2 =
        match (t1,t2) with
        | (L,L) -> L
        | (N(ta1,va,ta2), N(tb1,vb,tb2))
                -> N(f g ta1 tb1, g(va,vb), f g ta2 tb2);;
let rec h t = match t with
        | L -> L
        | N(t1, v, t2) -> N(h t2, v, h t1);;
let rec g = function
        | (_,L) -> None
        | (p, N(t1,a,t2)) when p a -> Some(t1,t2)
        | (p, N(t1,a,t2)) -> match g(p,t1) with
        | None -> g(p,t2)
        | res -> res;;
let t = N(N(L, 1, N(N(L, 2, L), 1, L)), 3, L);;

let rec count n = function
    |L -> 0
    |N(a,l,b) when n=l -> 1+(count n a)+ (count n b)
    |N(a,l,b) -> (count n a)+ (count n b) 

count 2 t

let rec replace a b t= match t with
        |L -> L
        |N(x,l,y) when l =a -> N((replace a b x),b,(replace a b y))
        |N(x,l,y) -> N((replace a b x),l,(replace a b y))

replace 1 0 t
 let set = Set.empty.Add(1).Add(1).Add(2)
 
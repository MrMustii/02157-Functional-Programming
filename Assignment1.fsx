//Name : Mustafa El-Madani
//Student Number: s215225
///////////////////////////Problem 1
#r "nuget: FsCheck";;
open FsCheck;;


type Book = string
type Shelf = Book list // ordered alphabetically
type Date = int
type Name = string
type Loan = Book * Name * Date

let sh0 = ["Introduction to meta-mathematics";
"To mock a mockingbird";
"What is the name of this book";"AAAAAAAAAAAAAAAAAAAAAA"];;

let ls0 = [("Communication and concurrency", "Bob", 4);
("Programming in Haskell", "Paul", 2);
("Communicating Sequential processes", "Mary", 7);
("Elements of the theory of computation", "Dick", 1)];;

(*1. . Declare a function onShelf: Book -> Shelf -> bool that can check whether a book is on a shelf.*)

let rec onShelf b sh = match b,sh with 
            |(b,[])->false
            |(b,sh::rest) when b= sh-> true
            |(b,sh::rest) -> onShelf b rest ;;


(*2. Declare a function toShelf: Book -> Shelf -> Shelf so that toShelf b bs is the shelf obtained from bs by insertion of b in the right position.*)

let rec toShelf b sh = match b,sh with
        |(b,[])-> [b]
        |(b,sh::rest) when b<=sh ->b::sh::rest
        |(b,sh::rest) when b>sh ->sh::toShelf b rest;;
toShelf "Zombie Book" sh0
(*3. Declare a function fromShelf: Book -> Shelf -> Shelf option. The value of the expression fromShelf b bs is None if bs does not contain b. Otherwise, the value is
Some bs0, where bs0 is obtained from bs by deletion of one occurrence of b.*)

let rec fromShelf b sh = match b,sh with 
        |(b,[]) ->None
        |(b,sh::rest)when b=sh-> Some rest
        |(b,sh::rest) -> Some(sh::Option.get((fromShelf b rest)));;

let rec fromShelf1 b sh = match sh with 
    | [] -> None
    | sh1 :: stail when b=sh1-> Some stail
    | sh1 :: stail ->
        match fromShelf1 b stail with 
        | Some rest -> Some (sh1::rest)
        | None -> None 

(*4. Declare a function addLoan b n d ls, that adds the loan (b, n, d) to the list of loans ls.*)

let addLoan b n d ls =
            (b,n,d)::ls;;
(*
Furthermore, declare a function removeLoan b n ls. The value of the function is the list
obtained from the list of loans ls by deletion of the first element of the form (b, n, d),
where d is some date, if such an element exists. Otherwise ls is returned
*)
let rec removeLoan b n ls = match b,n,ls with 
            |(b,n,[])->[]
            |(b,n,(b1,n1,_)::rest) when b=b1 && n1=n ->rest
            |(b,n,ls1::rest) ->ls1::removeLoan b n rest;;
(*5. Declare a function reminders: Date -> Loan list -> (Name * Book) list. The value of reminders d0 ls is a list of pairs (n, b) from loans (b, n, d) in ls where d < d0.
  We interpret d < d0 as “date d is before date d0”.*)

let rec reminders d ls = match d, ls with 
    |(d,[])-> []
    |(d,(b,n,dd)::rest) when dd<d ->(n,b)::reminders d rest
    |(d,(b,n,dd)::rest) ->reminders d rest;;

//6. Declare a function toLetters: (Name * Book) list -> string list, that transforms a list pairs (n, b) to a list of corresponding strings (letters). Notice, the escape 
// characters \n and \" denote newline and citation quotation, respectively.*)



let rec toLetters ls = match ls with 
        |([]) -> []
        |((n,b)::rest) -> ("Dear "+n+"\nPlease return \""+b+"\".\n Regards Robin")::toLetters rest ;;


(*7. This question should be solved using functions from the List library. You should not
use explicit recursion in the declarations.*)

let toLetters2 ls= 
    List.map (fun (x,y) -> "Dear "+x+"\nPlease return \""+y+"\".\n Regards Robin") ls;;


let reminders2 d ls =
        List.foldBack (fun (b,n,dd) s -> if dd<d then (n,b)::s else s ) ls [];;





///////////////////////////Problem 2
(*1. Give an argument showing that ’a -> ’b list -> (’a * ’b) list is indeed the most general type of f and that ’a list -> ’b list -> (’a * ’b) list is indeed
the most general type of allPairs. *)
let rec f x = function
    | [] -> []
    | y::ys -> (x,y)::f x ys;;
let rec allPairs xs ys =
    match xs with
    | [] -> []
    | x::xrest -> f x ys @ allPairs xrest ys;;
(*
    The function f takes 2 inputs as seen form the second pattern matching and outputs a list. Moreover the second input is a list of any type (y) and the first is a value of any type(x).
    Furthermore, we can see the function takes x and each element of y to make a a list of tuples. therefore ’a -> ’b list -> (’a * ’b) list  is the general type of f.

    The function allpairs takes 2 inputs. the first is a list (xs) we can see from the pattern matching and the second is also a list since it is the second input of the f function.
    The output of the function is a list, the same output of f. the function allPairs takes each element of xs and the list ys and apply the function of on them to append them.
     therefore, the output of allPairs must be the same as f and the inputs are any type of 2 lists.  
*)

(*2. Give an evaluation showing that [("a", 1); ("a", 2); ("a", 3)] is the value of the expression f "a" [1;2;3]. Present your evaluations using the notation e1 e2 from
the textbook, where you can use => in your F# file rather than . You should include at least as many evaluation steps as there are recursive calls*)

(*
    f "a" [1;2;3]
    =>("a",1)::(f "a" [2;3])
    =>("a",1)::(("a",2)::(f "a" [3]))
    =>("a",1)::(("a",2)::(("a",3)::(f "a" [])))
    =>("a",1)::(("a",2)::(("a",3)::([])))
    =>("a",1)::(("a",2)::([("a",3)]))
    =>("a",1)::([("a",2);("a",3)])
    =>[("a", 1); ("a", 2); ("a", 3)]
*)
(*3.Explain why the type of f "a" [1;2;3] is (string * int) list.*)

(*
    Since the first input is a string, the second input is a list of integers the function f will take the first input and each element of the second input and make a tuple list 
    therefore the output is of type  (string * int) list. 
*)


(*4. Give another declaration of f that is based on a single higher-order function from the List library. The new declaration of f should not be recursive.*)

let f2 x y =
    List.map(fun a->(x,a)) y;;


///////////////////////////Problem 3

(*1. Give the type for f and describe what f computes. Your description should focus on what it computes, rather than on individual computation steps.*)

type T = | One of int | Two of int * T * int * T
let rec f p t =
    match t with
        | One v when p v -> [v]                     (* C1 *)
        | Two(v1,t1,_,_) when p v1 -> v1::f p t1    (* C2 *)
        | Two(_,_,v2,t2) -> v2::f p t2              (* C3 *)
        | _ -> [];;                                 (* C4 *)

let t1 = Two(1,One(1),1,One(3))
let t2 = Two(3,One(2),2,One(3))
let t3 =Two(2,Two(1,t1,2,t2),3,t2);;
f (fun x -> x%2=0) t2

(*
    The function f takes 2 inputs, p and t, t is of the defined T type which is a tree of integers and p is a function that takes an int and return a bool and the output of f must be a list 
    of int as seen in c1. Therefore the type of f is (int -> bool) -> T -> int list.

    The function is a search function that finds all the integers in one branch of the tree t that satisfy the predicate p and collects them in a list. 
    If the first branch does not have any values that satisfy p ,then the adjacent branch is selected and this branch is ignored. 
    If no branch can satisfy p then the output is an empty list.
*)

(*2. Give a small number (≤ 4) of test descriptions for f. Together they should ensure thatevery clause of f is selected during an evaluation.*)

(* Test 1 the expected value is [2] and the selected clauses are : C1 *)
let p1 a = a%2=0;; 
let t1 = One(2)
f p1 t1;;

(* Test 2 the expected value is [] and the selected clauses are : C4 *)
let p2 a = a%2=0;; 
let t2 = One(1)
f p2 t2;;

(* Test 3 the expected value is [2;4] and the selected clauses are : C1 and C2*)
let p3 a = a%2=0;; 
let t3 = Two(2,One(4),5,One(7))
f p3 t3;;

(* Test 4 the expected value is [2;4] and the selected clauses are : C1 and C3 *)
let p4 a = a%2=0;; 
let t4 = Two(5,One(7),2,One(4))
f p4 t4;;


///////////////////////////Problem 4
//Part 1
type Instruction = | ADD | SUB | SIGN | ABS | PUSH of int;;
type Stack = int list;;
let intpInstr stack Inst = match  Inst with 
                |ADD ->match stack with 
                        a::b::rest -> b+a::rest
                |SUB -> match stack with 
                        a::b::rest -> b-a::rest
                |SIGN -> match stack with 
                        |a::rest  when a>0 -> -1*a::rest
                        |a::rest  when a<=0 -> a::rest
                |ABS -> match stack with 
                        |a::rest  when a>=0 ->      a::rest
                        |a::rest  when a<0 -> -1*a::rest
                |PUSH(r) -> match r,stack with 
                         (r,stack) -> r::stack;;


intpInstr [1;2;3;3;3;3] ADD ;;



let rec exechelper inst = match inst with
                        |[]      -> []
                        |a::rest -> intpInstr (exechelper rest) a 

let rec exec inst = 
            let a = exechelper (List.rev (inst))
            List.head a ;;

exec [PUSH(2);PUSH(6);PUSH(1);SIGN;ADD]


//Part 2
type expression = |C of int
                  |Add of expression*expression |Sub of expression*expression |Minus of expression |Abs of expression
                  |X ;;


let rec ctoint a = match a with  
            |(C(a')) -> a'
            |(Add(C(a'),C(b'))) -> a'+b' ;;
            

let rec sem e x = match e with 
                    |Add(a,b)-> match a,b with 
                                |(C(a'),C(b')) ->  a'+b'
                                |(X,b) ->  sem (Add(C(x),b)) x
                                |(a,X) ->  sem (Add(a,C(x))) x
                                |(a,b) ->  sem a x + sem b x 
                    |Sub(a,b)->  match a,b with 
                                |(C(a'),C(b')) ->  a'-b'
                                |(X,b) ->  sem (Sub(C(x),b)) x
                                |(a,X) ->  sem (Sub(a,C(x))) x
                                |(a,b) ->  sem a x - sem b x 
                    |Minus(a)-> match a with 
                                |X -> sem (Minus(C(x))) x
                                |C(a) -> if a >=0 then a*(-1) else a
                                |a -> sem (Minus(C(sem a x))) x
                    |Abs(a)  -> match a with 
                                |X-> sem (Abs(C(x))) x
                                |C(a) -> if a<=0 then a*(-1) else a
                                |a -> sem (Abs(C(sem a x))) x
                    |C(a)-> a
                    |X -> x;; 
//if it does not work restart f# interactive.

///sem (Abs (Minus X)) 2;
/// Abs (Minus X) 2
/// Minus X 2
/// Minus 2
/// -2


let a =C(1);;
let b =C(1);;
let c = Sub(Add(Add(C(2),C(2)),a),b);;

sem c 3


//Part 3 

let rec compile e x = match e with 
                |Add(a,b)-> match a,b with 
                            |(C(a'),C(b')) ->  PUSH(a')::PUSH(b')::[ADD]
                            |(X,b) ->  compile (Add(C(x),b)) x
                            |(a,X) ->  compile (Add(a,C(x))) x
                            |(a,b) ->  compile a x @ compile b x @[ADD]
                |Sub(a,b)->  match a,b with 
                            |(C(a'),C(b')) ->  PUSH(a')::PUSH(b')::[SUB]
                            |(X,b) ->  compile (Sub(C(x),b)) x
                            |(a,X) ->  compile (Sub(a,C(x))) x
                            |(a,b) ->  compile a x @ compile b x @ [SUB] 
                |Minus(a)-> match a with 
                            |X -> compile (Minus(C(x))) x
                            |C(a) -> if a >=0 then PUSH(a*(-1))::[SIGN] else PUSH(a)::[SIGN]
                            |a -> compile a x @ [SIGN]
                |Abs(a)  -> match a with 
                            |X-> compile (Abs(C(x))) x
                            |C(a) -> if a<=0 then PUSH(a*(-1))::[ABS] else PUSH(a)::[ABS]
                            |a -> compile a x @ [ABS]
                |C(a)-> [PUSH(a)]
                |X->[PUSH(x)];;

//(5+1)+(2+6)
let d=Add(C(5),X);;
let e =Sub(X,C(6));;
compile c 3;;


let checkcom e x = 
        exec (compile e x) = sem e x;;

exec (compile (X) 11);;
sem (X) 11 ;;

let _ = Check.Verbose checkcom;;
///////////problem 1 3 
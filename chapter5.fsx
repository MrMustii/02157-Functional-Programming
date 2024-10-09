// 5.1 Give a declaration for List.filter using List.foldBack.


// 5.2 Solve Exercise 4.15 using List.fold or List.foldBack
// 4.15 Declare an F# function revrev working on a list of lists, that maps a list to the reversed list of
// the reversed elements, for example: revrev [[1;2];[3;4;5]] = [[5;4;3];[2;1]]

let revrev s =
        List.fold (fun c x -> (List.rev x)::c) [] s;;

revrev [[1;2];[3;4;5]] 
//[[5;4;3];[2;1]]

// 5.3 Solve Exercise 4.12 using List.fold or List.foldBack.

// 4.12 Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of integers.
// The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
// Test the function on different predicates (e.g., p(x) = x > 0).
let sum (p,xs)=
    List.fold (fun s x-> if p x then  x+s else s) 0 xs;;

sum ((fun x -> x%2=0),[1..10]);;


// 5.4 Declare a function downto1 such that:
// downto1 fne = f(1, f(2,...,f(n−1, f(n, e)) ...)) for n > 0
// downto1 fne = e for n ≤ 0
//Declare the factorial function by use of downto1. Use downto1 to declare a function that builds the list [g(1), g(2),...,g(n)] for a function g
//and an integer n

let downto1 f n e =
    if n<=0 then e 
    else 
    List.foldBack (fun x n -> f x+n) n e ;;


// 5.6 We define a relation from a set A to a set B as a subset of A × B. A relation r is said to be
// smaller than r, if r is a subset of r, that is, if r ⊆ r. A relation r is called finite if it is a
// finite subset of A × B. Assuming that the sets A and B are represented by F# types ’a and ’b
// allowing comparison we can represent a finite relation r by a value of type set<’a * ’b>.



////5.4, 5.1 

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let rec fact n =
if n=0 then 1
else n * fact(n-1);;


(*1.4 Declare a recursive function f: int -> int, where
f(n)=1+2+ ··· + (n − 1) + n*)
let rec f = function
    |0->0
    |n->n+f(n-1);;


(*1.6 Declare a recursive function sum: int * int -> int, where
sum(m, n) = m + (m + 1) + (m + 2) + ··· + (m + (n − 1)) + (m + n)*)

let rec sum=function
    |(m,0)->m
    |(m,n)->m+n+sum(m,(n-1));;


let rec Fibo = function
    |0->0
    |1->1
    |n->Fibo(n-1)+Fibo(n-2);;
Fibo(10);;
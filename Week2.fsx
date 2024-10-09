//1.5 Fibonacci number
let rec Fn = function
    |0->0
    |1->1
    |n->Fn(n-1)+Fn(n-2);;


// 2.1 Declare a function f: int -> bool such that f(n) = true exactly when n is divisible by 2 or divisible by 3 
// but not divisible by 5. Write down the expected values of f(24), f(27), f(29) and f(30) 
//and compare with the result. Hint: n is divisible by q when n%q = 0.

let f  = function
    |a when (a%2=0 || a%3=0) && a%5<>0 -> true 
    |a-> false;;

f 27;;

// 2.2 make an opration that will repeat a string 

let rec pow  = function
    |(s,0) -> ""
    |(s,1) -> s
    |(s,n)-> s+pow(s,n-1);;

// 2.4 Declare the F# function occFromIth: string * int * char -> int where occFromIth(str, i, ch)
// the number of occurrences of character ch in positions j in the string str with j ≥ i.
let rec occFromIth (str :string)  i ch = 
    match (str, i ,ch) with
    | ((str:string),i,ch) when i =str.Length-> 0
    | (str,i,ch) -> if str[i]=ch then 1+(occFromIth str (i+1) ch ) else (occFromIth str (i+1) ch);;



// 2.8 Declare an F# function bin: int * int -> int to compute binomial coefficients.
let rec bin  = function
    |(n,0) -> 1
    |(n,k) -> if n=k then 1 else bin(n-1,k-1)+bin(n-1,k);; 


// 4.7 Declare an F# function multiplicity x xs to find the number of times the value x occurs in the list xs.
let rec multiplicity x xs = 
    match xs with
    |[]->0
    |xx::xs -> if xx=x then 1 + multiplicity x xs else multiplicity x xs;;





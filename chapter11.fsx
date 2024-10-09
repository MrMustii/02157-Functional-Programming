(*11.1 Make a declaration for the sequence of odd numbers.*)

let odd = Seq.initInfinite (fun i -> 2*i+1)
odd

(* 11.2 Make a declaration for the sequence of numbers 1, 1, 2, 6,...,n!,...*)
let rec fact (n,m) = match n,m with 
    |(0,m) -> m
    |(n,m) -> fact (n-1,m*n);;
let Sfact = Seq.initInfinite (fun i -> fact(i,1))
Sfact
(* 11.3 Make a declaration for the sequence of seq [1; 1; 2; 6; ... ; n!; ...], where the i + 1’st element is
generated from the i th element by multiplication with i + 1*)
let rec Sfact22 i m= seq { if i=0 then 
                         yield 1
                         yield! Sfact22 (i+1) (1)
                         else
                         yield i*m
                         yield! Sfact22 (i+1) (m*i)};;
Sfact22  0 1

(* 11.9 Declare a sequence denoting the following enumeration of the integers: 0, −1, 1, −2, 2, −3, 3,...*)

let rec alth i = seq {if i= 0 then yield 0 
                      if i <> 0 then 
                        yield -i
                        yield i
                      yield! alth (i+1)};;

let alt = alth 0;;
alt


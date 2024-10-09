(* 1. Declare a function collatz: int -> seq<int> that for a given n > 0 denotes the Collatz sequence starting with n.*)

let rec collatz n = seq {
    yield n
    if n%2=0 then yield! collatz (n/2) else yield! collatz (n*3+1)
}

(*2. Declare a sequence collatzSequences: seq<seq<int>> with the elements: collatz(1) collatz(2) collatz(3) collatz(4)*)
let collatzSequences = Seq.removeAt 0 (Seq.initInfinite collatz)
collatzSequences

(* 3. Declare a sequence stoppingTime: seq<int>. The element tiin this sequence is thestopping time of collatz (i + 1), 
for i ≥ 0. Hence, the first 4 elements of this sequence are 0,1, 7 and 2. Hint: You may use Seq.findIndex.*)

let rec stoppingTimeh n = seq {
    yield Seq.findIndex (fun x -> x=1) (collatz n)
    yield! stoppingTimeh (n+1)
}
let stoppingTime =stoppingTimeh 1

(*4. Make a declaration of the sequence maxStoppingTimes. You may consider making two declarations, 
where one is based on a recursive function and the other on the library function Seq.scan. Let sq be a sequence with 
elements a0 a1 a2 a3 · · · , then Seq.scan f x0 sq gives the sequence x0 x1 x2 x3 x4 · · ·
where x1 = f x0 a0, x2 = f x1 a1 and xi+1 = f xi ai
.*)

let rec maxStoppingTimes n s = seq {
    if n=0 then yield Seq.item n s 
                yield! maxStoppingTimes (n+1) s
    else
    yield max (Seq.item n s) (Seq.item (n-1) s)
    yield! maxStoppingTimes (n+1) s
}
maxStoppingTimes 0 (stoppingTimeh 1)

let maxStoppingTimes2 = Seq.scan (max ) 0 (stoppingTimeh 2)
maxStoppingTimes2
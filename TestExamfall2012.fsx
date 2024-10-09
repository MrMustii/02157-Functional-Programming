//////Problem
//1
type Name = string;;
type Score = int;;
type Result = Name * Score;;

let res1 = [("joe0",-1);("joe1",1001);("joe2",5);("joe3",6);("joe4",7);("joe5",8);("joe7",8);]
let resls = [("joe2",8);("joe3",6);("joe4",7);("joe5",5);("joe7",8);]
let rec legalResult = function
    |[]->true
    |(_,sc)::rest -> if (sc>=0)&&(sc<=100) then legalResult rest else false;;
legalResult res1
legalResult resls
//2
let rec maxScoreH n rls = match rls with 
    |[]->n
    |(_,sc)::rest when n<sc 
                    ->maxScoreH sc rest
    |(_,sc)::rest->maxScoreH n rest 
let maxScore rls = maxScoreH 0 rls;;
maxScore resls
//3
let rec best rls = match rls with 
    |[]->("no result",0)
    |(n,sc)::rest when sc>=(maxScore rest)
                        ->(n,sc)
    |(_,_)::rest -> best rest;;
best []
//4
let scorels rls =List.map (fun (x,y)->float y) rls 
let average rls = match rls with
    |[]-> -1.0
    |rls->List.average (scorels rls);;
average resls
//5
let rec delete r rs = match rs with 
    |[]->[]
    |(n,sc)::rest when r=(n,sc)->rest
    |r'::rest -> r'::delete r rest;;
delete ("joe2",8) resls
//6
let rec bestN rs n = match rs with
    |rs when (List.length rs)<n -> failwith("invaild number")
    |rs when n=0 ->[]
    |rs -> (best rs)::bestN (delete (best rs) rs) (n-1)

bestN resls 3
/////problem 2
//1

type Typ =  | Integer
            | Boolean
            | Ft of Typ list * Typ;;
type Decl = string * Typ;;
let decls =[("x",Integer);("y",Integer);("z",Boolean);("o",Ft([Integer;Integer;Boolean],Boolean))]
let declsiv =[("x",Integer);("x",Integer);("z",Boolean);("o",Ft([Integer;Integer;Boolean],Boolean))]


let distinctvats decls = (List.length decls) = List.length (List.distinct decls)
distinctvats declsiv
//2
type SymbolTable = Map<string,Typ>;;
let toSymbolTable decls=Map.ofList decls

toSymbolTable decls
let sys =Map [("o", Ft ([Integer; Integer; Boolean], Boolean)); ("x", Integer);("y", Integer); ("z", Boolean)]
//3
let rec extendst sys decls = match decls with 
    |[]-> sys
    |(x,t)::r->extendst (Map.add x t sys) r
extendst sys (decls@[("a",Boolean)])
//4
type Exp =  | V of string
            | A of string * Exp list;;

let rec symbolIsDefined sym e = match e with
        |V(a) -> Map.exists (fun k t -> k=a) sym
        |A(s,els) -> Map.exists (fun k t -> k=s) sym && symbolListIsDefined sym els 
and symbolListIsDefined sym els = match els with
    |[]->true
    |s::rest -> (symbolIsDefined sym s) && (symbolListIsDefined sym rest)

let exp1 = A(">",[V "x";V "y"])
symbolIsDefined sys exp1
let sys1 =Map [(">", Ft ([Integer; Integer], Boolean)); ("x", Integer);("y", Integer); ("z", Boolean)]
symbolIsDefined sys1 exp1
//5 
let getEndtype s =match s with 
    |Ft(_,b) ->b
    |_ -> failwith("cant be done")

let rec convert exp sys = match exp with 
    |A(s,ls) -> Ft((convertls ls sys),(getEndtype (Map.find s sys)))
    |V(a)-> Map.find a sys
and convertls ls sys = match ls with 
    |[]->[]
    |V(a)::rest -> (convert (V(a)) sys)::convertls rest sys 
    |A(s,ls)::rest -> (convert (A(s,ls)) sys)::convertls rest sys

let typeOf sys exp=match exp with 
    |A(str,ls)->
        if Map.find str sys = (convert exp sys)
            then getEndtype (Map.find str sys)
            else failwith("not well typed")
    |V(str) -> if Map.find str sys = (convert exp sys)
                then getEndtype (Map.find str sys)
                else failwith("not well typed");;

typeOf sys1 exp1
//6 

type Stm = 
    | Ass of string * Exp // assignment
    | Seq of Stm * Stm // sequential composition
    | Ite of Exp * Stm * Stm // if-then-else
    | While of Exp * Stm // while
    | Block of Decl list * Stm;; // block

let rec wellTyped sys st = match st with 
    |Ass(x,e) -> (Map.containsKey x sys)&& (symbolIsDefined sys e) && Map.find x sys =typeOf sys e
    |Seq(st1,st2) -> (wellTyped sys st1) && (wellTyped sys st2)
    |Ite(e,st1,st2) -> (typeOf sys e = Boolean)&&(symbolIsDefined sys e)&&(wellTyped sys st1) && (wellTyped sys st2)
    |While(e,st) -> (typeOf sys e = Boolean)&&(symbolIsDefined sys e)&&(wellTyped sys st)
    |Block(decls,st)->(distinctvats decls)&&(wellTyped (extendst sys decls) st)
//////Problem 3

let rec h a b =
    match a with
    | [] -> b
    | c::d -> c::(h d b);;
h decls decls
type T<'a,'b> = | A of 'a | B of 'b | C of T<'a,'b> * T<'a,'b>;;
C(A(5),B(true))
C((A([5])),B(Some "a"))
let rec f1 = function
    | C(t1,t2) -> 1 + max (f1 t1) (f1 t2)
    | _ -> 1;;

let rec f2 = function
    | A e | B e -> [e]
    | C(t1,t2) -> f2 t1 @ f2 t2;;
f2 (C((A([5])),B([5])))
let rec f3 e b t =
    match t with
    | C(t1,t2) when b -> C(f3 e b t1, t2)
    | C(t1,t2) -> C(t1, f3 e b t2)
    | _ when b -> C(A e, t)
    | _ -> C(t, B e);;
f3 [3] false (C((A([5])),B([5])))
//1


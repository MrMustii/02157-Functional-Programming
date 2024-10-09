type ArticleCode = string;;
type ArticleName = string;;
type Price = int;;
type Register = (ArticleCode * (ArticleName*Price)) list;;

let reg:Register = [("a1",("cheese",25));
("a2",("herring",4));
("a3",("soft drink",5)) ];;

type NoPieces = int;; 
type Item = NoPieces * ArticleCode;;
type Purchase = Item list;;

let pur :Purchase= [(3,"a2"); (1,"a1")];;

type Info = NoPieces * ArticleName * Price;;
type Infoseq = Info list;;
type Bill = Infoseq * Price;;

let rec findArticle ac = function
        |(ac',adesc)::_ when ac=ac' -> adesc
        | _::reg -> findArticle ac reg
        | _ ->
            failwith(ac + " is an unknown article code");;

let rec makeBill reg = function
        | [] -> ([],0)
        | (np,ac)::pur -> let (aname,aprice) = findArticle ac reg
                          let tprice = np*aprice
                          let (billtl,sumtl) = makeBill reg pur
                          ((np,aname,tprice)::billtl,tprice+sumtl);;

let findArticle1 ac (reg:Register) = 
        let  s= List.tryFind (fun (x,(_,_))->x=ac) reg
        if s <> None then
                let (a,(b,c))=Option.get((s))
                (b,c)
        else 
                ("",0);;


findArticle1 "a1" reg;;



let makeBill1 reg pur =
    let anames = List.map (fun (x,y)->y) pur
    let anum = List.map (fun (x,y)->x) pur 
    let items =  List.map (fun x -> findArticle1 x reg) anames
    let lsprice = List.map2 (fun x (y,z) -> (x,y,x*z) ) anum items 
    let total = List.foldBack (fun (x,y,z) s -> s+z) lsprice 0 
    (lsprice,total);;



List.foldBack (fun s (x,y)->findArticle1 x reg) pur ("",0);;
pur

makeBill1 reg pur;;

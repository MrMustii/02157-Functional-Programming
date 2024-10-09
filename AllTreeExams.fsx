///December 20th, 2011

//May 28th, 2014
//Problem 3
type Name = string;;
type Sex = | M // male
            | F // female
type YearOfBirth = int;;
type FamilyTree = P of Name * Sex * YearOfBirth * Children
and Children = FamilyTree list;;
//1
let rec WF p=match p with 
    |P(_,_,y,cs)-> WFCildren cs y
and WFCildren cs y = match cs with 
        |[] -> true
        |c::[]->true 
        |P(n1,s1,y1,cs1)::P(n2,s2,y2,cs2)::rest when y1<=y2&&y<y1->(WFCildren cs1 y1)&&(WFCildren (P(n2,s2,y2,cs2)::rest) y )
        |_->false

//2
let makePerson (n,s,y) = P(n,s,y,[])

let eve = P("Eve",F,2010,[])
let bob = P("bob",M,2008,[])
let Peter = P("Peter",M,2005,[])

let Mary = P("Mary",F,1980,[Peter;bob;eve])


let rec insertChildOf name (P(cn,csex,cy,ccs)) p = match p with 
    |(P(n,s,y,cs)) when y>=cy->None
    |(P(n,s,y,cs)) when n=name -> Some (P(pn,ps,py,(insertChildOfInList name c pcs)))
    |(P(n,s,y,cs)) -> match  insertChildOfInList name (P(cn,csex,cy,ccs)) cs with 
                            |None -> None
                            |Some ls -> Some (P(n,s,y,ls))
and insertChildOfInList name c cs = match cs with 
    |[]->None
    |rest  ->Some (List.map (insertChildOf name c ) rest)


insertChildOf "Eve" (P("m",M,2020,[])) Mary

let add a = a+1
List.map add [1;2]
// December 18th, 2014
//1
type Outcome = | S | F // S: for success and F: for failure
type Sample = Outcome list
type ProbTree = | Branch of string * float * ProbTree * ProbTree
                | Leaf of string

let exp = Branch(">2",0.67, Branch(">3",0.5, Leaf "A", Leaf "B")
                          , Branch(">3",0.5, Leaf "C", Leaf "D"))

let rec probOk = function
        |Branch(_,p,tl,tr)->(p>=0)&&(p<=1)&&(probOk tl)&&(probOk tr)
        |Leaf _ ->true
probOk exp
//2

let rec isSample (os,t) =match os,t with
    |([],Branch _) ->false
    |([],Leaf _) -> true
    |(o::rest,Branch(_,_,tl,tr)) when o=F ->isSample(rest,tr)
    |(o::rest,Branch(_,_,tl,tr)) when o=S ->isSample(rest,tl)
    |_ ->false
isSample([F;S],exp)

//3

type Description = (Outcome*string)list*int*string
let rec descriptionOfA acc accp os t = match os,t with 
    |(os,t) when not(isSample(os,t)) ->failwith "invalid sample"
    |(o::rest,Branch(d,p,tl,tr)) when o=F->  descriptionOfA (acc@[(o,d)]) (accp*(1.0-p)) rest tr  
    |(o::rest,Branch(d,p,tl,tr)) when o=S->  descriptionOfA (acc@[(o,d)]) (accp*(p)) rest tl
    |(_,Leaf a) ->(acc,accp,a)

let descriptionOf os t= descriptionOfA [] 1 os t
descriptionOf [F;S] exp

//4
let rec depthOfTree = function
    |Leaf _ -> 0
    |Branch(_,_,tl,tr) -> 1+(depthOfTree tl)
depthOfTree exp

let rec preorder = function
    |Leaf _ -> []
    |Branch(_,p,tl,tr) -> p::(preorder tl)@((1.0-p)::(preorder tr ))

let preFold f e t = List.fold f e (preorder t)
preFold (fun x->x) 0.0 exp 
preorder exp
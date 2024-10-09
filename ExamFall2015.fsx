type Name = string
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
    and Tributaries = River list


let riv =               R("R",10,[R("R1",5,[]);
                    R("R2",15,[R("R4",2,[])]);
                    R("R3",8,[])]);;

let riv3 =R("R3",8,[]);;

let rec contains n r = match r with 
        |R(name,s,ls) when n=name -> true
        |R(name,s,ls)->List.exists (fun x -> contains n x) ls;;
        
contains "R3" riv


let rec allNamesH rls = match rls with 
        |[] -> []
        |(R(name,s,ls))::xs -> name::(allNamesH ls)@(allNamesH xs);;
        
        
        
let allNames r = match r with
        |R(name,s,ls) ->name::(allNamesH ls)

allNames riv

let rec totalFlowH rls = match rls with 
        |[] -> []
        |R(name,s,ls)::xs -> s::(totalFlowH ls)@(totalFlowH xs);;
        
let totalFlow r = match r with
        |R(name,s,ls) -> s+(List.sum (totalFlowH ls));;

totalFlow riv

let rec mainSourceH rls = match rls with
            |[] -> []
            |R(name,s,ls)::xs -> (s,name)::(mainSourceH ls)@(mainSourceH xs)
            
let mainSourceHH r = match r with 
        |R(name,s,ls)->Map.maxKeyValue(Map.add s name (Map.ofList(mainSourceH ls)))

let mainSource r = 
        let (s,name)=mainSourceHH r
        (name,s)

mainSource riv
let rec tryInsert n t r = match r with 
            |R(name,s,ls) when List.exists (fun x -> n=x) (allNames r) -> Some (name,s,t::ls)
            |_->None;
tryInsert "R" riv3 riv

(*
        type Name = string
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
and Tributaries = River list


let riv = River("R",10,[River("R1",5,[]);
						River("R2",15,[River("R4",2,[])]);
						River("R3",8,[])]);;

let riv3 =River("R3",8,[]);;

let contains n r = match r with 
			|(name,s,ls) when n=name -> true
			|(name,s,ls)->List.exists (fun x -> contains n x) ls;;
			


let rec allNamesH rls = match rls with 
			|[] -> []
			|(name,s,ls)::xs -> name::(allNamesH ls)::(allNamesH xs);;
			
			
			
let allNames r = 
			let (name,s,ls) = r 
			name::(allNamesH ls);;
			

let rec totalFlowH r = 
			|[] -> []
			|(name,s,ls)::xs -> s::(totalFlowH ls)::(totalFlowH xs);;
			
let totalFlow rls = match rls with 
			let (name,s,ls) = r 
			s+(List.sum (totalFlowH ls));;


let rec mainSourceH rls = match rls with
			|[] -> []
			|(name,s,ls)::xs -> (s,name)::(mainSourceH ls)::(mainSourceH xs);;
			


let mainSource r = 
		let (name,s,ls) =r
		let m = Map.ofList (mainSourceH ls)
		let (ansS,ansN) = Map.maxKeyValue(Map.add s name m)
		(ansN,ansS)
		
let rec tryInsert n t r = match r with 
				|(name,s,ls) when List.exists (fun x -> n=x) (allNames r) -> Some (name,s,t::ls)
				|None
			
*)
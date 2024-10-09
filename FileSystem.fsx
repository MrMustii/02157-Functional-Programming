type FileSys = Element list
    and Element = | File of string*string
                  | Dir of string * FileSys;;




let rec NFSHelper fls= match fls with
        |[]->[]
        |File(a,b)::rest -> (a+"."+b)::NFSHelper rest
        |Dir(a,b)::rest -> a::NFSHelper b@NFSHelper rest;;


let rec namesFileSys d= match d with 
        |Dir(a,b)-> match a,b with
                        |(a,b) -> a :: NFSHelper b
        
namesFileSys d1
//= ["d1"; "a1.java"; "d2"; "a2.fsx"; "d3";"a3.fs"; "a4.fsx"; "d3"; "a5.pdf"]

let rec searchFileSys ext filesys = match filesys with
        |[] -> []
        |e::rest -> (searchElement ext e) @  (searchFileSys ext rest)
and searchElement ext elem = match elem with
        |Dir(a,b) -> searchFileSys ext b
        |File(a,b) when b=ext -> [a]
        |File(a,b) -> [];;


searchElement "fsx" d1


let d1 = Dir("d1",[File("a1","java");
         Dir("d2", [File("a2","fsx");
         Dir("d3", [File("a3","fs")])]);
         File("a4","fsx");
         Dir("d3", [File("a5","pdf")])]);;



let rec longNamesFileSys filesys =
         match filesys with 
        |[] -> Set.empty
        |D::rest -> Set.union (longNamesElement D) (longNamesFileSys rest)
and longNamesElement elem = match elem with 
        |Dir(a,b) -> Set.map (fun x -> a+"\\"+x) (longNamesFileSys b)
        |File(a,b) -> set[a+"."+b];;

longNamesElement d1

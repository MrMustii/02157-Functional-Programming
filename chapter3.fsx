// 3.1 A time of day can be represented as a triple (hours, minutes, f) where f is either AM or PM– or as a record. 
// Declare a function to test whether one time of day comes before another. For example, (11,59,"AM") comes before (1,15,"PM"). 
// Make solutions with triples as well as with records. Declare the functions in infix notation
type hourT = AM | PM;;
type time = int * int * hourT;;

let Before a b = match a , b with
    |(_,_,AM),(_,_,PM) -> true
    |(_,_,PM),(_,_,AM) -> false
    |(ha,_,_),(hb,_,_) when ha<>hb -> if ha<hb then true else false
    |(_,ma,_),(_,mb,_) -> if ma<mb then true else false ;;
Before (11,59,AM) (1,15,PM);;


// 3.2 The former British currency had 12 pence to a shilling and 20 shillings to a pound. 
// Declare functions to add and subtract two amounts, represented by triples (pounds, shillings, pence) of integers, and 
// declare the functions when a representation by records is used. 
// Declare the functions in infix notation with proper precedences, and use patterns to obtain readable declarations


type ukpounds= int * int * int;;

let (.+.) a b = match a,b with 
    (Pa,sa,pa),(Pb,sb,pb)->(((((pb+pa)/12)+sa+sb)/20)+Pa+Pb,(((pb+pa)/12)+sa+sb)%20,(pb+pa)%12);;

(7,5,9) .+. (100,14,6);;

// 3.3 The set of complex numbers is the set of pairs of real numbers. Complex numbers behave almost
// like real numbers if addition and multiplication are defined by:
//(a, b)+(c, d)=(a + c, b + d)
//(a, b) · (c, d) =(ac − bd, bc + ad)

// 1. Declare suitable infix functions for addition and multiplication of complex numbers


let (.++.) a b =match a,b with (a,b),(c,d) -> (a+c,b+d);;
(3,7) .++. (7,3);;

let (.*.) a b = match a,b with (a,b),(c,d) -> (a*c-b*d,b*c+a*d);;

(3,7) .*. (7,3);;

//2. The inverse of (a, b) with regard to addition, that is, −(a, b), is (−a, −b), and the inverse of (a, b) with regard to multiplication, 
// that is, 1/(a, b), is (a/(a2 +b2), −b/(a2 +b2)) (providedthat a and b are not both zero). 
// Declare infix functions for subtraction and division of complex numbers

let (.-.) a b =match a,b with (a,b),(c,d) -> (a,b).++.(-c,-d);;
(3,7) .-. (7,3);;


let reciprocalH  =function (a,b) -> (a/(a**2.0+b**2.0), -b/(a**2.0+b**2.0));;
let reciprocal = function (a,b) -> reciprocalH(float a ,float b);;
reciprocal(3,7)


let devideH a b =  match a,b with (a,b),(c,d) -> (float a ,float b),reciprocal(c,d);;
devideH (1,2) (3,7)

let floatmult (a,b) = match a,b with ((a:float),(b:float)),((c:float),(d:float)) ->  ((a*c-b*d,b*c+a*d));;
floatmult ((1.0,2.0),(3.0,7.0))


let (./.) a b = match a,b with (a,b),(c,d) -> floatmult(devideH (a,b) (c,d)) ;;
(3,7) ./. (5,6);;

// 3.4 A straight line y = ax + b in the plane can be represented by the pair (a, b) of real numbers.
// 1. Declare a type StraightLine for straight lines.
// 2. Declare functions to mirror straight lines around the x and y-axes.
// 3. Declare a function to give a string representation for the equation of a straight line

type StraightLine<'a> = 'a*'a


let mirrory (a,b)  = (-a,b);;

let mirrorx (a,b) =  (-1/a,-1/b)

let rep (a,b) = match a,b with
                | (a,b) when b >0 -> "y= " + string a + "x" + " + " + string  b
                | (a,b) when b < 0 -> "y= " + string a + "x" + string  b;;

// 3.5 Make a type Solution capturing the three capabilities for roots in a quadratic equation: two roots, one root and no root 
// (cf. Section 3.5). Declare a corresponding solve function.

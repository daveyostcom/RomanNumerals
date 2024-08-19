
module RomanNumerals3

open   RomanNumerals

// This variant uses one partial active pattern instead of two.

let parseRomanNumeral input  : RomanInt =
  let length = String.length input
  let (|C|_|) i =
    if i >= length then
      None
    else
      let vChar c p =
//      printfn $"V %d{i} %c{c} %d{p}"
        5,c,p,1,true,i+1
      let iChar c p =
        let rec countSame acc i =
          let isSame = i < length  &&  input[i] = c
          if  isSame then  countSame (acc+1) (i+1)
                     else  acc
        let n = countSame 1 (i+1)
//      printfn $"I %d{i} %c{c} %d{p} %d{n}"
        let ok = n <= 4
        1,c,p,n,ok,i+n
      let c = input[i]
      let x = "IVXLCDM".IndexOf c
      let p = x / 2                 // p for place or power of 10
      let isIXCM x =  x % 2 = 0
      match x with
      | -1                  ->  None
      | x  when x |> isIXCM ->  Some (iChar c p)
      | _                   ->  Some (vChar c p)
  let rec nextDigit s p acc i  : RomanInt =
    let eq p1 p2 =  p1 = p2  &&  p1 < p  
    let str c n = System.String(c, n)
    let acc' p n = acc + n * pown 10 p
    match i with
    | i                                       when i >= length ->  Value acc
    | C (1,c,p,1,true ,C (5,c2,p2,_,_   ,i))  when eq p  p2    ->  nextDigit  $"{c}{c2}"            p (acc' p    4 ) i  // IV
    | C (1,c,p,1,true ,C (1,c2,p2,1,true,i))  when eq p (p2-1) ->  nextDigit  $"{c}{c2}"            p (acc' p    9 ) i  // IX
    | C (5,c,p,_,_    ,C (1,c2,p2,n,true,i))  when eq p  p2    ->  nextDigit ($"{c}"  + (str c2 n)) p (acc' p (5+n)) i  // VIIII
    | C (x,c,p,n,true ,i)                     when eq p  p     ->  nextDigit            (str c  n)  p (acc' p (x*n)) i  // IIII or V
    | C (1,c,_,_,false,_)                                      ->  Error (BadOrder (s + (str c 4),c))         // IIIII
    | C (_,c,_,_,_    ,_)                                      ->  Error (BadOrder (s            ,c))         // IIX, IIV, ...
    | i                                                        ->  Error (BadChar  (input[0..i-1],input[i]))  // XCVe
  match input with
  | "" ->  Error Empty
  | _  ->  nextDigit "" (placeMax+1) 0 0



(*

  Output, if printfs are uncommented.
  Partial active patterns have a problem with redundant calls to the patterns.
  In the printfs, a repeat of the i number in column 2
  indicates an active pattern that is called redundantly.
    CDXCIIII
    I 0 C 2 1
    V 1 D 2
    I 2 X 1 1
    I 3 C 2 1
    I 2 X 1 1   <–- here
    I 3 C 2 1   <–- here
    I 4 I 0 4
    I 4 I 0 4   <–- here
    I 4 I 0 4   <–- here
    I 4 I 0 4   <–- here
    LXXXXV
    V 0 L 1
    V 0 L 1     <–- here
    V 0 L 1     <–- here
    I 1 X 1 4
    V 5 V 0
    V 5 V 0     <–- here
    V 5 V 0     <–- here
    V 5 V 0     <–- here

*)

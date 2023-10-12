
module RomanNumerals3

open   RomanNumerals

// This variant one partial active pattern instead of two.

let parseRomanNumeral (roman: string) : RomanInt =
  let length = String.length roman
  let vChar p c i =
//  printfn "V %d %c %d" i c p
    5,p,c,1,true,i+1
  let iChar p c i =
    let rec count acc i =
      let beyond = i >= length  ||  roman[i] <> c
      if  beyond then  acc
                 else  count (acc+1) (i+1)
    let n  = count 1 (i+1)
    let ok = n <= 4
//  printfn "I %d %c %d %d" i c p n
    1,p,c,n,ok,i+n
  let (|C|_|) i =
    if i >= length then  None else
    let c = roman[i]
    let p = "IXCMVLD".IndexOf c // p for place or power of 10
    match p with
    | -1            ->  None
    | p  when p < 4 ->  Some (iChar  p    c i)
    | _             ->  Some (vChar (p-4) c i) 
  let rec next s p acc i : RomanInt =
    let eq p1 p2 =  p1 = p2  &&  p1 < p  
    let str c n = System.String(c, n)
    let acc' p n = acc + n * pown 10 p
    match i with
    | i                                       when i >= length ->  Value acc
    | C (1,p,c,1,true ,C (5,p2,c2,_,_   ,i))  when eq p  p2    ->  next  $"{c}{c2}"            p (acc' p    4 ) i  // IV
    | C (1,p,c,1,true ,C (1,p2,c2,1,true,i))  when eq p (p2-1) ->  next  $"{c}{c2}"            p (acc' p    9 ) i  // IX
    | C (5,p,c,_,_    ,C (1,p2,c2,n,true,i))  when eq p  p2    ->  next ($"{c}"  + (str c2 n)) p (acc' p (5+n)) i  // VIIII
    | C (x,p,c,n,true ,i)                     when eq p  p     ->  next            (str c  n)  p (acc' p (x*n)) i  // IIII, V replaces 2 lines
    | C (1,_,c,_,false,_)                                      ->  Error (BadOrder (s + (str c 4),c))         // IIIII
    | C (_,_,c,_,_    ,_)                                      ->  Error (BadOrder (s            ,c))         // IIX, IIV, ...
    | i                                                        ->  Error (BadChar  (roman[0..i-1],roman[i]))  // XCVe
  match roman with
  | "" ->  Error Empty
  | _  ->  next "" (placeMax+1) 0 0



(*

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

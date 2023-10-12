
module RomanNumerals2

open   RomanNumerals

// This variant uses partial active patterns.

let parseRomanNumeral (roman: string) : RomanInt =
  let length = String.length roman
  let get (chars: string) i result =
    if i >= length then  None else
      let c = roman[i]
      let p = chars.IndexOf c
      if p = -1 then
//      printfn "g %d %c %d" i roman[i] p
        None else
        Some (result p c i)
  let (|V|_|) i =
    let result p c i =
//    printfn "V %d %c %d" i roman[i] p
      p,c,i+1
    get "VLD" i result
  let (|I|_|) i =
    let result p c i =
      let rec count acc i =
        let beyond = i >= length  ||  roman[i] <> c
        if  beyond then  acc
                   else  count (acc+1) (i+1)
      let n  = count 1 (i+1)
      let ok = n <= 4
//    printfn "I %d %c %d %d" i roman[i] p n
      p,c,n,ok,i+n
    get "IXCM" i result
  let rec next sPrev pPrev accPrev iPrev : RomanInt =
    let eq p p2 = p < pPrev  &&  p = p2
    let str c n = System.String(c, n)
    let acc p n = accPrev + n * pown 10 p
    match iPrev with
    | i                                   when i >= length ->  Value accPrev
    | I (p,c,1,true ,V (p2,c2       ,i))  when eq p  p2    ->  next  $"{c}{c2}"            p (acc p    4 ) i  // IV
    | I (p,c,1,true ,I (p2,c2,1,true,i))  when eq p (p2-1) ->  next  $"{c}{c2}"            p (acc p    9 ) i  // IX
    | V (p,c        ,I (p2,c2,n,true,i))  when eq p  p2    ->  next ($"{c}"  + (str c2 n)) p (acc p (5+n)) i  // VIIII
    | I (p,c,n,true ,i)                   when eq p  p     ->  next            (str c  n)  p (acc p    n ) i  // IIII
    | V (p,c        ,i)                   when eq p  p     ->  next  $"{c}"                p (acc p    5 ) i  // V
    | I (_,c,_,false,_)                                    ->  Error (BadOrder (sPrev + (str c 4),c))    // IIIII
    | I (_,c,_,_    ,_)                                    ->  Error (BadOrder (sPrev            ,c))    // IIX, IVI, VIX ..
    | V (_,c        ,_)                                    ->  Error (BadOrder (sPrev            ,c))    // IIV, IVV, VIV ..
    | i                                                    ->  Error (BadChar  (roman[0..i-1],roman[i])) // XCVe
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
    g 3 C -1
    I 2 X 1 1   <–- here
    I 3 C 2 1   <–- here
    I 4 I 0 4
    I 4 I 0 4   <–- here
    g 4 I -1    <–- here
    I 4 I 0 4   <–- here
    LXXXXV
    g 0 L -1
    g 0 L -1    <–- here
    V 0 L 1     <–- here
    I 1 X 1 4
    g 5 V -1
    g 5 V -1    <–- here
    V 5 V 0     <–- here
    g 5 V -1    <–- here
    V 5 V 0     <–- here


*)

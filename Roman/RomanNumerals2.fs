
module RomanNumerals2

open   RomanNumerals

// This variant uses partial active patterns.

let parseRomanNumeral input  : RomanInt =
  let length = String.length input
  let get (chars: string) i result =
    if i >= length then
      None
    else
      let c = input[i]
      let p = chars.IndexOf c
      if p = -1 then
//      printfn $"g %d{i} %c{c} %d{p}"
        None
      else
        Some (result c p i)
  let (|V|_|) i =
    let result c p i =
//    printfn $"V %d{i} %c{c} %d{p}"
      c,p,i+1
    get "VLD" i result
  let (|I|_|) i =
    let result c p i =
      let rec countSame acc i =
        let isSame = i < length  &&  input[i] = c
        if  isSame then  countSame (acc+1) (i+1)
                   else  acc
      let n = countSame 1 (i+1)
//    printfn $"I %d{i} %c{c} %d{p} %d{n}"
      let ok = n <= 4
      c,p,n,ok,i+n
    get "IXCM" i result
  let rec nextDigit s p acc i : RomanInt =
    let eq p1 p2 =  p1 = p2  &&  p1 < p
    let str c n = System.String(c, n)
    let acc' p n = acc + n * pown 10 p
    match i with
    | i                                   when i >= length ->  Value acc
    | I (c,p,1,true ,V (c2,p2       ,i))  when eq p  p2    ->  nextDigit  $"{c}{c2}"            p (acc' p    4 ) i  // IV
    | I (c,p,1,true ,I (c2,p2,1,true,i))  when eq p (p2-1) ->  nextDigit  $"{c}{c2}"            p (acc' p    9 ) i  // IX
    | V (c,p        ,I (c2,p2,n,true,i))  when eq p  p2    ->  nextDigit ($"{c}"  + (str c2 n)) p (acc' p (5+n)) i  // VIIII
    | I (c,p,n,true ,i)                   when eq p  p     ->  nextDigit            (str c  n)  p (acc' p    n ) i  // IIII
    | V (c,p        ,i)                   when eq p  p     ->  nextDigit  $"{c}"                p (acc' p    5 ) i  // V
    | I (c,_,_,false,_)                                    ->  Error (BadOrder (s + (str c 4),c))    // IIIII
    | I (c,_,_,_    ,_)                                    ->  Error (BadOrder (s            ,c))    // IIX, IVI, VIX ..
    | V (c,_        ,_)                                    ->  Error (BadOrder (s            ,c))    // IIV, IVV, VIV ..
    | i                                                    ->  Error (BadChar  (input[0..i-1],input[i])) // XCVe
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

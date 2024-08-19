// https://github.com/daveyostcom/RomanNumerals/blob/master/Roman/RomanNumerals4.fs
// Based on a 2016-01-26 comment by Femistoklov on http://disq.us/p/150kjcg
// Changes
// • Return errors instead of raising exceptions.
// • Advance a character index instead of using lists.
// • Use a multi-case active pattern.
// • Use tail recursion.

module RomanNumerals4

open RomanNumerals

let parseRomanNumeral input  : RomanInt =
  let length = String.length input
  let (|I|V|ITooMany|Bad|Done|) i =
    if i >= length then
      Done
    else
      let vChar c p =
//      printfn $"V %d{i} %c{c} %d{p}"
        V (c,p,i+1)
      let iChar c p =
        let rec countSame acc i =
          let isSame = i < length  &&  input[i] = c
          if  isSame then  countSame (acc+1) (i+1)
                     else  acc
        let n = countSame 1 (i+1)
//      printfn $"I %d{i} %c{c} %d{p} %d{n}"
        if n > 4 then  ITooMany c
                 else  I (c,p,n,i+n)
      let c = input[i]
      let x = "IVXLCDM".IndexOf c
      let p = x / 2                 // p for place or power of 10
      let isIXCM x =  x % 2 = 0
      match x with
      | -1                  ->  Bad (input[0..i-1],c)
      | x  when x |> isIXCM ->  iChar c p
      | _                   ->  vChar c p
  let rec nextDigit s p acc i  : RomanInt =
    let eq p1 p2 =  p1 = p2  &&  p1 < p  
    let str c n = System.String(c, n)
    let acc' p n = acc + n * pown 10 p
    match i with
    | I (c,p,1,V (c2,p2  ,i))  when eq p  p2    ->  nextDigit  $"{c}{c2}"            p (acc' p    4 ) i  // IV
    | I (c,p,1,I (c2,p2,1,i))  when eq p (p2-1) ->  nextDigit  $"{c}{c2}"            p (acc' p    9 ) i  // IX
    | V (c,p  ,I (c2,p2,n,i))  when eq p  p2    ->  nextDigit ($"{c}"  + (str c2 n)) p (acc' p (5+n)) i  // VIIII
    | I (c,p,n,           i )  when eq p  p     ->  nextDigit            (str c  n)  p (acc' p    n ) i  // IIII
    | V (c,p  ,           i )  when eq p  p     ->  nextDigit  $"{c}"                p (acc' p    5 ) i  // V
    | I (c,_,_,           _ ) 
    | V (c,_  ,           _ )                   ->  Error (BadOrder (s            ,c))  // IIX, IIV, ...
    | ITooMany c                                ->  Error (BadOrder (s + (str c 4),c))  // IIIII
    | Bad (s,c)                                 ->  Error (BadChar  (s            ,c))  // XCVe
    | Done                                      ->  Value acc
  match input with
  | "" ->  Error Empty
  | _  ->  nextDigit "" (placeMax+1) 0 0


(*

  Output, if printfs are uncommented.
  The multi-case active pattern is called only once for each match attempt.
  In the printfs, if the i number in column 2 were to repeat, it would
  indicate that the active pattern is called redundantly.

    CDXCIIII
    I 0 C 2 1
    V 1 D 2
    I 2 X 1 1
    I 3 C 2 1
    I 4 I 0 4
    LXXXXV
    V 0 L 1
    I 1 X 1 4
    V 5 V 0

*)
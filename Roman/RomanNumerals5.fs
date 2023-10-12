
module RomanNumerals5

open RomanNumerals

// What if F# didn’t have multi-case active patterns?
// This variant uses a function returning a choice type.
// The matching for adjacent characters (IV, VI, IX) is an ad hoc mess,
// and it still isn’t complete; it fails on some BadOrder inputs
// because the backtracking necessary to handle them will require
// even more complexity than you see here.

// This variant does not pass the bad-inputs test.

type Token =
  | I        of char * int * int * int
  | V        of char * int * int
  | ITooMany of char
  | Bad     of string * char
  | Done
  
let parseRomanNumeral (roman: string) : RomanInt =
  let length = String.length roman
  let probe i  : Token =
    let vChar c p =
//    printfn "V %d %c %d" i c p
      V (c,p,i+1)
    let iChar c p =
      let rec countSame n i =
        let isSame = i < length  &&  roman[i] = c
        if  isSame then  countSame (n+1) (i+1)
                   else  n
      let n = countSame 1 (i+1)
//    printfn "I %d %c %d %d" i c p n
      if n > 4 then  ITooMany c
               else  I (c,p,n,i+n)
    if i >= length then
      Done
    else
      let c = roman[i]
      let x = "IVXLCDM".IndexOf c
      let p = x / 2                 // p for place or power of 10
      match x with
      | -1                ->  Bad (roman[0..i-1],c)
      | x  when x % 2 = 0 ->  iChar c p
      | _                 ->  vChar c p
  let rec nextDigit s p acc i : RomanInt =
    let eq p1 p2 = p1 = p2  &&  p1 < p  
    let str c n = System.String(c, n)
    let acc' p n = acc + n * pown 10 p
    if i >= length then Value acc else
    match probe i with
    | I (c,p,1,i) ->
      if i >= length then
        if                      eq p  p   then  Value (acc' p 1)                                     // I
                                          else  Error (BadOrder (s, c))
      else
        match probe i with
        | V (c2,p2  ,i)    when eq p  p2    ->  nextDigit  $"{c}{c2}"            p (acc' p    4 ) i  // IV
        | I (c2,p2,1,i)    when eq p (p2-1) ->  nextDigit  $"{c}{c2}"            p (acc' p    9 ) i  // IX
        | _                                 ->  nextDigit            (str c  1)  p (acc' p    1 ) i  // IIII
    | I (c,p,n,i)          when eq p  p     ->  nextDigit            (str c  n)  p (acc' p    n ) i  // IIII
    | I (c,_,_,_)                           ->  Error (BadOrder (s, c))
    | V (c,p  ,i) ->
      if i >= length then
        if                      eq p  p   then  nextDigit  $"{c}"                p (acc' p    5 ) i  // V
                                          else  Error (BadOrder (s, c))
      else
        match probe i with
        | I (c2,p2,n,i)    when eq p  p2    ->  nextDigit ($"{c}"  + (str c2 n)) p (acc' p (5+n)) i  // VIIII
        | _                                 ->  nextDigit            (str c  1)  p (acc' p    5 ) i  // IIII
    | ITooMany c                            ->  Error (BadOrder (s + (str c  4),c))                  // IIIII
    | Bad (s,c)                             ->  Error (BadChar  (s             ,c))                  // XCVe
    | Done                                  ->  Value acc
  match roman with
  | "" ->  Error Empty
  | _  ->  nextDigit "" (placeMax+1) 0 0

(* output:
TestMode is PrintFailures
Output   is Stdout
Good inputs:
Bad inputs:
#  IVIX                BadOrder     I X   –– expected: BadOrder    IV I
#  IXIV                BadOrder     I V   –– expected: BadOrder    IX I
#  XLXC                BadOrder     X C   –– expected: BadOrder    XL X
#  XCXL                BadOrder     X L   –– expected: BadOrder    XC X
#  CDCM                BadOrder     C M   –– expected: BadOrder    CD C
#  CMCD                BadOrder     C D   –– expected: BadOrder    CM C
*)

(*
  The multi-case active pattern, used here, is called only once for each match attempt.
  In the printfs, a repeat of the i number in column 2
  indicates an active pattern that is called redundantly.
  There are no repeats below.  But you will see them in the other variants.
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

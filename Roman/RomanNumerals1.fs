// https://github.com/daveyostcom/RomanNumerals/blob/master/Roman/RomanNumerals1.fs
// Based on https://fsharpforfunandprofit.com/posts/roman-numerals/#the-entire-code-for-the-second-version
// Changes
// • Return errors from the parse
// • Use tail recursion.

module RomanNumerals1

open   RomanNumerals


let parseRomanNumeral (roman: string) : RomanInt =
  let rec next sPrev pPrev accPrev list =
    let (|I|_|) c = Array.tryFindIndex ((=) c) [|'I';'X';'C';'M'|]
    let (|V|_|) c = Array.tryFindIndex ((=) c) [|'V';'L';'D'    |]
    let iiii cPrev acc list =
      let eq4 p1 p2 p3 p4 = p1 = p2 && p1 = p3 && p1 = p4
      let eq3 p1 p2 p3    = p1 = p2 && p1 = p3
      let eq2 p1 p2       = p1 = p2
      let str c n =
        match cPrev with
        | V _ ->  string cPrev + System.String(c,n)
        | _   ->                 System.String(c,n)
      let acc' p n = acc + n * pown 10 p
      match list with
      | I p as c :: I p2 :: I p3 :: I p4 :: tail when eq4 p p2 p3 p4 ->  next (str c 4) p (acc' p 4) tail // IIII
      | I p as c :: I p2 :: I p3         :: tail when eq3 p p2 p3    ->  next (str c 3) p (acc' p 3) tail // III
      | I p as c :: I p2                 :: tail when eq2 p p2       ->  next (str c 2) p (acc' p 2) tail // II
      | I p as c                         :: tail                     ->  next (str c 1) p (acc' p 1) tail // I
      | c::_                                                         ->  Error (BadChar  (sPrev,c)) // Can’t happen
      | _                                                            ->  failwith "Can’t happen"
    let eq p1 p2 = p1 = p2  &&  p1 < pPrev
    let acc p n = accPrev + n * pown 10 p
    let sAll c =
      let index = roman.IndexOf(string c)
      if index - 1 < 0 then  ""
                       else  roman.Substring(0, index)
    match list with
    | []                                                 ->  Value accPrev
    | I p as c1 :: (V p2 as c2) :: tail when eq p  p2    ->  next  $"{c1}{c2}" p (acc p 4)      tail  // IV
    | I p as c1 :: (I p2 as c2) :: tail when eq p (p2-1) ->  next  $"{c1}{c2}" p (acc p 9)      tail  // IX
    | I p as c                  :: tail when eq p  p     ->  iiii    '~'          accPrev  (c ::tail) // I
    | V p as c1 :: (I p2 as c2) :: tail when eq p  p2    ->  iiii     c1         (acc p 5) (c2::tail) // VI
    | V p as c                  :: tail when eq p  p     ->  next  $"{c}"      p (acc p 5)      tail  // V
    | I _ as c  :: _
    |(V _ as c  :: _ )                                   ->  Error (BadOrder (sPrev ,c))
    | c         :: _                                     ->  Error (BadChar  (sAll c,c))
  match roman with
  | "" ->  Error Empty
  | s  ->  next "" (placeMax+1) 0 [ for c in s -> c ]


module Tests
open   RomanNumerals

// Generate input test cases.

let digitsSeq place = seq {
  let places = [|
    //   0       1           2            3             4           5         6          7
    [| "",0 ; "I",   1 ; "II",   2 ; "III",   3 ; "IIII",   4 ; "IV",  4 ; "V",  5 ; "IX",  9 |]
    [| "",0 ; "X",  10 ; "XX",  20 ; "XXX",  30 ; "XXXX",  40 ; "XL", 40 ; "L", 50 ; "XC", 90 |]
    [| "",0 ; "C", 100 ; "CC", 200 ; "CCC", 300 ; "CCCC", 400 ; "CD",400 ; "D",500 ; "CM",900 |]
    [| "",0 ; "M",1000 ; "MM",2000 ; "MMM",3000 ; "MMMM",4000                                 |] |]
  let digits = places[place]
  // The following magic numbers are all column numbers.
  for i in 0 .. 4 do
    digits[i]                // "" I II III IIII
  if digits.Length > 5 then
    digits[5]                // IV
    let s5,v5 = digits[6]
    for i in 0 .. 4 do
      let s,v = digits[i]
      s5+s,v5+v              // V VI VII VIII VIIII
    digits[7] }              // IX


type RomanAndExpected = string * RomanInt

let generateGoodInputs()  : RomanAndExpected seq = seq {
  printfn "Good inputs:"
  yield! seq {
    "CDXCIIII",Value 494
    "LXXXXV"  ,Value  95 }
  let rec gen input value place = seq {
    let doDigit digit = seq {
      let s,v = digit
      let input' = input + s
      let value' = value + v
      match place with
      | 0 ->
        if value' <> 0 then
          let expected = Value value'
          yield input',expected
      | _ ->
        yield! gen input' value' (place - 1) }
    for digit in digitsSeq place do
      yield! doDigit digit }
  yield! gen "" 0 placeMax }
//// Ensure no gaps in generated good inputs.
//let _ =
//  let mutable prev = 0
//  let testGood (_,expected) =
//    match expected with
//    | Value n ->
//      if   n > prev + 1 then  printfn "%4d %4d %3d" prev n (n - prev)
////    elif n < prev     then  printfn "%4d %4d %3d" prev n (n - prev)
//      prev <- n
//    | _ -> ()
//  generateGoodInputs() |> Seq.iter testGood


let generateBadInputs()  : RomanAndExpected seq = seq {
  printfn "Bad inputs:"

  yield! seq {
    yield "CCXVeIV",Error (BadChar ("CCXV",'e' ))
    yield ""       ,Error  Empty
    yield "eXV"    ,Error (BadChar (""    ,'e' ))
    yield "e"      ,Error (BadChar (""    ,'e' ))
    yield "Xe"     ,Error (BadChar ("X"   ,'e' ))
    yield "MeV"    ,Error (BadChar ("M"   ,'e' )) }

  yield! seq {
    let badOrder width n digits = seq {
      let f (input: string) =
        let last    = input.Length - 1
        let wid     = width - 1
        let lastMin = min wid (last - n)
        let sPrev = input[0 .. lastMin]
        let c     = input[last]
        let expected = Error (BadOrder (sPrev,c))
        input,expected
      for digit in digits do f digit }
    yield! [| // any of these
        "IIIII" ; "IIIIII" ; "IIIIIII" ; "IIIIIIII"
        "XXXXX" ; "XXXXXX" ; "XXXXXXX" ; "XXXXXXXX"
        "CCCCC" ; "CCCCCC" ; "CCCCCCC" ; "CCCCCCCC" |]
    |> badOrder 4 0
    yield! [| // any of these
      "VIIIII"
      "LXXXXX"
      "DCCCCC" |]
    |> badOrder 5 0
    let a0 = [| // any of these
      ( [| "II" ; "III" ; "IIII" |] , "VXLCDM" )
      ( [| "XX" ; "XXX" ; "XXXX" |] ,   "LCDM" )
      ( [| "CC" ; "CCC" ; "CCCC" |] ,     "DM" ) |]
    yield! seq {
      for place in a0 do
        let placeL,placeR = place
        for   c  in placeR do
          for ii in placeL do
            yield $"{ii}{c}" }
    |> badOrder 4 1 }

  yield! seq {
    let badOrder s1 s2 =
      let input: string = s1 + s2
      let sPrev = input[0 .. 1]
      let c     = input[2]
      let expected = Error (BadOrder (sPrev,c))
      input,expected
    let a1 = [| // any pairing of one from each group within each place
      [| [| "IV" ; "IX"|] ; [| "I" ; "II" ; "III" ; "IIII"|] |]
      [| [| "XL" ; "XC"|] ; [| "X" ; "XX" ; "XXX" ; "XXXX"|] |]
      [| [| "CD" ; "CM"|] ; [| "C" ; "CC" ; "CCC" ; "CCCC"|] |] |]
    for place in a1 do
      let place0 = place[0]
      let place1 = place[1]
      for   r0 in place0 do
        for p1 in place1 do
          yield badOrder r0 p1
    let a2 = [| // any pairing within each place
      [| "IV" ; "V" ; "IX"|]
      [| "XL" ; "L" ; "XC"|]
      [| "CD" ; "D" ; "CM"|] |]
    for place in a2 do
      for   i0 in 0 .. place.Length-1 do
        for i1 in 0 .. place.Length-1 do
          if i0 <> i1 then
            yield badOrder place[i0] place[i1] }

  // any lower place digit followed by a higher place digit, example: "VII" followed by "X"
  yield! seq {
    let allNonBlank place = digitsSeq place |> Seq.skip 1
    let badOrder (sPrev,_) (s: string,_) = seq {
      let input = sPrev + s
      let legal = match input with "IX"|"XC"|"CM" -> true | _ -> false
      if not legal then
        // It’s highly impractical to generate the correct preceding string, so we punt with "".
        let expected = Error (BadOrder ("",s[0])) // "" matches anything
        yield input,expected }
    for   place0 in 0        .. placeMax-1 do
      for place1 in place0+1 .. placeMax   do
        for   digit0 in allNonBlank place0 do
          for digit1 in allNonBlank place1 do
            yield! badOrder digit0 digit1 } }

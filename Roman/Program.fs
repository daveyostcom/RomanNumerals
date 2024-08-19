

open RomanNumerals
open Tests


// Test the parser using the generated input test cases.

type TestMode =
  | PrintFailures
  | PrintAll

let check out testMode result =
  let roman,expected,actual = result
  let isBadOrderMatchAny =
    match expected with
    | Error (BadOrder (s,_)) ->  s = "" // special case means match any
    | _                      ->  false
  let passed = actual = expected  ||  isBadOrderMatchAny 
  if not passed  ||  testMode = PrintAll then
    let ok = if passed then  " √" else  "# "
    let sInput = if roman = "" then  "\"\"" else  roman
    fprintf out $"%s{ok} %-19s{sInput} "
    let printResult result =
      match result with
      | Value v          ->  fprintf out $"= %4d{v}"
      | Error e ->
        match e with
        | BadChar  (s,c) ->  fprintf out $"BadChar  %5s{s} %c{c}"
        | BadOrder (s,c) ->  fprintf out $"BadOrder %5s{s} %c{c}"
        | Empty          ->  fprintf out  "Empty"
    printResult actual
    if not passed then
      fprintf out "   –– expected: "
      printResult expected
    fprintfn out ""


let defaultVariant = 4
let maxTest = 5

let getParserNumberArgs (argv: string array) =
  let success,v =
    let ok = argv.Length >= 1
    if not ok then
      printfn $"\nNo argument given – defaulting to variant {defaultVariant}.\n"
      true, [| defaultVariant |]
    else
      let isNum,n = System.Int32.TryParse argv[0]
      if   isNum           then  true , [| n            |]
      elif argv[0] = "all" then  true , [| 0 .. maxTest |]
                           else  false, [|              |]
  if not success  ||  v[0] < 0  ||  maxTest < v[0] then
    printfn $"\nFirst arg “{v}” must be 0 through {maxTest}: – defaulting to variant {defaultVariant}.\n"
    [| defaultVariant |]
  else
    v

let getParser n =
  match n with
  | 0 -> RomanNumerals0.parseRomanNumeral
  | 1 -> RomanNumerals1.parseRomanNumeral
  | 2 -> RomanNumerals2.parseRomanNumeral
  | 3 -> RomanNumerals3.parseRomanNumeral
  | 4 -> RomanNumerals4.parseRomanNumeral
  | 5 -> RomanNumerals5.parseRomanNumeral
  | _ -> failwithf $"Can’t happen: Invalid variant number: {n}."


type RomanExpectedActual = string * RomanInt * RomanInt

type Output = Stdout | File of string

[<EntryPoint>]
let main argv =
  let testMode = match 1 with | 1 -> PrintFailures | 2 -> PrintAll       | _ -> failwith "bad choice"
  let output   = match 1 with | 1 -> Stdout        | 2 -> File "out.txt" | _ -> failwith "bad choice"
  use writer =
    match output with
    | Stdout    ->  System.Console.Out
    | File path ->  new System.IO.StreamWriter(path)
  printfn "TestMode is %A" testMode
  printfn "Output   is %A" output
  let testParser parser =
    let parseOne (input: RomanAndExpected)  : RomanExpectedActual =
      let roman,expected = input
      let actual = roman |> parser
      roman,expected,actual
    let checkOne (result: RomanExpectedActual)  : unit =
      check writer testMode result
    if true then
      let runAll generator =
        generator()
        |> Seq.map parseOne
        |> Seq.iter checkOne
      runAll generateGoodInputs
      runAll generateBadInputs
    else
      // Uncomment the printfs in the active pattern code
      // to show redundant calls of active patterns.
      seq {
        printfn "CDXCIIII"               // first 3 of 5 patterns
        yield   "CDXCIIII",Value 494
        printfn "LXXXXV"                 // last 2 
        yield   "LXXXXV"  ,Value  95 }
      |> Seq.map parseOne
      |> Seq.iter checkOne
  let numberArgs = getParserNumberArgs argv
  for n in numberArgs do
    if numberArgs.Length > 1 then printfn $"==== Parser {n}"
    let parser = getParser n
    testParser parser
  0

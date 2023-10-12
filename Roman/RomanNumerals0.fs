
module RomanNumerals0

open   RomanNumerals


// This variant does not pass the bad-inputs test.


type Role =
    | R1  // I X C M etc.  
    | R5  // V L D
    | Ro  // other
type FragId =
   // R1  R1   R1     R1    R5  Ro   Ro     place
    | I | II | III | IIII | V | IV | IX  //   0
    | X | XX | XXX | XXXX | L | XL | XC  //   1
    | C | CC | CCC | CCCC | D | CD | CM  //   2
    | M | MM | MMM | MMMM                //   3
let maxFrag  = 4000
let maxPlace = 3
type Frag = { FragId: FragId; Value: int; Role: Role; Place: int }
let makeFrag fragId value role place =
    { FragId=fragId; Value=value; Role=role; Place=place }

let parseRomanNumeral (input: string) : RomanInt =
    let rec parse acc fragPrev charList : RomanInt =
        let take fragId value place role tail =
            let orderIsOk =
                if fragPrev.Place = place then
                    (fragPrev.Role, role) = (R5, R1)
                else
                    fragPrev.Place > place
            if not orderIsOk then
                let sFragId = string fragId
                if sFragId.Length > 1 then
                    let sPrev = (string fragPrev.FragId) + sFragId[0..sFragId.Length-1]
                    let sChar = sFragId[sFragId.Length-1]
                    Error (BadOrder (sPrev, sChar))
                else
                    Error (BadOrder (string fragPrev.FragId, (string fragId)[0]))
            else
                let frag = makeFrag fragId value role place
                let acc' = acc + value
                parse acc' frag tail
        match charList with
        | 'I'::'I'::'I'::'I' :: tail -> take IIII    4 0 R1 tail
        | 'X'::'X'::'X'::'X' :: tail -> take XXXX   40 1 R1 tail
        | 'C'::'C'::'C'::'C' :: tail -> take CCCC  400 2 R1 tail
        | 'M'::'M'::'M'::'M' :: tail -> take MMMM 4000 3 R1 tail
        | 'I'::'I'::'I'      :: tail -> take III     3 0 R1 tail
        | 'X'::'X'::'X'      :: tail -> take XXX    30 1 R1 tail
        | 'C'::'C'::'C'      :: tail -> take CCC   300 2 R1 tail
        | 'M'::'M'::'M'      :: tail -> take MMM  3000 3 R1 tail
        | 'I'::'I'           :: tail -> take II      2 0 R1 tail
        | 'X'::'X'           :: tail -> take XX     20 1 R1 tail
        | 'C'::'C'           :: tail -> take CC    200 2 R1 tail
        | 'M'::'M'           :: tail -> take MM   2000 3 R1 tail
        | 'I'::'V'           :: tail -> take IV      4 0 Ro tail
        | 'I'::'X'           :: tail -> take IX      9 0 Ro tail
        | 'X'::'L'           :: tail -> take XL     40 1 Ro tail
        | 'X'::'C'           :: tail -> take XC     90 1 Ro tail
        | 'C'::'D'           :: tail -> take CD    400 2 Ro tail
        | 'C'::'M'           :: tail -> take CM    900 2 Ro tail
        | 'I'                :: tail -> take I       1 0 R1 tail
        | 'V'                :: tail -> take V       5 0 R5 tail
        | 'X'                :: tail -> take X      10 1 R1 tail
        | 'L'                :: tail -> take L      50 1 R5 tail
        | 'C'                :: tail -> take C     100 2 R1 tail
        | 'D'                :: tail -> take D     500 2 R5 tail
        | 'M'                :: tail -> take M    1000 3 R1 tail
        |  c                 :: _    -> Error (BadChar ("", c))
        | [] when acc = 0            -> Error Empty
        | []                         -> Value acc
    let initialFrag =
        let fragId = M            // Any fragId will do here.
        let value  = maxFrag + 1
        let role   = Ro
        let place  = maxPlace + 1
        makeFrag fragId value role place
    input.ToCharArray()
    |> Array.toList
    |> parse 0 initialFrag

(* output:
TestMode is PrintFailures
Output   is Stdout
Good inputs:
Bad inputs:
#  CCXVeIV             BadChar        e   –– expected: BadChar   CCXV e
#  Xe                  BadChar        e   –– expected: BadChar      X e
#  MeV                 BadChar        e   –– expected: BadChar      M e
#  IIIIII              BadOrder IIIIII I   –– expected: BadOrder  IIII I
#  IIIIIII             BadOrder IIIIIII I   –– expected: BadOrder  IIII I
#  IIIIIIII            BadOrder IIIIIIII I   –– expected: BadOrder  IIII I
#  XXXXXX              BadOrder XXXXXX X   –– expected: BadOrder  XXXX X
#  XXXXXXX             BadOrder XXXXXXX X   –– expected: BadOrder  XXXX X
#  XXXXXXXX            BadOrder XXXXXXXX X   –– expected: BadOrder  XXXX X
#  CCCCCC              BadOrder CCCCCC C   –– expected: BadOrder  CCCC C
#  CCCCCCC             BadOrder CCCCCCC C   –– expected: BadOrder  CCCC C
#  CCCCCCCC            BadOrder CCCCCCCC C   –– expected: BadOrder  CCCC C
#  VIIIII              BadOrder  IIII I   –– expected: BadOrder VIIII I
#  LXXXXX              BadOrder  XXXX X   –– expected: BadOrder LXXXX X
#  DCCCCC              BadOrder  CCCC C   –– expected: BadOrder DCCCC C
#  IVII                BadOrder  IVII I   –– expected: BadOrder    IV I
#  IVIII               BadOrder IVIII I   –– expected: BadOrder    IV I
#  IVIIII              BadOrder IVIIII I   –– expected: BadOrder    IV I
#  IXII                BadOrder  IXII I   –– expected: BadOrder    IX I
#  IXIII               BadOrder IXIII I   –– expected: BadOrder    IX I
#  IXIIII              BadOrder IXIIII I   –– expected: BadOrder    IX I
#  XLXX                BadOrder  XLXX X   –– expected: BadOrder    XL X
#  XLXXX               BadOrder XLXXX X   –– expected: BadOrder    XL X
#  XLXXXX              BadOrder XLXXXX X   –– expected: BadOrder    XL X
#  XCXX                BadOrder  XCXX X   –– expected: BadOrder    XC X
#  XCXXX               BadOrder XCXXX X   –– expected: BadOrder    XC X
#  XCXXXX              BadOrder XCXXXX X   –– expected: BadOrder    XC X
#  CDCC                BadOrder  CDCC C   –– expected: BadOrder    CD C
#  CDCCC               BadOrder CDCCC C   –– expected: BadOrder    CD C
#  CDCCCC              BadOrder CDCCCC C   –– expected: BadOrder    CD C
#  CMCC                BadOrder  CMCC C   –– expected: BadOrder    CM C
#  CMCCC               BadOrder CMCCC C   –– expected: BadOrder    CM C
#  CMCCCC              BadOrder CMCCCC C   –– expected: BadOrder    CM C
#  IVIX                BadOrder  IVIX X   –– expected: BadOrder    IV I
#  VIV                 BadOrder   VIV V   –– expected: BadOrder    VI V
#  VIX                 BadOrder   VIX X   –– expected: BadOrder    VI X
#  IXIV                BadOrder  IXIV V   –– expected: BadOrder    IX I
#  XLXC                BadOrder  XLXC C   –– expected: BadOrder    XL X
#  LXL                 BadOrder   LXL L   –– expected: BadOrder    LX L
#  LXC                 BadOrder   LXC C   –– expected: BadOrder    LX C
#  XCXL                BadOrder  XCXL L   –– expected: BadOrder    XC X
#  CDCM                BadOrder  CDCM M   –– expected: BadOrder    CD C
#  DCD                 BadOrder   DCD D   –– expected: BadOrder    DC D
#  DCM                 BadOrder   DCM M   –– expected: BadOrder    DC M
#  CMCD                BadOrder  CMCD D   –– expected: BadOrder    CM C
*)
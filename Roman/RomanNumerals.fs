
module RomanNumerals

type Error =
  | BadChar  of string * char
  | BadOrder of string * char
  | Empty
type RomanInt =
  | Value of int
  | Error of Error

let placeMax = 3  // place or p indicates decimal place or power of 10


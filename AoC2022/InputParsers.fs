module InputParsers

open System.Text.RegularExpressions

let (|String|_|) (str: string) =
   Some(str)

let (|Char|_|) (str: string) =
   if (str.Length = 1) then Some str[0] else None

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

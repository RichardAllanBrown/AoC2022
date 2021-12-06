module Day3

let getCountOfBitsInCol(bits: string list)(index: int) : int * int =
    let relevantBits = bits |> List.map(fun s -> s[index])
    let zeroCount = relevantBits |> List.filter(fun s -> s.Equals('0')) |> List.length
    let oneCount = relevantBits |> List.filter(fun s -> s.Equals('1')) |> List.length
    zeroCount, oneCount

let getLeastFrequentCharAtIndex(bits: string list)(index: int): char =
    match getCountOfBitsInCol(bits)(index) with
    | (zeroCount, oneCount) when zeroCount < oneCount -> '0'
    | (zeroCount, oneCount) when oneCount < zeroCount -> '1'
    | _ -> '0'

let getMostFrequentCharAtIndex(bits: string list)(index: int) : char =
    match getCountOfBitsInCol(bits)(index) with
    | (zeroCount, oneCount) when zeroCount < oneCount -> '1'
    | (zeroCount, oneCount) when oneCount < zeroCount -> '0'
    | _ -> '1'

let buildCommonCharString(bits: string list) : string =
    [0 .. bits.Head.Length - 1]
    |> List.map(getMostFrequentCharAtIndex(bits))
    |> List.map(string)
    |> String.concat("")

let binaryStringToInt(s: string): int = System.Convert.ToInt32(s, 2)

let switchBinaryChar(c: char): char =
    match c with
    | '0' -> '1'
    | '1' -> '0'
    | _ -> c

let invertBinaryString(s: string): string = 
    s.ToCharArray()
    |> Array.map(switchBinaryChar)
    |> Array.map(string)
    |> String.concat("")

let epsilonRate(s: string): int = s |> invertBinaryString |> binaryStringToInt

let computePowerConsumption(bits: string list): int =
    let commonChars = buildCommonCharString(bits)
    binaryStringToInt(commonChars) * epsilonRate(commonChars)

let computeDay3Part1(): int =
    Utils.readInputFile("./Input/Day3.txt")
    |> computePowerConsumption

let hasCharAtIndex(c: char, i: int)(s: string): bool = c.Equals s[i]

let filterStringWithCommonCharAtIndex(bits: string list)(i :int): string list =
    bits |> List.filter(hasCharAtIndex(getMostFrequentCharAtIndex(bits)(i), i))

let getOxyGenRating(bits: string list): int =
    [0 .. bits.Head.Length - 1]
    |> List.fold(filterStringWithCommonCharAtIndex)(bits)
    |> List.head
    |> binaryStringToInt

let filterStringWithLeastCommonCharAtIndex(bits: string list)(i :int): string list =
    if (bits.Length = 1) then bits
    else bits |> List.filter(hasCharAtIndex(getLeastFrequentCharAtIndex(bits)(i), i))

let getScrubberRating(bits: string list): int =
    [0 .. bits.Head.Length - 1]
    |> List.fold(filterStringWithLeastCommonCharAtIndex)(bits)
    |> List.head
    |> binaryStringToInt

let computeLifeSupportRating(bits: string list): int = getScrubberRating(bits) * getOxyGenRating(bits)

let computeDay3Part2(): int = 
    Utils.readInputFile("./Input/Day3.txt") 
    |> computeLifeSupportRating

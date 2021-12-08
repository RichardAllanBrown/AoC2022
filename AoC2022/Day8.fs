module Day8

let parseLine(s: string): string list = 
    s.Substring(s.IndexOf(" | ")).Split(" ") |> Array.toList

let isDistinctDisplaySignal(s: string): bool =
    [2;3;4;7] |> List.contains(s.Length)

let computeDay8Part1(): int = 
    Utils.readInputFile("./Input/Day8.txt")
    |> List.collect(parseLine)
    |> List.filter(isDistinctDisplaySignal)
    |> List.length

type Signal = 
    { a: bool; b: bool; c: bool; d: bool; e: bool; f: bool; g: bool }

module Signal =
    let parse(s: string): Signal =
        { 
            a = s.Contains('a')
            b = s.Contains('b')
            c = s.Contains('c')
            d = s.Contains('d')
            e = s.Contains('e')
            f = s.Contains('f')
            g = s.Contains('g')
        }

    let unparse(s: Signal): string =
        let sb = new System.Text.StringBuilder()
        if (s.a) then sb.Append('a') |> ignore
        if (s.b) then sb.Append('b') |> ignore
        if (s.c) then sb.Append('c') |> ignore
        if (s.d) then sb.Append('d') |> ignore
        if (s.e) then sb.Append('e') |> ignore
        if (s.f) then sb.Append('f') |> ignore
        if (s.g) then sb.Append('g') |> ignore
        sb.ToString()

module Display = 
    let getDigit(s: Signal): int option =
        match s with
        | { a = true;  b = true;  c = true;  d = false; e = true;  f = true;  g = true  } -> Some 0
        | { a = false; b = false; c = true;  d = false; e = false; f = true;  g = false } -> Some 1
        | { a = true;  b = false; c = true;  d = true;  e = true;  f = false; g = true  } -> Some 2
        | { a = true;  b = false; c = true;  d = true;  e = false; f = true;  g = true  } -> Some 3
        | { a = false; b = true;  c = true;  d = true;  e = false; f = true;  g = false } -> Some 4
        | { a = true;  b = true;  c = false; d = true;  e = false; f = true;  g = true  } -> Some 5
        | { a = true;  b = true;  c = false; d = true;  e = true;  f = true;  g = true  } -> Some 6
        | { a = true;  b = false; c = true;  d = false; e = false; f = true;  g = false } -> Some 7
        | { a = true;  b = true;  c = true;  d = true;  e = true;  f = true;  g = true  } -> Some 8
        | { a = true;  b = true;  c = true;  d = true;  e = false; f = true;  g = true  } -> Some 9
        | _ -> None

    let isValid(s: Signal): bool = getDigit(s).IsSome

module FourDigitDisplay =
    let getNumber(d1: Signal, d2: Signal, d3: Signal, d4: Signal): int = 
        1000 * Display.getDigit(d1).Value +
        100 * Display.getDigit(d2).Value +
        10 * Display.getDigit(d3).Value +
        Display.getDigit(d4).Value

let buildSignalMap(mapping: string)(input: Signal): Signal =
    let source = "abcdefg".ToCharArray()

    let validateMap(s: string) = 
        if source = (s.ToCharArray() |> Array.sort) then ()
        else invalidArg (nameof(mapping)) $"Dest must be a string that contains each of 'abcdefg' exactly once, instead it was {mapping}"
    validateMap(mapping)

    let swap (x, y) = (y, x)
    let toCharArray (s: string) = s.ToCharArray()
    let indexedMapping = mapping.ToCharArray() |> Array.zip(source) |> Array.map(swap) |> Map

    input
    |> Signal.unparse
    |> toCharArray
    |> Array.map(indexedMapping.TryFind)
    |> Array.map(Option.get)
    |> System.String
    |> Signal.parse

let allSignalMaps: (Signal -> Signal) list =
    "abcdefg".ToCharArray()
    |> Array.toList
    |> Utils.permute
    |> List.map(fun chars -> chars |> List.toArray |> System.String)
    |> List.map(buildSignalMap)

let isValidMapping(knownSignals: Signal list)(mapping: Signal -> Signal): bool =
    knownSignals
    |> List.map(mapping)
    |> List.forall(Display.isValid)

let decode(knownSignals: Signal list)(values: Signal list): int =
    let mapping = allSignalMaps |> List.find(isValidMapping(knownSignals))
    let mappedValues = values |> List.map(mapping)
    match mappedValues with
    | [ s1; s2; s3; s4 ] -> FourDigitDisplay.getNumber(s1, s2, s3, s4)
    | _ -> invalidArg (nameof(values)) "Values must have exactly 4 entries"

let parseAndDecode(line: string): int =
    let signals = 
        line.Split([| ' '; '|' |], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map(Signal.parse)
        |> Array.toList
    decode(List.take(10)(signals))(List.skip(10)(signals))

let computeDay8Part2() =
    Utils.readInputFile("./Input/Day8.txt")
    |> List.map(parseAndDecode)
    |> List.sum

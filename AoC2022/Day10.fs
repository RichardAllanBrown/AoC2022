module Day10

let closer(c: char): char =
    match c with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> failwith $"Unrecognised char '{c}'"

let opener(c: char): char =
    match c with
    | ')' -> '('
    | ']' -> '['
    | '}' -> '{'
    | '>' -> '<'
    | _ -> failwith $"Unrecognised char '{c}'"

type ParserStack =
    | Empty 
    | Stack of char * ParserStack

    member s.Handle c = 
        if List.contains(c)(['('; '['; '{'; '<']) then s.HandleOpen(c)
        else s.HandleClose(c)

    member s.HandleOpen(c: char): Result<ParserStack, char> = Ok(Stack(c, s))

    member s.HandleClose(c: char): Result<ParserStack, char> =
        match s with
        | Empty -> failwith "Underflow"
        | Stack(h, tail) when h = opener(c) -> Ok tail
        | Stack(h, _) -> Error c

    member s.ClosersNeeded(): char list =
        match s with
        | Empty -> List.empty
        | Stack(h, tail) -> closer(h) :: tail.ClosersNeeded()

let processStack(line: string): Result<ParserStack, char> =
    let rec helper(s: ParserStack)(remaining: char list): Result<ParserStack, char> =
        match s.Handle(remaining.Head) with
        | Ok s' when remaining.Tail.IsEmpty -> Ok s'
        | Ok s' -> helper(s')(remaining.Tail)
        | Error c -> Error c

    line.ToCharArray()        
    |> Array.toList
    |> helper(ParserStack.Empty)

let getIllegalCharScore(c: Result<ParserStack, char>): int =
    match c with 
    | Error ')' -> 3
    | Error ']' -> 57
    | Error '}' -> 1197
    | Error '>' -> 25137
    | _ -> 0

let computeDay10Part1(): int = 
    Utils.readInputFile("./Input/Day10.txt")
    |> List.map(processStack)
    |> List.sumBy(getIllegalCharScore)

let completionScore(charsNeeded: char list): int64 =
    let scoreCompletion(acc: int64)(c: char): int64 =
        let charScore = 
            match c with
            | ')' -> 1L
            | ']' -> 2L
            | '}' -> 3L
            | '>' -> 4L
            | _ -> failwith $"Unrecognised char '{c}'"
        acc * 5L + charScore
    charsNeeded |> List.fold(scoreCompletion)(0)

let getCompletionScore(r: Result<ParserStack, char>): int64 =
    match r with
    | Ok p -> p.ClosersNeeded() |> completionScore
    | _ -> 0

let getMedianValue values =
    let sortedList = values |> List.sort
    sortedList.[values.Length / 2]

let computeDay10Part2(): int64 =
    Utils.readInputFile("./Input/Day10.txt")
    |> List.map(processStack)
    |> List.filter(function
        | Ok _ -> true
        | Error _ -> false)
    |> List.map(getCompletionScore)
    |> getMedianValue

module Day4

let updateValueInList(value: 'T, index: int)(list: 'T list): 'T list =
    list |> List.mapi(fun i -> fun v -> if (i = index) then value else v)

type BingoBoard =
    { board: int list; matches: bool list }

    static member create(board: int list) : BingoBoard = 
        if (board.Length = 25) then { board = board; matches = List.replicate(25)(false) }
        else invalidArg (nameof board) "Must have exactly 25 items"

    member this.handleCall(number: int) : BingoBoard =
        let index = List.tryFindIndex(fun i -> i = number)(this.board)
        match index with
        | Some(i) -> { this with matches = updateValueInList(true, i)(this.matches) }
        | None -> this

    member this.getIndex(x: int, y: int): int = x * 5 + y
    
    member this.getValueAt(x: int, y: int): int = this.board[ this.getIndex(x, y) ]

    member this.columnHasMatch(y: int): bool = [0..4] |> List.forall(fun x -> this.matches[this.getIndex(x, y)])

    member this.rowHasMatch(x: int): bool = [0..4] |> List.forall(fun y -> this.matches[this.getIndex(x, y)])

    member this.hasWon(): bool = [0..4] |> List.exists(fun z -> this.columnHasMatch(z) || this.rowHasMatch(z))

    member this.hasNotWon(): bool = not(this.hasWon())
        
    member this.sumUnmatchedNumbers(): int = 
        List.zip(this.board)(this.matches)
        |> List.filter(fun (_, m) -> not m)
        |> List.sumBy fst

let rec getWinningBoard(boards: BingoBoard list)(numbers: int list): BingoBoard * int =
    let newBoards = boards |> List.map(fun b -> b.handleCall(numbers.Head))
    let wonBoards = newBoards |> List.filter(fun b -> b.hasWon())
    if wonBoards.IsEmpty then getWinningBoard(newBoards)(numbers.Tail)
    else (wonBoards.Head, numbers.Head)

let rec getLastWinningBoard(boards: BingoBoard list)(numbers: int list): BingoBoard * int =
    let newBoards = boards |> List.map(fun b -> b.handleCall(numbers.Head))
    if (newBoards.Length = 1 && newBoards.Head.hasWon()) then (newBoards.Head, numbers.Head)
    else getLastWinningBoard(newBoards |> List.filter(fun b -> b.hasNotWon()))(numbers.Tail)

let splitToNumberList(str: string): int list = 
    str.Split([|','|], System.StringSplitOptions.TrimEntries) 
    |> Array.map(int)
    |> Array.toList

let buildBoard(lines: string list): BingoBoard =
    lines
    |> List.collect(fun row -> row.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList)
    |> List.map(int)
    |> BingoBoard.create

let parseFile(lines: string list): BingoBoard list * int list =
    let boards = 
        lines.Tail
        |> List.filter(fun str -> str.Length > 0)
        |> List.chunkBySize(5)
        |> List.map(buildBoard)
    let numbers = splitToNumberList(lines.Head)
    (boards, numbers)

let computeDay4Part1(): int =
    let (boards, numbers) = Utils.readInputFile("./Input/Day4.txt") |> parseFile
    let (winningBoard, lastCalledNumber) = getWinningBoard(boards)(numbers)
    winningBoard.sumUnmatchedNumbers() * lastCalledNumber

let computeDay4Part2(): int =
    let (boards, numbers) = Utils.readInputFile("./Input/Day4.txt") |> parseFile
    let (winningBoard, lastCalledNumber) = getLastWinningBoard(boards)(numbers)
    winningBoard.sumUnmatchedNumbers() * lastCalledNumber

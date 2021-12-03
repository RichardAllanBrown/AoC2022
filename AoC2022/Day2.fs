module Day2

type Move = 
  Forward of int
  | Up of int
  | Down of int

let parseMove (move: string) =
    let splitMove = move.Split(' ')
    let move = splitMove[0]
    let distance = int splitMove[1]
    match move with 
    | "forward" -> Some(Move.Forward(distance))
    | "up" -> Some(Move.Up(distance))
    | "down" -> Some(Move.Down(distance))
    | _ -> None

type SubPosition = 
    { horizontal: int; depth: int; aim: int }
    static member Initial = { horizontal = 0; depth = 0; aim = 0 }

let applyMove (state: SubPosition)(move: Move) : SubPosition =
    match move with
    | Forward i -> { state with horizontal = state.horizontal + i }
    | Up i -> { state with depth = state.depth - i }
    | Down i -> { state with depth = state.depth + i }

let applyAdvancedMove (state: SubPosition)(move: Move) : SubPosition =
    match move with
    | Forward i -> { state with horizontal = state.horizontal + i; depth = state.depth + state.aim * i }
    | Up i -> { state with aim = state.aim - i }
    | Down i -> { state with aim = state.aim + i }

let processPilotingMoves (moveRules: SubPosition -> Move -> SubPosition)(moves: string list) : int =
    moves
    |> List.map(parseMove)
    |> List.collect(Option.toList)
    |> List.fold(moveRules)(SubPosition.Initial)
    |> fun m -> m.horizontal * m.depth

let solveDay2Part1 () : int = 
    Utils.readInputFile("./Input/Day2.txt") 
    |> processPilotingMoves(applyMove)

let solveDay2Part2 () : int = 
    Utils.readInputFile("./Input/Day2.txt") 
    |> processPilotingMoves(applyAdvancedMove)

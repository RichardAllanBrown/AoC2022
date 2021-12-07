module Day6

let computeLanternfish(state: int64 array)(day: int): int64 array =
    printfn "Computing for day %d" day
    [|
        // Basically shunt the array along one, wrapping the last value around
        state[1];
        state[2];
        state[3];
        state[4];
        state[5];
        state[6];
        state[7] + state[0]; // Lanternfish don't die and make a new fish every 6 days 
        state[8];
        state[0]; // Breed new lanternfish
    |]

let runSimulation(days: int)(state: int64 array): int64 =
    [ 1 .. days ]
    |> List.fold(computeLanternfish)(state)
    |> Array.sum

let parseFile(s: string): int64 array =
    let values = s.Split(',') |> Array.map(int)
    [ 0..8 ] 
    |> List.map(fun i -> (Array.filter(fun f -> f = i)(values)).LongLength) 
    |> List.toArray

let computeDay6Part1(): int64 =
    Utils.readInputFile("./Input/Day6.txt")
    |> List.head
    |> parseFile
    |> runSimulation(80)
    
let computeDay6Part2(): int64 =
    Utils.readInputFile("./Input/Day6.txt")
    |> List.head
    |> parseFile
    |> runSimulation(256)
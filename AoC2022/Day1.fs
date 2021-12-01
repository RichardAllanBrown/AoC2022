module Day1

let countDeeperMeasurements (depths: int list): int =
    depths
    |> List.windowed(2)
    |> List.filter(fun ls -> ls[0] < ls[1])
    |> List.length

let solveDay1Part1 (): int =
    Utils.readInputFile("./Input/Day1.txt")
    |> List.map(int)
    |> countDeeperMeasurements

let countDeeperWithNoiseReducation (depths: int list): int =
    depths
    |> List.windowed(3)
    |> List.map(List.sum)
    |> countDeeperMeasurements

let solveDay2Part2 (): int =
    Utils.readInputFile("./Input/Day1.txt")
    |> List.map(int)
    |> countDeeperWithNoiseReducation
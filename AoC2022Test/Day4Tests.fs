module Day4Tests

open System
open Xunit
open Day4

[<Fact>]
let ``Parses file sensibly`` () =
    let (boards, numbers) = Utils.readInputFile("./Input/Day4.txt") |> parseFile
    Assert.Equal(100, boards.Length)
    Assert.Equal(100, numbers.Length)

[<Fact>]
let ``Computes correct result for Day 4 Part 1`` () =
    Assert.Equal(74320, computeDay4Part1())

module Tests

open System
open Xunit
open Day1

[<Fact>]
let ``Computes correct result for Day 1 Part 1`` () =
    Assert.Equal(1316, solveDay1Part1())

[<Fact>]
let ``Computes correct result for Day 1 Part 2`` () =
    Assert.Equal(1344, solveDay2Part2())

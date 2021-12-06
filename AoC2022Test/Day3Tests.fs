module Day3Tests

open System
open Xunit
open Day3

[<Fact>]
let ``Computes correct result for Day 3 Part 1`` () =
    Assert.Equal(3148794, computeDay3Part1())

[<Fact>]
let ``Computes correct result for Day 3 Part 2`` () =
    Assert.Equal(2795310, computeDay3Part2())

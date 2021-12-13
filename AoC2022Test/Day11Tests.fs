module Day11Tests

open Xunit
open Day11

[<Fact>]
let ``Computes day 11 part 1 correctly`` () =
    Assert.Equal(1749, computeDay11Part1())

[<Fact>]
let ``Computes day 11 part 2 correctly`` () =
    Assert.Equal(285, computeDay11Part2())

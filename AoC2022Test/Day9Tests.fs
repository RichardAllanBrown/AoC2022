module Day9Tests

open Xunit
open Day9

[<Fact>]
let ``Computes day 9 part 1 correctly`` () =
    Assert.Equal(512, computeDay9Part1())

[<Fact>]
let ``Computes day 9 part 2 correctly`` () =
    Assert.Equal(1600104, computeDay9Part2())
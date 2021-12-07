module Day6Tests

open Xunit
open Day6

[<Fact>]
let ``Computes day 6 part 1 correctly`` () =
    Assert.Equal(365862L, computeDay6Part1())

[<Fact>]
let ``Computes day 6 part 2 correctly`` () =
    Assert.Equal(1653250886439L, computeDay6Part2())
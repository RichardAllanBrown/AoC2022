module Day7Tests

open Xunit
open Day7

[<Fact>]
let ``Computes day 7 part 1 correctly`` () =
    Assert.Equal(347449, computeDay7Part1())

[<Fact>]
let ``Computes day 7 part 2 correctly`` () =
    Assert.Equal(98039527, computeDay7Part2())

module Day14Tests

open Xunit
open Day14

[<Fact>]
let ``Computes day 14 part 1 correctly`` () =
    Assert.Equal(2915, computeDay14Part1())

[<Fact>]
let ``Computes day 14 part 2 correctly`` () =
    Assert.Equal(0, computeDay14Part2())

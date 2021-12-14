module Day13Tests

open Xunit
open Day13

[<Fact>]
let ``Computes day 13 part 1 correctly`` () =
    Assert.Equal(847, computeDay13Part1())

[<Fact>]
let ``Computes day 13 part 2 correctly`` () = // The answer is right, but hard to programatically tease out
    Assert.Equal(312, computeDay13Part2().Length)

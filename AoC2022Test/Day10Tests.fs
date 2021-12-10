module Day10Tests

open Xunit
open Day10

[<Fact>]
let ``Computes day 10 part 1 correctly`` () =
    Assert.Equal(364389, computeDay10Part1())

[<Fact>]
let ``Computes closers properly`` () =
    let p = 
        match processStack("[({(<(())[]>[[{[]{<()<>>") with
        | Ok p -> p
        | _ -> failwith "Should not be here"
    Assert.Equal("}}]])})]".ToCharArray(), p.ClosersNeeded())

[<Fact>]
let ``Computes closing scores`` () =
    Assert.Equal(288957L, completionScore("}}]])})]".ToCharArray() |> Array.toList))

[<Fact>]
let ``Get median value works`` () =
    Assert.Equal(6, getMedianValue([ 9; 1; 6; 3; 8; ]))

[<Fact>]
let ``Computes day 10 part 2 correctly`` () =
    Assert.Equal(2870201088L, computeDay10Part2())

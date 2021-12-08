module Day2Tests

open Xunit
open Day2

[<Fact>]
let ``Updates state for a basic forward move`` () =
    Assert.Equal({ horizontal = 6; depth = 0; aim = 0 }, applyMove(SubPosition.Initial)(Forward(6)))

[<Fact>]
let ``Updates state for a basic down move`` () =
    Assert.Equal({ horizontal = 0; depth = 3; aim = 0 }, applyMove(SubPosition.Initial)(Down(3)))

[<Fact>]
let ``Computes correct result for Day 2 Part 1`` () =
    Assert.Equal(1451208, solveDay2Part1())

[<Fact>]
let ``Computes correct result for Day 2 Part 2`` () =
    Assert.Equal(1620141160, solveDay2Part2())

module Day8Tests

open Xunit
open Day8

[<Fact>]
let ``Computes day 8 part 1 correctly`` () =
    Assert.Equal(387, computeDay8Part1())

[<Fact>]
let ``Can decode example correctly`` () =
    //acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
    let knownSignals = [
        Signal.parse("acedgfb")
        Signal.parse("cdfbe")
        Signal.parse("gcdfa")
        Signal.parse("fbcad")
        Signal.parse("dab")
        Signal.parse("cefabd")
        Signal.parse("cdfgeb")
        Signal.parse("eafb")
        Signal.parse("cagedb")
        Signal.parse("ab")
    ]
    let values = [
        Signal.parse("cdfeb")
        Signal.parse("fcadb")
        Signal.parse("cdfeb")
        Signal.parse("cdbaf")
    ]
    Assert.Equal(5353, decode(knownSignals)(values))

[<Fact>]
let ``Signal map converts signals`` () =
    let mapping = buildSignalMap("gfedcba")
    Assert.Equal(Signal.parse("d"), mapping(Signal.parse("d")))
    Assert.Equal(Signal.parse("abcdefg"), mapping(Signal.parse("abcdefg")))
    Assert.Equal(Signal.parse("abc"), mapping(Signal.parse("fge")))

let validSignals = [
    Signal.parse("abcefg")  // 0
    Signal.parse("cf")      // 1
    Signal.parse("acdeg")   // 2
    Signal.parse("acdfg")   // 3
    Signal.parse("bcdf")    // 4
    Signal.parse("abdfg")   // 5
    Signal.parse("abdefg")  // 6
    Signal.parse("acf")     // 7
    Signal.parse("abcdefg") // 8
    Signal.parse("abcdfg")  // 9
]

[<Fact>]
let ``All valid signals are validated by the display`` () =
    Assert.True(validSignals |> List.forall(Display.isValid))

[<Fact>]
let ``Can check if a mapping is valid`` () =
    Assert.True(isValidMapping(validSignals)(buildSignalMap("abcdefg"))) // This identity map

[<Fact>]
let ``Computes day 8 part 2 correctly`` () =
    Assert.Equal(986034, computeDay8Part2())

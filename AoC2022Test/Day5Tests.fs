module Day5Tests

open Xunit
open Day5

[<Fact>]
let ``Computes lines points correctly`` () =
    let line = { source = { x=0; y=9 }; dest = { x=5; y=9 } }
    let expectedPointsCovered = seq {
        { x=0; y=9 };
        { x=1; y=9 };
        { x=2; y=9 };
        { x=3; y=9 };
        { x=4; y=9 };
        { x=5; y=9 };
    }
    Assert.Equal<Set<Point>>(Set.ofSeq(expectedPointsCovered), line.coversPoints())

[<Fact>]
let ``Computes lines points correctly backwards`` () =
    let line = { source = { x=5; y=9 }; dest = { x=0; y=9 } }
    let expectedPointsCovered = seq {
        { x=0; y=9 };
        { x=1; y=9 };
        { x=2; y=9 };
        { x=3; y=9 };
        { x=4; y=9 };
        { x=5; y=9 };
    }
    Assert.Equal<Set<Point>>(Set.ofSeq(expectedPointsCovered), line.coversPoints())

[<Fact>]
let ``Computes lines points correctly vertically`` () =
    let line = { source = { x=5; y=2 }; dest = { x=5; y=4 } }
    let expectedPointsCovered = seq {
        { x=5; y=2 };
        { x=5; y=3 };
        { x=5; y=4 };
    }
    Assert.Equal<Set<Point>>(Set.ofSeq(expectedPointsCovered), line.coversPoints())

[<Fact>]
let ``Computes intersects correctly when none is present`` () =
    let line1 = { source = { x=7; y=0 }; dest = { x=7; y=4 } } 
    let line2 = { source = { x=9; y=4 }; dest = { x=3; y=4 } } 
    Assert.Equal<List<Point>>([ { x=7; y=4 } ], line1.intersects(line2))

[<Fact>]
let ``Computes intersects correctly when one is present`` () =
    let line1 = { source = { x=1; y=0 }; dest = { x=1; y=4 } } 
    let line2 = { source = { x=1; y=4 }; dest = { x=3; y=4 } } 
    Assert.Equal<List<Point>>([ { x=1; y=4 } ], line1.intersects(line2))

[<Fact>]
let ``Computes intersects correctly when two lines start and stop in same place`` () =
    let line1 = { source = { x=9; y=4 }; dest = { x=3; y=4 } } 
    let line2 = { source = { x=3; y=4 }; dest = { x=1; y=4 } } 
    Assert.Equal<List<Point>>([ { x=3; y=4 } ], line1.intersects(line2))

[<Fact>]
let ``Computes example correctly`` () =
    let lines = [ 
        { source = { x=0; y=9 }; dest = { x=5; y=9 } }
        { source = { x=9; y=4 }; dest = { x=3; y=4 } }
        { source = { x=2; y=2 }; dest = { x=2; y=1 } }
        { source = { x=7; y=0 }; dest = { x=7; y=4 } }
        { source = { x=0; y=9 }; dest = { x=2; y=9 } } 
        { source = { x=3; y=4 }; dest = { x=1; y=4 } }
    ]
    Assert.Equal(5, countOverlaps(lines))

[<Fact>]
let ``Computes example correctly with diagonals`` () =
    let lines = [ 
        { source = { x=0; y=9 }; dest = { x=5; y=9 } }
        { source = { x=8; y=0 }; dest = { x=0; y=8 } } 
        { source = { x=9; y=4 }; dest = { x=3; y=4 } } 
        { source = { x=2; y=2 }; dest = { x=2; y=1 } } 
        { source = { x=7; y=0 }; dest = { x=7; y=4 } } 
        { source = { x=6; y=4 }; dest = { x=2; y=0 } } 
        { source = { x=0; y=9 }; dest = { x=2; y=9 } } 
        { source = { x=3; y=4 }; dest = { x=1; y=4 } } 
        { source = { x=0; y=0 }; dest = { x=8; y=8 } } 
        { source = { x=5; y=5 }; dest = { x=8; y=2 } } 
    ]
    Assert.Equal(12, countOverlaps(lines))

[<Fact>]
let ``Computes correct result for Day 5 Part 1`` () =
    Assert.Equal(7142, computeDay5Part1())

[<Fact>]
let ``Computes correct result for Day 5 Part 2`` () =
    Assert.Equal(20012, computeDay5Part2())

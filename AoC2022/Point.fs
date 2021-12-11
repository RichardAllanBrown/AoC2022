﻿module Point

type Point = 
    { x: int; y: int }

module Point =
    let getCardinalNeighbours(p: Point): Point list =
        [
            { x = p.x; y = p.y + 1 }
            { x = p.x; y = p.y - 1 }
            { x = p.x + 1; y = p.y }
            { x = p.x - 1; y = p.y }
        ]

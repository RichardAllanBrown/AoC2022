module Day15

open Point
open Grid


// We're path finding with weightings, so let's use Dijkstra's (https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)

//1  function Dijkstra(Graph, source):
//2      dist[source] ← 0                           // Initialization
//3
//4      create vertex priority queue Q
//5
//6      for each vertex v in Graph:          
//7          if v ≠ source
//8              dist[v] ← INFINITY                 // Unknown distance from source to v
//9              prev[v] ← UNDEFINED                // Predecessor of v
//10
//11         Q.add_with_priority(v, dist[v])
//12
//13
//14     while Q is not empty:                      // The main loop
//15         u ← Q.extract_min()                    // Remove and return best vertex
//16         for each neighbor v of u:              // only v that are still in Q
//17             alt ← dist[u] + length(u, v)
//18             if alt < dist[v]
//19                 dist[v] ← alt
//20                 prev[v] ← u
//21                 Q.decrease_priority(v, alt)
//22
//23     return dist, prev

let dijkstra(g: int Grid, s: Point) =
    let mutable dist = Grid.createSameSize(g)(0)
    let mutable prev = Grid.createSameSize(g)(None)
    let mutable queue = PriorityQueue<Point>()

    // todo: does this always work...
    let getLength p1 p2 = Grid.get(g)(p1)

    for p in Grid.getAllPoints(g) do
        if (p = s) then () else
            let dist = Grid.set(dist)(p)(System.Int32.MaxValue)
            let prev = Grid.set(prev)(p)(None)
            ()

        queue.addWithPriority(p, Grid.get(dist)(p))

    while not(queue.isEmpty()) do
        let u = queue.getMin()
        for v in Grid.getCardinalNeighbouringPoints(g)(u) do
            let alt = Grid.get(dist)(u) + getLength u v
            if (alt < Grid.get(dist)(v)) then
                let dist = Grid.set(dist)(v)(alt)
                let prev = Grid.set(prev)(v)(Some(u))
                queue.setPriority(v, alt)
                () else ()

    (dist, prev)

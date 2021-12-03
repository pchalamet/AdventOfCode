module Day3

type Direction =
    | Down
    | Left
    | Right
    | Up
with
    member this.Next () =
        match this with
        | Up -> Left
        | Left -> Down
        | Down -> Right
        | Right -> Up

type Coord =
    { X : int
      Y : int }
with
    member this.Shift direction =
        match direction with
        | Down -> { this with Y = this.Y - 1 }
        | Left -> { this with X = this.X + 1 }
        | Right -> { this with X = this.X - 1 }
        | Up -> { this with Y = this.Y + 1 }

    member this.Distance (other : Coord) =
        { X = other.X - this.X; Y = other.Y - this.Y }


// ----------------------------------------------------------------------------------------

let mutable sumMap = Array2D.create 0 0 0



let adjacentSum (coord : Coord) =
    let adjSum = sumMap.[coord.X, coord.Y] 
                    + sumMap.[coord.X, coord.Y + 1] 
                    + sumMap.[coord.X + 1, coord.Y + 1]
                    + sumMap.[coord.X + 1, coord.Y]
                    + sumMap.[coord.X + 1, coord.Y - 1]
                    + sumMap.[coord.X, coord.Y - 1]
                    + sumMap.[coord.X - 1, coord.Y - 1]
                    + sumMap.[coord.X - 1, coord.Y]
                    + sumMap.[coord.X - 1, coord.Y + 1]
    sumMap.[coord.X, coord.Y] <- adjSum
    adjSum

let rec moveMax (coord : Coord) max (direction : Direction) =
    let adjSum = adjacentSum coord
    if max <= adjSum then
        coord, adjSum
    else
        let newCoord, newDirection = 
            let newDirection = direction.Next()
            let newCoord = newDirection |> coord.Shift
            if sumMap.[newCoord.X, newCoord.Y] = 0 then newCoord, newDirection
            else direction |> coord.Shift, direction
        moveMax newCoord max newDirection

let maxDistance input =
    sumMap <- Array2D.create 1000 1000 0
    let origin = { X = sumMap.GetLength(0) / 2; Y = sumMap.GetLength(1) / 2 }
    sumMap.[origin.X, origin.Y] <- 1
    let coord, max = moveMax origin input Up
    origin.Distance coord, max
   

let Part2 () =
    let input = 277678
    let res = maxDistance input
    printfn "%A -> %A" input res


// ----------------------------------------------------------------------------------------

let mutable freeMap = Array2D.create 0 0 true

let rec moveFree (coord : Coord) count (direction : Direction) =
    freeMap.[coord.X, coord.Y] <- false
    if count = 1 then
        coord
    else
        let newCoord, newDirection = 
            let newDirection = direction.Next()
            let newCoord = newDirection |> coord.Shift
            if freeMap.[newCoord.X, newCoord.Y] then newCoord, newDirection
            else direction |> coord.Shift, direction
        moveFree newCoord (count-1) newDirection

let manhattanDistance input =
    freeMap <- Array2D.create 1000 1000 true
    let origin = { X = freeMap.GetLength(0) / 2; Y = freeMap.GetLength(1) / 2 }
    let coord = moveFree origin input Up
    let vector = origin.Distance coord
    (vector.X |> abs) + (vector.Y |> abs)

let Part1() =
    let input = 277678
    let res = manhattanDistance input
    printfn "%A -> %A" input res

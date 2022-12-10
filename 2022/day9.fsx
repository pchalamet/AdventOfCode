#load "Helpers.fsx"
open Helpers

let inputfile = "inputs/day9.input"

let score nbKnots =
    let lines =
        inputfile
        |> readlines
    
    let pos = Array.create nbKnots (0,0)
    let mutable visits = set [0, 0]

    let move mx my steps =
        let add (x,y) (dx,dy) = (x+dx, y+dy)
        let neg (x,y) = (-x, -y)
        let abs (x,y) = (abs x, abs y)
        let move index =
            let dx, dy = add pos[index] (neg pos[index+1])
            let adx, ady = abs (dx, dy)
            let mad = max adx ady
            let dx = if mad > 1 && adx > 0 then dx/adx else 0
            let dy = if mad > 1 && ady > 0 then dy/ady else 0
            pos[index+1] <- add pos[index+1] (dx, dy)

        for _ in 1..steps do
            pos[0] <- add pos[0] (mx, my)
            for index in 0..nbKnots-2 do
                move index
            visits <- visits |> Set.add pos[nbKnots-1]

    for line in lines do
        match line with
        | Regex "^R (\d+)$" [steps] -> steps |> int |> move 1 0
        | Regex "^L (\d+)$" [steps] -> steps |> int |> move -1 0
        | Regex "^U (\d+)$" [steps] -> steps |> int |> move 0 1
        | Regex "^D (\d+)$" [steps] -> steps |> int |> move 0 -1
        | _ -> failwith "Invalid match"

    visits |> Set.count

let part1() =
    score 2
    |> print1

let part2() =
    score 10
    |> print2

part1()
part2()

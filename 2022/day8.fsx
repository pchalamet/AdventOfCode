#load "Helpers.fsx"
open Helpers

let inputfile = "inputs/day8.input"


let score () =
    let forest =
        inputfile
        |> readlines
        |> Array.map (fun x -> x.ToCharArray())

    let width = forest.Length
    let height = forest[0].Length

    let isVisible i j =
        let rec isVisible i j di dj h dist =
            let ni, nj = i+di, j+dj
            if ni < 0 || ni >= width || nj < 0 || nj >= height then true, dist
            elif forest[ni][nj] < h then isVisible ni nj di dj (max h (forest[ni][nj])) (dist+1)
            else false, dist+1
        let north, dnorth = isVisible i j -1 0 (forest[i][j]) 0
        let south, dsouth = isVisible i j 1 0 (forest[i][j]) 0
        let west, dwest = isVisible i j 0 -1 (forest[i][j]) 0
        let east, deast = isVisible i j 0 1 (forest[i][j]) 0
        let visible = north || south || west || east
        let dist = dnorth * dsouth * dwest * deast
        visible, dist

    [0..width-1] |> List.map (fun i -> [0..height-1] |> List.map (fun j -> isVisible i j))

let part1() =
    let visibility (visible, _) =
         visible ? (1, 0)

    score()
    |> List.sumBy (List.sumBy visibility)
    |> print1

let part2() =
    let visibility (_, dist) = dist

    score()
    |> List.map (fun l -> l |> List.map visibility |> List.max) |> List.max
    |> print2

part1()
part2()

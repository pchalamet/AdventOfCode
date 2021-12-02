open System
open System.IO


let readFile filename = 
    filename |> File.ReadAllLines |> Seq.map int |> List.ofSeq

let puzzle1() =
    let count = readFile "Input1.txt" 
                |> List.pairwise 
                |> List.filter (fun (x, y) -> x < y) 
                |> List.length

    printfn $"Puzzle 1: {count}"


let puzzle2() =
    let values0 = readFile "Input1.txt"
    let values1 = 0 :: values0 @ [ 0 ]
    let values2 = 0 :: 0 :: values0
    let values0 = values0 @ [ 0; 0 ]

    let count = List.zip3 values2 values1 values0 
                |> List.skip 2
                |> List.rev
                |> List.skip 2
                |> List.rev
                |> List.map (fun (x, y, z) -> x + y + z)
                |> List.pairwise
                |> List.filter (fun (x, y) -> x < y) 
                |> List.length

    printfn $"Puzzle 2: {count}"

puzzle1()
puzzle2()

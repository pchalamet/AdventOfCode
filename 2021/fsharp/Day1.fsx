open System.IO

let readFile filename = 
    filename |> File.ReadAllLines |> Seq.map int


let puzzle1() =
    let count = readFile "Input1.txt" 
                |> Seq.pairwise 
                |> Seq.filter (fun (x, y) -> x < y) 
                |> Seq.length

    printfn $"Puzzle 1: {count}"


let puzzle2() =
    let count = readFile "Input1.txt"
                |> Seq.windowed 3
                |> Seq.map (fun [| x; y; z |] -> x + y + z)
                |> Seq.pairwise
                |> Seq.filter (fun (x, y) -> x < y) 
                |> Seq.length

    printfn $"Puzzle 2: {count}"

puzzle1()
puzzle2()

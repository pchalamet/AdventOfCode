open System.IO

let readFile filename =
    filename |> File.ReadAllLines |> Seq.map int


let puzzle1 fileName =
    let count =
        readFile fileName
        |> Seq.pairwise
        |> Seq.filter (fun (x, y) -> x < y)
        |> Seq.length

    printfn $"Puzzle 1: {count}"


let puzzle2 fileName =
    let count =
        readFile fileName
        |> Seq.windowed 3
        |> Seq.map (fun [| x; y; z |] -> x + y + z)
        |> Seq.pairwise
        |> Seq.filter (fun (x, y) -> x < y)
        |> Seq.length

    printfn $"Puzzle 2: {count}"

puzzle1 "../Input1.txt"
puzzle2 "../Input1.txt"

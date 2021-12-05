open System.IO

type Move =
    | Up of int
    | Down of int
    | Forward of int


let toMove (x: string) =
    match x.Split(" ") with
    | [| "up"; x |] -> x |> int |> Up
    | [| "down"; x |] -> x |> int |> Down
    | [| "forward"; x |] -> x |> int |> Forward
    | _ -> failwith $"Unknown move: {x}"

let readFile fileName =
    fileName |> File.ReadAllLines |> Seq.map toMove


let puzzle1 fileName =
    let doMove pos depth (m: Move) =
        match m with
        | Up x -> pos, depth - x
        | Down x -> pos, depth + x
        | Forward x -> pos + x, depth

    let (pos, depth) =
        readFile fileName
        |> Seq.fold (fun (pos, depth) m -> doMove pos depth m) (0, 0)

    let res = pos * depth
    printfn $"Puzzle1: {res}"

let puzzle2 fileName =
    let doMove aim pos depth (m: Move) =
        match m with
        | Up x -> aim - x, pos, depth
        | Down x -> aim + x, pos, depth
        | Forward x -> aim, pos + x, depth + aim * x

    let (_, pos, depth) =
        readFile fileName
        |> Seq.fold (fun (aim, pos, depth) m -> doMove aim pos depth m) (0, 0, 0)

    let res = pos * depth
    printfn $"Puzzle2: {res}"


puzzle1 "../Input2.txt"
puzzle2 "../Input2.txt"

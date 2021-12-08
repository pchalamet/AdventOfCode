
open System.IO

let readFile fileName =
    let content = fileName |> File.ReadAllText
    let values = content.Split(",") |> Seq.map (fun x -> x |> int) |> List.ofSeq
    values


let computeFuelPerPosition dist crabspos target =
    let fuel pos = pos - target |> abs |> dist
    crabspos |> List.map fuel |> List.sum


let puzzle1 fileName =
    let values = readFile fileName
    let min = values |> List.min
    let max = values |> List.max
    let res = [min..max] |> List.map (computeFuelPerPosition id values) |> List.min
    printfn $"Puzzle1: {res}"



let puzzle2 fileName =
    let values = readFile fileName
    let min = values |> List.min
    let max = values |> List.max
    let dist n = (n * (n+1)) / 2
    let res = [min..max] |> List.map (computeFuelPerPosition dist values) |> List.min
    printfn $"Puzzle2: {res}"


puzzle1 "../Input7.txt"
puzzle2 "../Input7.txt"

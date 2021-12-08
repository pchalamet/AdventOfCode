
open System.IO


let readFile fileName =
    let content = fileName |> File.ReadAllText
    content.Split(",") |> Seq.map (fun x -> x |> int) |> List.ofSeq


let ageLanternfishes (lanternfishes: (int * int64) list) =
    let newLanterfishes = lanternfishes |> List.collect (fun (age, count) -> match age with
                                                                             | 0 -> [(6, count); (8, count) ]
                                                                             | x -> [x-1, count])
                                        |> List.groupBy (fun (age, _) -> age)
                                        |> List.map (fun (age, lst) -> age, lst |> List.sumBy (fun (_, count) -> count))
    newLanterfishes

let countLanternfishes days (lanternfishes: int list) =
    let initialFishes = lanternfishes |> List.map (fun x -> x, 1L)
    let finalFishes = [1..days] |> List.fold (fun acc day -> ageLanternfishes acc) initialFishes
    let res = finalFishes |> List.sumBy (fun (age, count) -> count)
    res

let puzzle1 fileName =
    let ages = readFile fileName
    let res = ages |> countLanternfishes 80
    printfn $"Puzzle1: {res}"

let puzzle2 fileName =
    let ages = readFile fileName
    let res = ages |> countLanternfishes 256
    printfn $"Puzzle2: {res}"

puzzle1 "../Input6.txt"
puzzle2 "../Input6.txt"


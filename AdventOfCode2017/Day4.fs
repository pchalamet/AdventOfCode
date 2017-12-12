module Day4
open System.Linq
open System


let input2 = "aa bb cc dd aa"

// --------------------------------------------------------------------------------------------------------------

let reorderWords (line : string seq) =
    let orderWord (word : string) =
        word |> seq |> Seq.sort |> Seq.toArray |> System.String
    let res = line |> Seq.map orderWord
    res

let Part2 () =
    let lines = System.IO.File.ReadAllLines("data/Day4.txt")
    let valid = lines |> Seq.fold (fun s t -> let items = t.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                                                            |> reorderWords
                                              s + (items.Distinct().Count() / items.Count())) 0
    printfn "%d" valid



// --------------------------------------------------------------------------------------------------------------

let Part1 () =
    let lines = System.IO.File.ReadAllLines("data/Day4.txt")
    let valid = lines |> Seq.fold (fun s t -> let items = t.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                                              s + (items.Distinct().Count() / items.Count())) 0
    printfn "%d" valid


module Day2
open System.Linq
open System



// ------------------------------------------------------------------------------------

let input = System.IO.File.ReadAllText("data/Day2.txt")


// ------------------------------------------------------------------------------------

let convert (line : string) =
    line.Split([|'\t'; ' '|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun x -> x |> int)

let Part1 () =
    let lines = input.Split('\n')
    let findMinMax s = 
        let il = s |> convert
        il.Min(), il.Max()

    let diff (min,max) = max - min

    let res = lines |> Seq.fold (fun s t -> s + (t |> findMinMax |> diff)) 0
    printfn "Checksum is %A" res


// ------------------------------------------------------------------------------------

let Part2 () =
    let lines = input.Split('\n')
    let findDiv s = 
        let il = s |> convert |> List.ofSeq
        let mutable res = 0
        for ia in 0..il.Length-1 do
            for ib in 0..il.Length-1 do
                if ia <> ib then
                    if il.[ia] % il.[ib] = 0 then 
                        res <- il.[ia] / il.[ib]
        res

    let res = lines |> Seq.fold (fun s t -> s + (t |> findDiv)) 0
    printfn "Checksum is %A" res


open System.IO


let readFile fileName = fileName |> File.ReadAllLines |> List.ofSeq

let computeGamma (values: string list) =
    let mutable gamma = ""
    let bitMax = values[0].Length - 1
    for idx in 0..bitMax do
        let counts = values |> List.countBy (fun x -> x[idx]) |> Map.ofList
        let bitG = 
            match counts |> Map.tryFind '0', counts |> Map.tryFind '1' with
            | Some c0, Some c1 -> if c0 <= c1 then "1" else "0"
            | None, _ -> "0"
            | _, _ -> "1"
        gamma <- gamma + bitG
    gamma

let negate (value: string) = value.Replace("0", "X").Replace("1", "0").Replace("X", "1")

let bin2dec v = System.Convert.ToInt32(v, 2)


let puzzle1 fileName =
    let values = readFile fileName
    let bitMax = values[0].Length - 1

    let gamma = computeGamma values
    let epsilon = gamma |> negate
    let gamma = gamma |> bin2dec
    let epsilon = epsilon |> bin2dec
    let power = gamma * epsilon

    printfn $"Puzzle1: {power}"

let puzzle2 fileName =
    let values = readFile fileName
    let bitMax = values[0].Length - 1

    let computeRate computePattern =
        let mutable res = values
        for idx in 0..bitMax do
            if 1 < res.Length then
                let pattern: string = computePattern res
                res <- res |> List.filter (fun x -> x[idx] = pattern[idx])
        res[0]

    let oxygen = computeRate computeGamma |> bin2dec
    let co2 = computeRate (negate << computeGamma) |> bin2dec

    let lifeRating = oxygen * co2
    printfn $"Puzzle2: {lifeRating}"

puzzle1 "../Input3.txt"
puzzle2 "../Input3.txt"



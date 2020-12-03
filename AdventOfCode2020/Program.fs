open System
open System.IO

let banner day part =
    let info = sprintf "Day %s - Part %s" day part
    let row = String('-', info.Length)
    printfn "\n%s\n%s" info row

let readData filename = filename |> File.ReadAllLines

module Day1 =

    let part1 () =
        banner "1" "1"
        let data = readData "Input1.txt" |> Array.map Int32.Parse
        for i in 0..data.Length-1 do
            for j in i..data.Length-1 do
                let x = data.[i]
                let y = data.[j]
                if x + y = 2020 then printfn "%d" (x * y)

    let part2 () =
        banner "1" "2"
        let data = readData "Input1.txt" |> Array.map Int32.Parse
        for i in 0..data.Length-1 do
            for j in i..data.Length-1 do
                for k in j..data.Length-1 do
                    let x = data.[i]
                    let y = data.[j]
                    let z = data.[k]
                    if x + y + z = 2020 then printfn "%d" (x * y * z)


module Day2 =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let toPolicy s =
        match s with
        | Regex "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" [min; max; letter; pwd] -> (min |> Int32.Parse, max |> Int32.Parse, letter.[0], pwd)
        | _ -> failwith "Invalid input"

    let part1() =
        let checkPolicy (min, max, letter, pwd: string) =
            let c = pwd |> Seq.filter (fun x -> x = letter) |> Seq.length
            min <= c && c <= max

        banner "2" "1"
        let valid = readData "Input2.txt" |> Array.filter (toPolicy >> checkPolicy) |> Array.length
        printfn "%d" valid


    let part2() =
        let checkPolicy (min, max, letter, pwd: string) =
            match pwd.[min-1] = letter, pwd.[max-1] = letter with
            | true, false -> true
            | false, true -> true
            | _ -> false

        banner "2" "1"
        let valid = readData "Input2.txt" |> Array.filter (toPolicy >> checkPolicy) |> Array.length
        printfn "%d" valid

[<EntryPoint>]
let main argv =
    // Day1.part1() ; Day1.part2()
    Day2.part1() ; Day2.part2()
    0

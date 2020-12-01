open System
open System.IO

let banner day part =
    let info = sprintf "Day %s - Part %s" day part
    let row = String('-', info.Length)
    printfn "\n%s\n%s" info row


module Day1 =

    let readData filename = filename |> File.ReadAllLines |> Array.map Int32.Parse

    let part1 () =
        banner "1" "1"
        let data = readData "Input1.txt"
        for i in 0..data.Length-1 do
            for j in i..data.Length-1 do
                let x = data.[i]
                let y = data.[j]
                if x + y = 2020 then printfn "%d" (x * y)

    let part2 () =
        banner "1" "2"
        let data = readData "Input1.txt"
        for i in 0..data.Length-1 do
            for j in i..data.Length-1 do
                for k in j..data.Length-1 do
                    let x = data.[i]
                    let y = data.[j]
                    let z = data.[k]
                    if x + y + z = 2020 then printfn "%d" (x * y * z)

[<EntryPoint>]
let main argv =
    Day1.part1()
    Day1.part2()
    0

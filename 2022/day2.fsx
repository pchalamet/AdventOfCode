open System
open System.IO

let inputfile = "inputs/day2.txt"

let score1 (s: string) =
    match s.Split(" ") with
    | [| "C"; "X" |] -> 6 + 1
    | [| "A"; "X" |] -> 3 + 1
    | [| "B"; "X" |] -> 0 + 1

    | [| "A"; "Y" |] -> 6 + 2
    | [| "B"; "Y" |] -> 3 + 2
    | [| "C"; "Y" |] -> 0 + 2

    | [| "B"; "Z" |] -> 6 + 3
    | [| "C"; "Z" |] -> 3 + 3
    | [| "A"; "Z" |] -> 0 + 3
    | _ -> failwith "invalid match"

let score2 (s: string) =
    match s.Split(" ") with
    | [| "C"; "X" |] -> score1 "C Y"
    | [| "A"; "X" |] -> score1 "A Z"
    | [| "B"; "X" |] -> score1 "B X"

    | [| "A"; "Y" |] -> score1 "A X"
    | [| "B"; "Y" |] -> score1 "B Y"
    | [| "C"; "Y" |] -> score1 "C Z"

    | [| "B"; "Z" |] -> score1 "B Z"
    | [| "C"; "Z" |] -> score1 "C X"
    | [| "A"; "Z" |] -> score1 "A Y"
    | _ -> failwith "invalid match"

let part1() =
    let response =
        File.ReadAllLines(inputfile)
        |> Seq.map score1
        |> Seq.sum
    printfn $"part1: {response}"

let part2() =
    let response =
        File.ReadAllLines(inputfile)
        |> Seq.map score2
        |> Seq.sum
    printfn $"part2: {response}"

part1()
part2()

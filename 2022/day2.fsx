#load "Helpers.fsx"
open Helpers

let inputfile = "inputs/day2.input"

let score1 (s: string) =
    match s |> split " " with
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
    match s |> split " " with
    | [| "C"; "X" |] -> "C Y"
    | [| "A"; "X" |] -> "A Z"
    | [| "B"; "X" |] -> "B X"

    | [| "A"; "Y" |] -> "A X"
    | [| "B"; "Y" |] -> "B Y"
    | [| "C"; "Y" |] -> "C Z"

    | [| "B"; "Z" |] -> "B Z"
    | [| "C"; "Z" |] -> "C X"
    | [| "A"; "Z" |] -> "A Y"
    | _ -> failwith "invalid match"

let part1() =
    inputfile
    |> readlines
    |> Seq.sumBy score1
    |> print1

let part2() =
    inputfile
    |> readlines
    |> Seq.sumBy (score1 << score2)
    |> print2

part1()
part2()

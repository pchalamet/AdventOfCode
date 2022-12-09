#load "Helpers.fsx"
open Helpers

let inputfile = "inputs/day6.input"

let score len =
    let index =
        inputfile
        |> readlines
        |> Seq.head
        |> Seq.windowed len
        |> Seq.findIndex (fun x -> x |> Array.distinct |> Array.length = len)
    index + len

let part1 () =
    score 4
    |> print1

let part2 () =
    score 14
    |> print1

part1()
part2()

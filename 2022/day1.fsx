#load "Helpers.fsx"
open Helpers

let inputfile = "inputs/day1.input"

let compute () =
    inputfile
    |> readlines
    |> join "+"
    |> split "++"
    |> Seq.map (Seq.sum << Seq.map int << split "+")

let part1() =
    compute()
    |> Seq.max
    |> print1

let part2() =
    compute()
    |> Seq.sortDescending
    |> Seq.take 3 
    |> Seq.sum
    |> print2

part1()
part2()

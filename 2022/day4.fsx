#load "Helpers.fsx"
open Helpers

let inputfile = "inputs/day4.input"

let parse = function
    | Regex "^(\d+)-(\d+),(\d+)-(\d+)$" [start1; end1; start2; end2] ->
        set [int start1..int end1], set [int start2..int end2]
    | _ ->
        failwith "Invalid match"

let score predicate =
    inputfile
    |> readlines
    |> Seq.map parse
    |> Seq.filter predicate
    |> Seq.length

let part1() = score (fun (set1, set2) -> Set.isSubset set1 set2 || Set.isSubset set2 set1)

let part2() = score (fun (set1, set2) -> Set.intersect set1 set2 <> Set.empty)

part1() |> print1
part2() |> print2

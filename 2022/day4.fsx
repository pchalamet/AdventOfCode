#load "Helpers.fsx"
open Helpers

let inputfile = "inputs/day4.input"


let parse = function
    | Regex "^(\d+)-(\d+),(\d+)-(\d+)$" [start1; end1; start2; end2] ->
        let start1 = start1 |> int
        let end1 = end1 |> int
        let start2 = start2 |> int
        let end2 = end2 |> int
        [start1..end1] |> Set.ofList, [start2..end2] |> Set.ofList
    | _ ->
        failwith "Invalid match"

let score1 (set1, set2) =
    if Set.isSubset set1 set2 || Set.isSubset set2 set1 then 1
    else 0

let score2 (set1, set2) =
    if Set.intersect set1 set2 <> Set.empty then 1
    else 0

let compute f =
    inputfile
    |> readlines
    |> Seq.map (f << parse)
    |> Seq.sum

let part1() = compute score1

let part2() = compute score2

part1() |> print1
part2() |> print2

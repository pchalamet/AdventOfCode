#load "Helpers.fsx"
open Helpers

let inputfile = "inputs/day3.input"

let allItems = ['a'..'z'] @ ['A'..'Z'] |> Set.ofList

let score (rss: string seq) =
    let common =
        rss 
        |> Seq.fold (fun acc rs -> Set.intersect acc (rs |> Set.ofSeq)) allItems
        |> Seq.head
    let b = 
        if common <= 'Z' then ('A' |> int) - 27
        else ('a' |> int) - 1
    (common |> int) - b

let split2 (s: string) =
    let h = s.Length/2
    [ s |> mid 0 h; s |> mid h h ]

let part1() =
    inputfile
    |> readlines
    |> Seq.map split2
    |> Seq.sumBy score
    |> print1

let part2() =
    inputfile
    |> readlines
    |> Seq.chunkBySize 3
    |> Seq.sumBy score
    |> print2

part1()
part2()

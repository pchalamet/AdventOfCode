#load "Helpers.fsx"
open Helpers
open System.Collections.Generic

let inputfile = "inputs/day5.input"

let isEmpty s =
    s = ""

let score reorder =
    let input_state =
        inputfile
        |> readlines
        |> Array.takeWhile (not << isEmpty)
        |> List.ofArray

    let nb_stacks = (1 + input_state[0].Length) / 4
    let height = input_state.Length - 1
    let stacks = Array.init nb_stacks (fun _ -> Stack<char>())
    for i in 0..nb_stacks-1 do
        for j in height-1..-1..0 do
            let x = i * 4 + 1
            let y = j
            let crate = input_state[y][x]
            if crate <> ' ' then
                stacks[i].Push(crate)

    let commands =
        inputfile
        |> readlines
        |> Array.skipWhile (not << isEmpty)
        |> Array.skip 1

    for command in commands do
        match command with
        | Regex "^move (\d+) from (\d+) to (\d+)$" [qty; f; t] ->
            let qty, f, t = int qty, int f, int t
            let items = [1..qty] |> List.fold (fun acc _ -> stacks[f-1].Pop() :: acc) [] |> reorder
            items |> List.iter (fun item -> stacks[t-1].Push(item))
        | _ -> failwith "Invalid match"

    stacks |> Seq.fold (fun acc t -> acc + $"{t.Peek()}" ) ""

let part1() =
    score (fun items -> items |> List.rev)
    |> print1

let part2() =
    score id
    |> print2

part1()
part2()

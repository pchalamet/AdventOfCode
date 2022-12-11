#load "Helpers.fsx"
open Helpers
open System.Collections.Generic
open System.Numerics

let inputfile = "inputs/day11.input"

type MonkeyMath =
    | Square
    | Multiply of int64
    | Add of int64

type Monkey =
    { mutable Stats: int64
      Items: Queue<int64>
      WorryCompute: MonkeyMath
      Test: int64
      TestTrue: int
      TestFalse: int }

let rec readMonkey lines =
    let items, lines = 
        match lines with
        | Regex "^  Starting items: (.+)$" [items] :: tail ->
            items |> split ", " |> Seq.map int64 |> Queue, tail
        | _ -> failwith "Invalid match"

    let maths, lines =
        match lines with
        | Regex "^  Operation: new = old \* old$" [] :: tail ->
            Square, tail
        | Regex "^  Operation: new = old \* (\d+)$" [by] :: tail ->
            Multiply (int64 by), tail
        | Regex "^  Operation: new = old \+ (\d+)$" [by] :: tail ->
            Add (int64 by), tail
        | _ -> failwith $"Invalid match {lines}"

    let tests, lines =
        match lines with
        | Regex "^  Test: divisible by (\d+)$" [by] :: tail ->
            int64 by, tail
        | _ -> failwith "Invalid match"

    let testTrue, lines =
        match lines with
        | Regex "^    If true: throw to monkey (\d+)$" [target]:: tail ->
            int target, tail
        | _ -> failwith "Invalid match"

    let testFalse, lines =
        match lines with
        | Regex "^    If false: throw to monkey (\d+)$" [target]:: tail ->
            int target, tail
        | _ -> failwith "Invalid match"

    { Stats = 0L
      Items = items
      WorryCompute = maths
      Test = tests
      TestTrue = testTrue
      TestFalse = testFalse }, lines

let rec readMonkeys monkeys lines =
    match lines with
    | Regex "^Monkey (\d+):$" [id] :: tail ->
        let monkey, lines = readMonkey tail
        readMonkeys (monkeys @ [monkey]) lines
    | "" :: tail ->
        readMonkeys monkeys tail
    | _ -> monkeys

let round (monkeys: Monkey list) boredom (roundNumber: int) =
    for monkey in monkeys do
        while 0 < monkey.Items.Count do
            monkey.Stats <- monkey.Stats + 1L
            let item = monkey.Items.Dequeue()
            let item =
                match monkey.WorryCompute with
                | Square -> item * item
                | Multiply by -> item * by
                | Add value -> item + value
            let item = boredom item
            let target = (item % monkey.Test = 0L) ? (monkey.TestTrue, monkey.TestFalse)
            // printfn $"round {roundNumber}: monkey sends to monkey {target} item {item}"
            monkeys[target].Items.Enqueue(item)

    // printfn $"*** round {roundNumber}"
    // monkeys |> List.iteri (fun idx monkey ->
    //                         printf $"Monkey {idx} ({monkey.Stats}):"
    //                         for item in monkey.Items do
    //                              printf $" {item}"
    //                         printfn "")

let parse () =
    let lines =
        inputfile
        |> readlines
        |> List.ofArray

    readMonkeys [] lines

let run boredom maxrounds monkeys =
    for roundNumber in 1..maxrounds do
        round monkeys boredom roundNumber

    monkeys
    |> List.sortByDescending (fun m -> m.Stats)
    |> List.map (fun m -> m.Stats)
    |> List.take 2
    |> List.fold (*) 1L

let part1() =
    let boredom item = item / 3L

    parse()
    |> run boredom 20
    |> print1

let part2() =
    let monkeys = parse()
    let m = monkeys |> List.fold (fun acc m -> acc * m.Test) 1L

    let boredom item = item % m

    monkeys    
    |> run boredom 10000
    |> print2

part1()
part2()

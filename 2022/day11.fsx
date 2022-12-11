#load "Helpers.fsx"
open Helpers
open System.Collections.Generic
open System.Numerics

let inputfile = "inputs/day11.input"

type MonkeyMath =
    | Square
    | Multiply of BigInteger
    | Add of BigInteger

type Monkey =
    { mutable Stats: BigInteger
      Items: Queue<BigInteger>
      WorryCompute: MonkeyMath
      Test: BigInteger
      TestTrue: int
      TestFalse: int }

let toBigInteger s =
    s |> int |> BigInteger

let rec readMonkey lines =
    let items, lines = 
        match lines with
        | Regex "^  Starting items: (.+)$" [items] :: tail ->
            items |> split ", " |> Seq.map toBigInteger |> Queue, tail
        | _ -> failwith "Invalid match"

    let maths, lines =
        match lines with
        | Regex "^  Operation: new = old \* old$" [] :: tail ->
            Square, tail
        | Regex "^  Operation: new = old \* (\d+)$" [by] :: tail ->
            Multiply (toBigInteger by), tail
        | Regex "^  Operation: new = old \+ (\d+)$" [by] :: tail ->
            Add (toBigInteger by), tail
        | _ -> failwith $"Invalid match {lines}"

    let tests, lines =
        match lines with
        | Regex "^  Test: divisible by (\d+)$" [by] :: tail ->
            toBigInteger by, tail
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

    { Stats = BigInteger.Zero
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
            monkey.Stats <- monkey.Stats + BigInteger.One
            let item = monkey.Items.Dequeue()
            let item =
                match monkey.WorryCompute with
                | Square -> item * item
                | Multiply by -> item * by
                | Add value -> item + value
            let item = boredom item
            let target = (item % monkey.Test = BigInteger.Zero) ? (monkey.TestTrue, monkey.TestFalse)
            // printfn $"round {roundNumber}: monkey sends to monkey {target} item {item}"
            monkeys[target].Items.Enqueue(item)

    printfn $"*** round {roundNumber}"
    // monkeys |> List.iteri (fun idx monkey ->
    //                         printf $"Monkey {idx} ({monkey.Stats}):"
    //                         for item in monkey.Items do
    //                              printf $" {item}"
    //                         printfn "")

let score boredom maxrounds =
    let lines =
        inputfile
        |> readlines
        |> List.ofArray

    let monkeys = readMonkeys [] lines
    for roundNumber in 1..maxrounds do
        round monkeys boredom roundNumber

    monkeys
    |> List.sortByDescending (fun m -> m.Stats)
    |> List.map (fun m -> m.Stats)
    |> List.take 2
    |> List.fold (*) BigInteger.One

let part1() =
    let boredom (item: BigInteger) = item / BigInteger(3)

    score boredom 20
    |> print1

let part2() =
    let boredom item = item

    score boredom 10000
    |> print2


part1()
part2()

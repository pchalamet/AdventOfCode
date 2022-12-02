open System
open System.IO

let inputfile = "inputs/day1.txt"

let compute() =
    File.ReadAllLines(inputfile)
    |> String.concat "+"
    |> (fun x -> x.Split("++"))
    |> Seq.map (fun x -> x.Split("+") |> Seq.map int |> Seq.sum)

let part1() =
    let response = compute() |> Seq.max
    printfn $"part1: {response}"

let part2() =
    let response = compute() |> Seq.sortDescending |> Seq.take 3 |> Seq.sum
    printfn $"part2: {response}"

part1()
part2()

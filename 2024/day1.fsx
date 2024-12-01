#load "Helpers.fsx"
open System
open Helpers

let inputfile = "inputs/day1.input"

let data1, data2 =
    inputfile
    |> readlines
    |> Array.map (fun line ->
        let items = line.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        Int32.Parse(items[0]), Int32.Parse(items[1]))
    |> Array.unzip


let part1() =
    let data1 = Array.sort data1
    let data2 = Array.sort data2
    let data = Array.zip data1 data2
    let diff =
        data
        |> Array.map (fun (item1,item2) -> abs(item1 - item2))
        |> Array.sum
    diff
    |> print1

let part2() =
    let diff =
        data1
        |> Array.map (fun item1 -> item1 * (data2 |> Array.filter (fun item2 -> item2 = item1) |> Array.length))
        |> Array.sum
    diff
    |> print1

part1()
part2()

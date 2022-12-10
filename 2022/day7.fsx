#load "Helpers.fsx"
open Helpers
open System.IO

let inputfile = "inputs/day7.input"


let score filter =
    let lines =
        inputfile
        |> readlines

    let mutable folders = Map [ "/", Map.empty ]
    let mutable pwd = "/"

    for line in lines do
        let cd = function
            | ".." -> pwd <- Path.GetDirectoryName(pwd)
            | "/" -> pwd <- "/"
            | dir -> pwd <- Path.Combine(pwd, dir)

        let newdir dir =
            match folders |> Map.tryFind dir with
            | None -> folders <- folders |> Map.add (Path.Combine(pwd, dir)) Map.empty
            | _ -> ()

        let newfile file len =
            let files = folders |> Map.find pwd
            match files |> Map.tryFind file with
            | None -> folders <- folders |> Map.add pwd (files |> Map.add file len)
            | _ -> ()

        match line with
        | Regex "^\$ cd (.+)$" [dir] -> cd dir
        | Regex "^\$ ls$" [] -> ()
        | Regex "^dir (.+)$" [dir] -> newdir dir
        | Regex "^(\d+) (.+)$" [len; file] -> newfile file (int len)
        | _ -> failwith "Invalid match"

    let folderSizes = 
        folders
        |> Map.map (fun _ v -> v |> Seq.sumBy (fun (KeyValue (_, l)) -> l))

    let mutable folderWithSubSizes = Map.empty
    for folder in folderSizes.Keys |> Seq.sortByDescending (fun x -> x.Length) do
        let folderSize =
            folderSizes 
            |> Map.filter (fun k _ -> k.StartsWith(folder))
            |> Seq.sumBy (fun (KeyValue(_, s)) -> s)
        folderWithSubSizes <- folderWithSubSizes |> Map.add folder folderSize

    folderWithSubSizes |> filter

let part1() =
    let filter sizes =
        sizes
        |> Map.filter (fun _ v -> v <= 100_000)
        |> Seq.sumBy (fun (KeyValue (_, l)) -> l)

    score filter
    |> print1

let part2() =
    let filter sizes =
        let usedSize = sizes |> Map.find "/"
        let unusedSize = 70000000 - usedSize

        sizes
        |> Seq.map (fun (KeyValue (_, l)) -> l)
        |> Seq.sort
        |> Seq.find (fun v -> 30000000 <= unusedSize + v)

    score filter
    |> print2

part1()
part2()

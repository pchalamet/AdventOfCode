module Day7

open System
open System.Linq

let input = System.IO.File.ReadAllLines("data/Day7.txt")


//let data = @"pbga (66)
//xhth (57)
//ebii (61)
//havc (66)
//ktlj (57)
//fwft (72) -> ktlj, cntj, xhth
//qoyq (66)
//padx (45) -> pbga, havc, qoyq
//tknk (41) -> ugml, padx, fwft
//jptl (61)
//ugml (68) -> gyxo, ebii, jptl
//gyxo (61)
//cntj (57)"
//let input = data.Split("\r\n")




type Dependency =
    { Name : string
      Weight : int
      Dependencies : string list }


// -------------------------------------------------------------------------------------------

let parseDependency (line : string) =
    let leftRight = line.Split("->", StringSplitOptions.RemoveEmptyEntries)
    let leftItems = leftRight.[0].Split(' ', StringSplitOptions.RemoveEmptyEntries)
    let name = leftItems.[0]
    let weight = leftItems.[1].Substring(1, leftItems.[1].Length-2) |> int
    let rightItems = if leftRight.Length > 1 then leftRight.[1].Trim().Split(", ", StringSplitOptions.RemoveEmptyEntries)
                     else Array.empty
    { Name = name; Weight = weight; Dependencies = rightItems |> List.ofSeq }



// -------------------------------------------------------------------------------------------


let rec checkBalanced (indexedDependencies : Map<string, Dependency>) (node : string) =
    let thisNode = indexedDependencies.[node]
    let nodeWeights = thisNode.Dependencies |> Seq.fold (fun s t -> (t, checkBalanced indexedDependencies t) :: s) List.empty
    let weights = nodeWeights |> Seq.map snd
    if 1 < weights.Distinct().Count() then 
        let max = weights.Max<int>()
        let delta = max - weights.Min()
        let faultyNode = nodeWeights |> Seq.find (fun x -> x |> snd = max) |> fst
        failwithf "Delta is %d from weight %d for node %s" delta (indexedDependencies.[faultyNode].Weight) faultyNode
    
    let childrenWeights = weights |> Seq.fold (fun s t -> s + t) 0
    thisNode.Weight + childrenWeights
    

let Part2() =
    try
        let dependencies = input |> Seq.map parseDependency
        let allDependencies = dependencies |> Seq.fold (fun s t -> s @ t.Dependencies) List.empty
                                           |> Set
        let allNodes = dependencies |> Seq.fold (fun s t ->  t.Name :: s) List.empty
                                    |> Set

        let rootNodes =  allNodes - allDependencies
        let rootNode = rootNodes |> Seq.head

        let indexedDependencies = dependencies |> Seq.map (fun x -> x.Name, x)
                                               |> Map
        checkBalanced indexedDependencies rootNode |> ignore
    with
        exn -> printfn "%s" exn.Message

// -------------------------------------------------------------------------------------------

let Part1() =
    let dependencies = input |> Seq.map parseDependency
    let allDependencies = dependencies |> Seq.fold (fun s t -> s @ t.Dependencies) List.empty
                                       |> Set
    let allNodes = dependencies |> Seq.fold (fun s t ->  t.Name :: s) List.empty
                                |> Set

    let rootNodes =  allNodes - allDependencies
    let rootNode = rootNodes |> Seq.head
    printfn "%s" rootNode

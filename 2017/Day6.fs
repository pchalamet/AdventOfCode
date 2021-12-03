module Day6

open System
open System.Linq


let input = System.IO.File.ReadAllText("data/Day6.txt").Split('\t', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int

//let input = [| 0; 2; 7; 0 |]



// ------------------------------------------------------------------------------------------------


let rec rebalanceCycleAgain (banks : int []) (targetBanks : int []) (steps : int) =
    if banks = targetBanks && 0 < steps then steps
    else
        let max = banks.Max()
        let mutable maxIndex = banks |> Array.findIndex (fun x -> x = max)
        
        banks.[maxIndex] <- 0
        for index in 1..max do
            let bankIndex = (maxIndex + index) % banks.Length
            banks.[bankIndex] <- banks.[bankIndex] + 1

        rebalanceCycleAgain banks targetBanks (steps+1)


let rec rebalanceUntilCycle (banks : int []) (knownConfigs : Set<int []>) (steps : int) =
    if knownConfigs |> Set.contains banks then
        rebalanceCycleAgain banks (banks |> Seq.toArray) 0
    else
        let newKnownConfigs = knownConfigs |> Set.add (banks |> Seq.toArray)
        let max = banks.Max()
        let mutable maxIndex = banks |> Array.findIndex (fun x -> x = max)
        
        banks.[maxIndex] <- 0
        for index in 1..max do
            let bankIndex = (maxIndex + index) % banks.Length
            banks.[bankIndex] <- banks.[bankIndex] + 1

        rebalanceUntilCycle banks newKnownConfigs (steps+1)

let Part2 () =
    let steps = rebalanceUntilCycle input Set.empty 0
    printfn "Cycle detected in %d steps"  steps



// ------------------------------------------------------------------------------------------------




let rec rebalance (banks : int []) (knownConfigs : Set<int []>) (steps : int) =
    if knownConfigs |> Set.contains banks then steps
    else
        let newKnownConfigs = knownConfigs |> Set.add (banks |> Seq.toArray)
        let max = banks.Max()
        let mutable maxIndex = banks |> Array.findIndex (fun x -> x = max)
        
        banks.[maxIndex] <- 0
        for index in 1..max do
            let bankIndex = (maxIndex + index) % banks.Length
            banks.[bankIndex] <- banks.[bankIndex] + 1

        rebalance banks newKnownConfigs (steps+1)

let Part1 () =
    let steps = rebalance input Set.empty 0
    printfn "Cycle detected in %d steps"  steps

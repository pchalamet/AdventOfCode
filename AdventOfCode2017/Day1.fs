module Day1



// --------------------------------------------------------------------------------


let input = System.IO.File.ReadAllText("data/Day1.txt")


// --------------------------------------------------------------------------------

let private current (pos : int) (input : string) =
    input.[pos] |> string |> int

let private next (pos : int) (input : string) =
    let nextPos = (pos + 1) % input.Length
    input.[nextPos] |> string |> int

let private isEnd (pos : int) (input : string) =
    input.Length <= pos

let rec private add (pos : int) (input : string) =
    if isEnd pos input then 0
    else
        let current = current pos input
        let next = next pos input
        let res = if current = next then current else 0
        res + add (pos+1) input

let Part1() =
    let res = add 0 input
    printfn "Result for %A is %A" input res



// --------------------------------------------------------------------------------


let private nextN (pos : int) (input : string) =
    let nextPos = (pos + input.Length/2) % input.Length
    input.[nextPos] |> string |> int

let rec private addN (pos : int) (input : string) =
    if isEnd pos input then 0
    else
        let current = current pos input
        let next = nextN pos input
        let res = if current = next then current else 0
        res + addN (pos+1) input


let Part2 () =
    let res = addN 0 input
    printfn "ResultN for %A is %A" input res


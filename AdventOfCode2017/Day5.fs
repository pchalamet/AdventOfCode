module Day5


let input = System.IO.File.ReadAllLines ("data/Day5.txt") |> Seq.map int

// --------------------------------------------------------------------------------------------


let rec escape jumpRule (jumps : int []) (pc : int) (steps : int) =
    if pc < 0 || jumps.Length <= pc then steps
    else
        let jump = jumps.[pc]
        jumps.[pc] <- jumpRule jump
        escape jumpRule jumps (pc + jump) (steps + 1)


// --------------------------------------------------------------------------------------------

let part2rule jump =
    if jump >= 3 then jump - 1
    else jump + 1


let Part2 () =
    let jumps = input |> Array.ofSeq
    let steps = escape part2rule jumps 0 0
    printfn "Escaped in %d step" steps


// --------------------------------------------------------------------------------------------

let part1rule jump =
    jump + 1

let Part1 () =
    let jumps = input |> Array.ofSeq
    let steps = escape part1rule jumps 0 0
    printfn "Escaped in %d step" steps



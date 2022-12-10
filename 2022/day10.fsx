#load "Helpers.fsx"
open Helpers

let inputfile = "inputs/day10.input"

let score () =
    let rec run cycles reg lines =
        seq {
            yield cycles, reg
            match lines with
            | Regex "^addx (.+)$" [v] :: tail -> 
                yield cycles + 1, reg
                yield! run (cycles+2) (reg + (int v)) tail 
            | Regex "^noop$" [] :: tail ->
                yield! run (cycles+1) reg tail
            | [] -> ()
            | _ -> failwith $"Invalid match"
        }

    inputfile
    |> readlines |> List.ofArray
    |> run 1 1

let part1() =
    let signal_strength (cycles, reg) =
        if (cycles - 20) % 40 = 0 then cycles * reg
        else 0

    score()
    |> Seq.sumBy signal_strength
    |> print1

let part2() =
    let display_sprite (cycles, reg) =
        seq {
            let crt = (cycles - 1) % 40
            let sprite = reg
            yield if abs (sprite - crt) < 2 then '#' else ' '
            if crt = 39 then yield '\n'
        }

    score()
    |> Seq.collect display_sprite
    |> Array.ofSeq
    |> (fun x -> "\n" + System.String(x))
    |> print2

part1()
part2()

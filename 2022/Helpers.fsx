open System.IO
open System.Text.RegularExpressions

let split (sep: string) (s: string) = s.Split(sep)
let mid start len (s: string) = s.Substring(start, len)
let join (sep: string) (ss: string seq) = String.concat sep ss

let readlines (f: string) = f |> File.ReadAllLines
let print1 result = printfn $"part1: {result}"
let print2 result = printfn $"part2: {result}"


let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

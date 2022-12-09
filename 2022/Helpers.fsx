open System
open System.IO
open System.Text.RegularExpressions

let split (sep: string) (s: string) = s.Split(sep)
let mid start len (s: string) = s.Substring(start, len)
let join (sep: string) (ss: string seq) = String.concat sep ss

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then List.tail [ for g in m.Groups -> g.Value ] |> Some
    else None

let readlines (f: string) = f |> File.ReadAllLines

let print1 result = printfn $"part1: {result}"
let print2 result = printfn $"part2: {result}"

type set<'T when 'T : comparison> = Set<'T>

let (?) (q: bool) (yes: 'a, no: 'a) = if q then yes else no

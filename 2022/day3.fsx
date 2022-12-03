open System.IO

let inputfile = "inputs/day3.input"

let allItems = (['a'..'z'] |> Set.ofList) + (['A'..'Z'] |> Set.ofList)

let score (rss: string seq) =
    let common =
        rss 
        |> Seq.fold (fun acc rs -> Set.intersect acc (rs |> Set.ofSeq)) allItems
        |> Seq.head
    let b = 
        if common <= 'Z' then ('A' |> int) - 27
        else ('a' |> int) - 1
    (common |> int) - b

let split2 (s: string) =
    [s.Substring(0, s.Length/2); s.Substring(s.Length/2)]

let day1() =
    let response =
        File.ReadAllLines(inputfile)
        |> Seq.map split2
        |> Seq.sumBy score

    printfn $"part1: {response}"

let day2() =
    let response =
        File.ReadAllLines(inputfile)
        |> Seq.chunkBySize 3
        |> Seq.sumBy score

    printfn $"part2: {response}"


day1()
day2()


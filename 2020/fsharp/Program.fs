open System
open System.IO
open System.Text.RegularExpressions

let banner day part =
    let info = sprintf "Day %s - Part %s" day part
    let row = String('-', info.Length)
    printfn "\n%s\n%s" info row

let readData filename = filename |> File.ReadAllLines

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|Integer|_|) (s: string) =
    match Int32.TryParse(s) with
    | true, i -> Some i
    | _ -> None

module Day1 =
    let part1 () =
        banner "1" "1"
        let data = readData "Input1.txt" |> Array.map Int32.Parse
        for i in 0..data.Length-1 do
            for j in i..data.Length-1 do
                let x = data.[i]
                let y = data.[j]
                if x + y = 2020 then printfn "%d" (x * y)

    let part2 () =
        banner "1" "2"
        let data = readData "Input1.txt" |> Array.map Int32.Parse
        for i in 0..data.Length-1 do
            for j in i..data.Length-1 do
                for k in j..data.Length-1 do
                    let x = data.[i]
                    let y = data.[j]
                    let z = data.[k]
                    if x + y + z = 2020 then printfn "%d" (x * y * z)


module Day2 =
    let toPolicy s =
        match s with
        | Regex "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" [min; max; letter; pwd] -> (min |> Int32.Parse, max |> Int32.Parse, letter.[0], pwd)
        | _ -> failwith "Invalid input"

    let part1() =
        let checkPolicy (min, max, letter, pwd: string) =
            let c = pwd |> Seq.filter (fun x -> x = letter) |> Seq.length
            min <= c && c <= max

        banner "2" "1"
        let valid = readData "Input2.txt" |> Array.filter (toPolicy >> checkPolicy) |> Array.length
        printfn "%d" valid


    let part2() =
        let checkPolicy (min, max, letter, pwd: string) =
            match pwd.[min-1] = letter, pwd.[max-1] = letter with
            | true, false -> true
            | false, true -> true
            | _ -> false

        banner "2" "1"
        let valid = readData "Input2.txt" |> Array.filter (toPolicy >> checkPolicy) |> Array.length
        printfn "%d" valid

module Day3 =
    let data = readData "Input3.txt" |> Array.ofSeq

    let maxX = data.Length
    let maxY = match data |> Array.tryHead with
               | Some col -> col.Length
               | _ -> failwith "Invalid input"

    let slide dx dy =
        // acc is long to avoid overflow
        let rec slide acc x y =
            if x < maxX then
                let newAcc = if data.[x].[y] = '#' then acc + 1L
                             else acc
                slide newAcc (x + dx) ((y + dy) % maxY)
            else acc            
        slide 0L dx dy

    let part1 () =
        let trees = slide 1 3
        printfn "%d" trees

    let part2 () =
        let slopes = [ 1, 1
                       1, 3
                       1, 5
                       1, 7
                       2, 1 ]

        let res = slopes |> List.fold (fun acc (dx, dy) -> acc * slide dx dy) 1L
        printfn "%d" res


module Day4 =
    let data = readData "Input4.txt" |> List.ofArray

    let readPassports data =
        let rec readPassports tmpPP data =
            seq {
                match data with
                | [] -> yield tmpPP
                | "" :: tail -> yield tmpPP
                                yield! readPassports Map.empty tail
                | line :: tail -> let newItems = line.Split(' ') |> Seq.map (fun x -> let items = x.Split(':')
                                                                                      items.[0], items.[1])
                                                                 |> Map
                                  let newPP = Map.fold (fun s k v -> Map.add k v s) tmpPP newItems
                                  yield! readPassports newPP tail
            }
        readPassports Map.empty data

    let part1() =
        let checkPassport (passport: Map<string, string>) =
            let mandatoryKeys = Set ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
            let keys = passport |> Seq.map (fun kvp -> kvp.Key) |> Set
            Set.intersect mandatoryKeys keys = mandatoryKeys

        let passports = readPassports data
        let valid = passports |> Seq.filter checkPassport |> Seq.length
        printfn "%d" valid

    let part2() =
        let checkPassport (passport: Map<string, string>) =
            let byr = match passport |> Map.tryFind "byr" with
                      | Some (Regex "^([0-9]{4})$" [Integer i]) when 1920 <= i && i <= 2002 -> Some i
                      | _ -> None

            let iyr = match passport |> Map.tryFind "iyr" with
                      | Some (Regex "^([0-9]{4})$" [Integer i]) when 2010 <= i && i <= 2020 -> Some i
                      | _ -> None

            let eyr = match passport |> Map.tryFind "eyr" with
                      | Some (Regex "^([0-9]{4})$" [Integer i]) when 2020 <= i && i <= 2030 -> Some i
                      | _ -> None

            let hgt = match passport |> Map.tryFind "hgt" with
                      | Some (Regex "^([0-9]+)cm$" [Integer c]) when 150 <= c && c <= 193 -> Some c
                      | Some (Regex "^([0-9]+)in$" [Integer i]) when 59 <= i && i <= 76 -> Some i
                      | _ -> None

            let hcl = match passport |> Map.tryFind "hcl" with
                      | Some (Regex "^#([0-9a-f]{6})$" [s]) -> Some s
                      | _ -> None

            let ecl = match passport |> Map.tryFind "ecl" with
                      | Some (Regex "^(amb|blu|brn|gry|grn|hzl|oth)$" [s]) -> Some s
                      | _ -> None

            let pid = match passport |> Map.tryFind "pid" with
                      | Some (Regex "^([0-9]{9})$" [s]) -> Some s
                      | _ -> None

            let res = byr.IsSome && iyr.IsSome && eyr.IsSome && hgt.IsSome && hcl.IsSome && ecl.IsSome && pid.IsSome
            res


        let passports = readPassports data
        let valid = passports |> Seq.filter checkPassport |> Seq.length
        printfn "%d" valid


module Day5 =
    let data = readData "Input5.txt" |> List.ofArray

    let decode br1 br2 s lo hi =
        let rec decode s lo hi =
            match s with
            | br :: tail when br = br1 -> decode tail lo ((lo + hi) / 2)
            | br :: tail when br = br2 -> decode tail ((lo + hi) / 2) hi
            | _ -> lo
        decode s lo hi

    let seatIds = data |> List.map (fun x -> let row = decode 'F' 'B' (x.Substring(0, 7) |> List.ofSeq) 0 128
                                             let col = decode 'L' 'R' (x.Substring(7, 3) |> List.ofSeq) 0 8
                                             let seatId = row * 8 + col
                                             seatId)

    let part1() =
        let maxSeatId = seatIds |> List.max
        printfn "%d" maxSeatId

    let part2() =
        let minSeat = seatIds |> List.min
        let maxSeat = seatIds |> List.max
        let seats = seatIds |> Set
        let mySeat = [minSeat..maxSeat] |> List.filter (fun x -> seats |> Set.contains x |> not)
        printfn "%A" mySeat.Head


[<EntryPoint>]
let main argv =
    // Day1.part1() ; Day1.part2()
    // Day2.part1() ; Day2.part2()
    // Day3.part1(); Day3.part2()
    // Day4.part1(); Day4.part2()
    Day5.part1(); Day5.part2()
    0

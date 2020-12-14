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


module Day6 =
    let data = File.ReadAllText "Input6.txt"

    let part1() =
        let res = data.Split("\n\n", StringSplitOptions.RemoveEmptyEntries) |> Array.sumBy (fun x -> x.Replace("\n", "") |> Set.ofSeq |> Set.count)
        printfn "%d" res

    let part2() =
        let res = data.Split("\n\n", StringSplitOptions.RemoveEmptyEntries) |> Array.sumBy (fun x -> x.Split("\n") |> Array.map Set.ofSeq 
                                                                                                                   |> Array.reduce Set.intersect 
                                                                                                                   |> Set.count)
        printfn "%d" res

module Day7 =
    let rec toContainerInfo (line: string list) res =
        match line with
        | Integer count 
              :: name1 
              :: name2 
              :: Regex "(bag|bags)(,|\.)" _ 
              :: tail -> res |> Map.add $"{name1} {name2}" count |> toContainerInfo tail
        | _ -> // printfn "%A %A" res line
               res

    let toBagInfo (line: string list) =
        match line with
        | name1 :: name2 :: "bags" :: "contain" :: tail -> $"{name1} {name2}", toContainerInfo tail Map.empty
        | _ -> failwith "invalid line"

    let data = readData "Input7.txt"
    let containers = data |> Array.map (fun x -> x.Split(" ") |> List.ofArray |> toBagInfo) |> Map.ofArray
    
    let part1() =        
        let rec find name =
            match containers |> Map.tryFind name with
            | Some m -> if m |> Map.containsKey "shiny gold" then 1
                        else if m |> Seq.exists (fun x -> find x.Key > 0) then 1 else 0
            | _ -> 0

        let res = containers |> Seq.map (fun kvp -> find kvp.Key) |> Seq.sum
        printfn "%d" res

    let part2() =
        let rec collect name =
            match containers |> Map.tryFind name with
            | Some m -> m |> Seq.sumBy (fun x -> (x.Value |> int64) + (x.Value |> int64) * (collect x.Key))
            | _ -> 0L

        let res = collect "shiny gold"
        printfn "%d" res

module Day8 =
    let data = readData "Input8.txt" |> Array.map (fun x -> x.Split(" ") |> List.ofArray)

    let part1() =
        let rec eval processedPC acc pc =
            if processedPC |> Set.contains pc then acc
            else
                let processedPC = processedPC |> Set.add pc
                match data.[pc] with
                | "nop" :: _ -> eval processedPC acc (pc+1)
                | "acc" :: Integer c :: [] -> eval processedPC (acc+c) (pc+1)
                | "jmp" :: Integer offset :: [] -> eval processedPC acc (pc+offset)
                | x -> failwithf "Unknown instruction %A" x
        let res = eval Set.empty 0 0
        printfn "%d" res

    let part2() =
        let rec eval processedPC acc pc =
            if processedPC |> Set.contains pc then None
            else if pc = data.Length then Some acc
            else
                let processedPC = processedPC |> Set.add pc
                match data.[pc] with
                | "nop" :: _ -> eval processedPC acc (pc+1)
                | "acc" :: Integer c :: [] -> eval processedPC (acc+c) (pc+1)
                | "jmp" :: Integer offset :: [] -> eval processedPC acc (pc+offset)
                | x -> failwithf "Unknown instruction %A" x

        let rec tryPatch offset =
            let patchAndEval instr count =
                let backup = data.[offset]
                data.[offset] <- instr :: count
                let res = eval Set.empty 0 0
                data.[offset] <- backup
                res

            let res = match data.[offset] with
                      | "nop" :: count -> patchAndEval "jmp" count
                      | "jmp" :: count -> patchAndEval "nop" count
                      | _ -> None
            match res with
            | Some acc -> acc
            | _ -> tryPatch (offset+1)

        let res = tryPatch 0
        printfn "%d" res

module Day9 = 
    let data = readData "Input9.txt" |> Array.map (System.Int64.Parse) |> List.ofArray

    let rec find offset =
        let indices = seq { for i in offset-25..offset-1 do
                                for j in i..offset-1 do 
                                    yield i,j }

        match indices |> Seq.tryFind (fun (i, j) -> data.[i] + data.[j] = data.[offset]) with
        | Some _ -> find (offset+1)
        | _ -> data.[offset]

    let part1() =
        let res = find 25
        printfn "%d" res

    let part2() =
        let invalidNumber = find 25

        let rec check data =
            let rec incSum acc data =
                let s = acc |> List.sum
                if s = invalidNumber then Some acc
                elif s > invalidNumber then None
                else
                    match data with
                    | head :: tail -> incSum (head::acc) tail
                    | _ -> None
            match data with
            | head :: tail -> match incSum [head] tail with
                              | Some x -> x
                              | _ -> check tail
            | _ -> failwith "no more data"
        let res = check data
        let minRes = res |> List.min
        let maxRes = res |> List.max
        printfn "%d" (minRes + maxRes)

module Day10 =
    let data = readData "Input10.txt" |> List.ofArray |> List.map Int32.Parse |> List.sort

    let part1() =
        let diffs = 0 :: data @ [3 + (data |> List.max)] |> List.pairwise
        let diffs = diffs |> List.map (fun (a,b) -> b-a)

        let jolt1s = diffs |> List.filter (fun x -> x = 1) |> List.length
        let jolt3s = diffs |> List.filter (fun x -> x = 3) |> List.length
        printfn "%A" (jolt1s * jolt3s)

    let part2() =
        // let deviceJolts = 3 + (data |> List.max)
        // printfn "%d %A" deviceJolts data

        // let rec explore acc data =
        //     seq {
        //         if deviceJolts <= acc + 3 then yield 1
        //         else
        //             let rec exploreCompatible data =
        //                 seq {
        //                     match data with
        //                     | candidate :: tail -> if candidate <= acc + 3 then
        //                                                yield! explore candidate tail
        //                                                yield! exploreCompatible tail
        //                     | _ -> ()
        //                 }

        //             yield! exploreCompatible data
        //     }
        // let res = explore (0, 0, 1) (data |> List.rev)
        // printfn "%d" res

        let data = 0 :: data
        let mutable res = (1L, 0L, 0L)
        for i in [data.Length-2..-1..0] do
            let (a, b, c) = res
            let s = (if i+1 < data.Length && data.[i+1] - data.[i] <= 3 then a else 0L)
                  + (if i+2 < data.Length && data.[i+2] - data.[i] <= 3 then b else 0L)
                  + (if i+3 < data.Length && data.[i+3] - data.[i] <= 3 then c else 0L)
            res <- (s, a, b)
        let (a, _, _) = res
        printfn "%d" a

module Day11 = 
    let data = readData "Input11.txt" |> Array.map (fun x -> x |> Array.ofSeq)
    let width = data.[0].Length
    let height = data.Length


    // let print (data: 't array array) =
    //     for i in 0..height-1 do
    //         for j in 0..width-1 do
    //             printf "%A " data.[i].[j]
    //         printfn ""

    let isAdjacentOccupied (data: char array array) i j di dj =
        let i = i + di
        let j = j + dj
        if 0 <= i && i < height && 0 <= j && j < width then
            if data.[i].[j] = '#' then 1
            else 0
        else 0

    let isVisibilityOccupied (data: char array array) x y di dj =
        let rec findVisible i j =
            if 0 <= i && i < height && 0 <= j && j < width then
                match data.[i].[j] with
                | '#' -> 1
                | 'L' -> 0
                | '.' -> findVisible (i+di) (j+dj)
                | _ -> failwith "invalid data"
            else 0
        findVisible (x+di) (y+dj)

    let directions = [ (-1, -1); (-1, 0); (-1, 1); (0, 1); (1, 1); (1, 0); (1, -1); (0, -1)]

    let computeOccupation data occupancyEvaluator =
        let occupation = (Array.zeroCreate<int> height) |> Array.map (fun _ -> Array.zeroCreate<int> width)
        for i in 0..height-1 do
            for j in 0..width-1 do
                occupation.[i].[j] <- directions |> List.sumBy (fun (di, dj) -> occupancyEvaluator data i j di dj)
        occupation

    let countOccupied (data: char array array) =
        let res = [ for i in 0..height-1 do
                        for j in 0..width-1 do
                            if data.[i].[j] = '#' then 1
                            else 0 ] |> List.sum
        res

    let convergeOccupancy occupancyEvaluator threshold =
        let workData = data |> Array.map (fun x -> x |> Array.copy)

        let rec mutate () =
            let occupation = computeOccupation workData occupancyEvaluator

            let mutable hasMutated = false
            for i in 0..height-1 do
                for j in 0..width-1 do
                    if workData.[i].[j] = 'L' && occupation.[i].[j] = 0 then 
                        workData.[i].[j] <- '#'
                        hasMutated <- true
                    elif workData.[i].[j] = '#' && occupation.[i].[j] >= threshold then
                        workData.[i].[j] <- 'L'
                        hasMutated <- true
            hasMutated
        while mutate () do ()
        countOccupied workData

    let part1() =
        let res = convergeOccupancy isAdjacentOccupied 4
        printfn "%d" res
        
    let part2() =
        let res = convergeOccupancy isVisibilityOccupied 5
        printfn "%d" res


module Day12 =
    let data = readData "Input12.txt" |> List.ofArray

    let findMove x y dir instruction =
        match instruction with
        | Regex "N([0-9]+)" [Integer m] -> x, (y+m), dir
        | Regex "E([0-9]+)" [Integer m] -> (x+m), y, dir
        | Regex "S([0-9]+)" [Integer m] -> x, (y-m), dir
        | Regex "W([0-9]+)" [Integer m] -> (x-m), y, dir
        | Regex "L([0-9]+)" [Integer m] -> x, y, (dir + 360 - m) % 360
        | Regex "R([0-9]+)" [Integer m] -> x, y, (dir + m) % 360
        | Regex "F([0-9]+)" [Integer m] -> match dir with
                                           | 0 -> x, (y+m), dir
                                           | 90 -> (x+m), y, dir
                                           | 180 -> x, (y-m), dir
                                           | 270 -> (x-m), y, dir
                                           | _ -> failwith "invalid move forward"
        | _ -> failwith "Invalid data"

    let rec computeFinalDestination x y dir instructions =
        match instructions with
        | instruction :: tail -> let x, y, dir = findMove x y dir instruction
                                 computeFinalDestination x y dir tail
        | _ -> abs(x) + abs(y) 

    let part1() = 
        let dist = computeFinalDestination 0 0 90 data
        printfn "%d" dist



[<EntryPoint>]
let main argv =
    // Day1.part1() ; Day1.part2()
    // Day2.part1() ; Day2.part2()
    // Day3.part1(); Day3.part2()
    // Day4.part1(); Day4.part2()
    // Day5.part1(); Day5.part2()
    // Day6.part1(); Day6.part2()
    // Day7.part1(); Day7.part2()
    // Day8.part1(); Day8.part2()
    // Day9.part1(); Day9.part2()
    // Day10.part1(); Day10.part2()
    // Day11.part1(); Day11.part2()
    Day12.part1()
    0

open System.IO
open System.Text.RegularExpressions


let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

let rec gridSize maxX maxY lines =
    match lines with
    | (x1, y1, x2, y2) :: tail -> gridSize (max (max x1 maxX) x2) (max (max y1 maxY) y2) tail
    | _ -> maxX, maxY

let readFile fileName =
    let toLine line =
        match line with
        | Regex "(\d+),(\d+) -> (\d+),(\d+)" [x1; y1; x2; y2] -> (x1 |> int, y1 |> int, x2 |> int, y2 |> int)
        | _ -> failwith "Parsing error"

    fileName |> File.ReadAllLines |> Seq.map toLine |> List.ofSeq

let drawLine (grid: int[,]) (x1, y1, x2, y2) =
    let incX = if x1 = x2 then 0
               elif x1 < x2 then 1
               else -1
    let incY = if y1 = y2 then 0
               elif y1 < y2 then 1
               else -1
    let mutable x = x1
    let mutable y = y1
    while x <> (x2 + incX) || y <> (y2 + incY) do
        grid[x, y] <- grid[x, y] + 1
        x <- x + incX
        y <- y + incY

let fillGrid lines =
    let maxX, maxY = gridSize 0 0 lines
    let grid = Array2D.create (maxX+1) (maxY+1) 0
    lines |> List.iter (drawLine grid)
    let res = [ for y in 0..maxY do
                    for x in 0..maxX do
                        grid[x, y] ] |> List.filter (fun x -> x > 1) |> List.length
    res

let puzzle1 fileName =
    let lines = readFile fileName |> List.filter (fun (x1, y1, x2, y2) -> x1 = x2 || y1 = y2)
    let res =fillGrid lines
    printfn $"Puzzle1: {res}"


let puzzle2 fileName =
    let lines = readFile fileName |> List.filter (fun (x1, y1, x2, y2) -> x1 = x2 || y1 = y2 || abs (x2-x1) = abs (y2-y1))
    let res =fillGrid lines
    printfn $"Puzzle2: {res}"

puzzle1 "../Input5.txt"
puzzle2 "../Input5.txt"

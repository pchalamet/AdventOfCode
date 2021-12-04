open System.IO
open System


let readFile fileName =
    use file = File.OpenText fileName
    let numbers = file.ReadLine().Split(",")

    let loadBoards() =
        seq {
            while file.EndOfStream |> not do
                file.ReadLine() |> ignore
                let board = Array2D.create 5 5 struct("", false)
                for i in 0..4 do
                    let line = file.ReadLine().Split(" ", StringSplitOptions.RemoveEmptyEntries)
                    for j in 0..4 do
                        board[j, i] <- (line[j], false)
                yield board
        }
    let boards = loadBoards() |> List.ofSeq
    numbers, boards





let markNumber number (board: struct(string * bool)[,]) =
    for i in 0..4 do
        for j in 0..4 do
            let struct(boardNumber, _) = board[j, i]
            if boardNumber = number then board[j, i] <- (number, true)

let row idx (board: struct(string * bool)[,]) =
    [| board[idx, 0]; board[idx, 1]; board[idx, 2]; board[idx, 3]; board[idx, 4] |] 

let col idx (board: struct(string * bool)[,]) =
    [| board[0, idx]; board[1, idx]; board[2, idx]; board[3, idx]; board[4, idx] |] 

let isComplete (line: struct(string * bool)[]) = 
    line |> Array.fold (fun acc struct(_, set) -> set && acc) true

let checkBoardComplete (board: struct(string * bool)[,]) =
    let rows = [0..4] |> List.map (fun idx -> board |> row idx)
    let cols = [0..4] |> List.map (fun idx -> board |> col idx)
    rows @ cols |> List.exists isComplete

let unmarkedNumbers (board: struct(string * bool)[,]) =
    [ for i in 0..4 do
          for j in 0..4 do
              let struct(number, marked) = board[j, i]
              if marked |> not then yield number |> int ]

let puzzle1 fileName =
    let numbers, boards = readFile fileName

    let rec drawNumbers numbers =
        match numbers with
        | number :: numbers -> boards |> List.iter (markNumber number)
                               match boards |> List.tryFind checkBoardComplete with
                               | Some board -> Some (number, board |> Array2D.copy)
                               | _ -> drawNumbers numbers
        | _ -> None

    match drawNumbers (numbers |> List.ofArray) with
    | Some (number, board) -> let sum = board |> unmarkedNumbers
                              let sum = sum |> List.sum
                              let num = number |> int
                              let res = sum * num
                              printfn $"Puzzle1: {res}"
    | _ -> ()

let puzzle2 fileName =
    let numbers, boards = readFile fileName

    let rec drawNumbers boards numbers =
        seq {
            match numbers with
            | number :: numbers -> boards |> List.iter (markNumber number)

                                   let completedBoards = boards |> List.filter checkBoardComplete
                                                                |> List.map (fun arr -> number, Array2D.copy arr)
                                   let uncompletedBoards = boards |> List.filter (not << checkBoardComplete)
                                   yield! completedBoards
                                   yield! drawNumbers uncompletedBoards numbers
            | _ -> ()
        }

    let (number, board) = drawNumbers boards (numbers |> List.ofArray) |> Seq.last
    let sum = board |> unmarkedNumbers
    let sum = sum |> List.sum
    let num = number |> int
    let res = sum * num
    printfn $"Puzzle2: {res}"

puzzle1 "Test4.txt"
puzzle2 "Test4.txt"

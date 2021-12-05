open System.IO
open System


let readFile fileName =
    use file = File.OpenText fileName
    let numbers = file.ReadLine().Split(",")

    let loadBoards() =
        seq {
            while file.EndOfStream |> not do
                file.ReadLine() |> ignore
                let board = Array2D.create 5 5 ""
                for i in 0..4 do
                    let line = file.ReadLine().Split(" ", StringSplitOptions.RemoveEmptyEntries)
                    for j in 0..4 do
                        board[j, i] <- line[j]
                yield board
        }
    let boards = loadBoards() |> List.ofSeq
    numbers, boards





let markNumber number (board: string[,]) =
    for i in 0..4 do
        for j in 0..4 do
            let boardNumber = board[j, i]
            if boardNumber = number then board[j, i] <- ""

let row idx (board: string[,]) =
    [| board[idx, 0]; board[idx, 1]; board[idx, 2]; board[idx, 3]; board[idx, 4] |] 

let col idx (board: string[,]) =
    [| board[0, idx]; board[1, idx]; board[2, idx]; board[3, idx]; board[4, idx] |] 

let isComplete (line: string[]) = 
    line |> Array.fold (fun acc number -> (number = "") && acc) true

let checkBoardComplete (board: string[,]) =
    let rows = [0..4] |> List.map (fun idx -> board |> row idx)
    let cols = [0..4] |> List.map (fun idx -> board |> col idx)
    rows @ cols |> List.exists isComplete

let unmarkedNumbers (board: string[,]) =
    [ for i in 0..4 do
          for j in 0..4 do
              let number = board[j, i]
              if number <> "" then yield number |> int ]


let findCompletedBoard boards numbers order =
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

    let (number, board) = drawNumbers boards (numbers |> List.ofArray) |> order
    let sum = board |> unmarkedNumbers
    let sum = sum |> List.sum
    let num = number |> int
    let res = sum * num
    res

let puzzle1 fileName =
    let numbers, boards = readFile fileName
    let res = findCompletedBoard boards numbers Seq.head
    printfn $"Puzzle1: {res}"

let puzzle2 fileName =
    let numbers, boards = readFile fileName
    let res = findCompletedBoard boards numbers Seq.last
    printfn $"Puzzle2: {res}"

puzzle1 "../Input4.txt"
puzzle2 "../Input4.txt"

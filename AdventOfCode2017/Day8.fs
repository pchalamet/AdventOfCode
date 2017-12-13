module Day8

open System.Linq

let input = System.IO.File.ReadAllLines("data/Day8.txt")


type Operation =
    | Inc
    | Dec
with
    static member Parse s =
        match s with
        | "inc" -> Inc
        | "dec" -> Dec
        | x -> failwithf "unknown operation %s" x

type Compare =
    | Lower
    | LowerOrEqual
    | Equal
    | NotEqual
    | Greater
    | GreaterOrEqual
with
    static member Parse s =
        match s with
        | "<" -> Lower
        | "<=" -> LowerOrEqual
        | "==" -> Equal
        | "!=" -> NotEqual
        | ">" -> Greater
        | ">=" -> GreaterOrEqual
        | x -> failwithf "unknown comparer %s" x

type Operand =
    { Register : string
      Operation : Operation
      Value : int
      ConditionRegister : string
      ConditionOperation : Compare
      ConditionValue : int }
with
    static member Parse (s : string) =
        //um inc -671 if lbf != 5
        let items = s.Split(' ')
        { Register = items.[0]
          Operation = items.[1] |> Operation.Parse
          Value = items.[2] |> int
          ConditionRegister = items.[4]
          ConditionOperation = items.[5] |> Compare.Parse
          ConditionValue = items.[6] |> int }
    
let getValue register (registers : Map<string, int>) =
    match registers |> Map.tryFind register with
    | Some x -> x
    | None -> 0

let setValue register value (registers : Map<string, int>) =
    let tmpRegs = if registers |> Map.containsKey register then
                      registers |> Map.remove register
                  else
                      registers

    tmpRegs |> Map.add register value

let evaluate (operand : Operand) (registers : Map<string, int>) =
    let condRegValue = registers |> getValue operand.ConditionRegister
    let condition = match operand.ConditionOperation with
                    | Lower -> condRegValue < operand.ConditionValue
                    | LowerOrEqual -> condRegValue <= operand.ConditionValue
                    | Equal -> condRegValue = operand.ConditionValue
                    | NotEqual -> condRegValue <> operand.ConditionValue
                    | Greater -> condRegValue > operand.ConditionValue
                    | GreaterOrEqual -> condRegValue >= operand.ConditionValue

    let regValue = registers |> getValue operand.Register
    let newRegValue = match operand.Operation with
                        | Inc when condition -> regValue + operand.Value
                        | Dec when condition -> regValue - operand.Value
                        | _ -> regValue
    registers |> setValue operand.Register newRegValue


let rec evaluateOperands (operands : Operand list) maxReg (registers : Map<string, int>) =
    match operands with
    | operand :: tail -> let newRegs = evaluate operand registers 
                         let currMax = newRegs.Select(fun x -> x.Value).Max()
                         let newMax = if currMax > maxReg then currMax else maxReg
                         newRegs |> evaluateOperands tail newMax
    | _ -> registers.Select(fun x -> x.Value).Max(), maxReg


let Part2 () =
    let operands = input |> Seq.map Operand.Parse |> List.ofSeq
    let max = evaluateOperands operands 0 Map.empty
    printfn "Max value is %d" (max |> snd)


let Part1() =
    let operands = input |> Seq.map Operand.Parse |> List.ofSeq
    let max = evaluateOperands operands 0 Map.empty
    printfn "Max value is %d" (max |> fst)

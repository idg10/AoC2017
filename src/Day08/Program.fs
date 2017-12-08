// Learn more about F# at http://fsharp.org

open System

open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

type OpType =
    | Inc
    | Dec

type ComparisonType =
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    | EqualTo
    | NotEqualTo

type SourceOperand = Register of string | Literal of int

type Comparison =
    {
        left: SourceOperand
        comparisonType: ComparisonType
        right: SourceOperand
    }

type Instruction =
    {
        targetRegister: string
        operation: OpType
        sourceOperand: SourceOperand
        condition: Comparison
    }

let pRegisterName<'a> : Parser<string, 'a> = many1Satisfy isLetter
let pOp<'a> : Parser<OpType, 'a> = 
        (stringReturn "inc" Inc)
    <|> (stringReturn "dec" Dec)
let pComp<'a> : Parser<ComparisonType, 'a> = 
        (stringReturn ">=" GreaterThanOrEqual)
    <|> (stringReturn ">" GreaterThan)
    <|> (stringReturn "==" EqualTo)
    <|> (stringReturn "!=" NotEqualTo)
    <|> (stringReturn "<=" LessThanOrEqual)
    <|> (stringReturn "<" LessThan)
let pLiteralOperand<'a> : Parser<SourceOperand, 'a> = pint32 |>> Literal
let pRegisterOperand<'a> : Parser<SourceOperand, 'a> = pRegisterName |>> Register
let pSourceOperand<'a> : Parser<SourceOperand, 'a> = pLiteralOperand <|> pRegisterOperand

let pCondition<'a> : Parser<Comparison, 'a> =
    pipe3
        (pstring "if" >>. spaces >>. pSourceOperand)
        (spaces >>. pComp)
        (spaces >>. pSourceOperand)
        (fun left comp right -> { left = left; comparisonType = comp; right = right })

let pInputLine<'a> : Parser<Instruction, 'a> =
    pipe4
        (pRegisterName .>> spaces)
        (pOp .>> spaces)
        (pSourceOperand .>> spaces)
        pCondition
        (fun target op source condition ->
            {
                targetRegister = target
                operation = op
                sourceOperand = source
                condition = condition
            })

let parseInputLine s = testp pInputLine s
let parseInput rows = Seq.map parseInputLine rows


type CpuState =
    {
        registerValues: Map<string, int>
    }

let initialCpuState = { registerValues = Map.empty }

let executeInstruction (instruction: Instruction) (state: CpuState) =
    let getRegisterValue r =
        match Map.tryFind r state.registerValues with
        | Some value -> value
        | None -> 0
    let getSourceOperandValue op =
        match op with
        | Literal i -> i
        | Register r -> getRegisterValue r

    let condLeft = getSourceOperandValue instruction.condition.left
    let condRight = getSourceOperandValue instruction.condition.right
    let shouldExecute =
        match instruction.condition.comparisonType with
        | LessThan -> condLeft < condRight
        | LessThanOrEqual -> condLeft <= condRight
        | EqualTo -> condLeft = condRight
        | NotEqualTo -> condLeft <> condRight
        | GreaterThanOrEqual -> condLeft >= condRight
        | GreaterThan -> condLeft > condRight
    if shouldExecute then
        let currentTargetRegisterValue = getRegisterValue instruction.targetRegister
        let sourceOperandValue = getSourceOperandValue instruction.sourceOperand
        let targetValue =
            match instruction.operation with
            | Inc -> currentTargetRegisterValue + sourceOperandValue            
            | Dec -> currentTargetRegisterValue - sourceOperandValue            
        {
            registerValues = Map.add instruction.targetRegister targetValue state.registerValues
        }
    else
        state

let executeProgram program =
    program
    |> Seq.scan
        (fun state instruction ->
            executeInstruction instruction state)
        initialCpuState
    |> Seq.skip 1


// The instructions tell us to find the "largest" value, but the example shows a final state in
// which register a has a value of 1, and c has a value of -10. I'd say "c" is larger, by exactly
// one order of magnitude, but the instructions tell us the largest value is 1.
// So I guess they mean highest.
// Let's calculate both...
let maxRegisterValue criterion (state: CpuState) =
    let maxKvp = Seq.maxBy criterion state.registerValues
    (maxKvp.Key, maxKvp.Value)

let largestRegisterValue = maxRegisterValue (fun kvp -> abs kvp.Value)
let highestRegisterValue = maxRegisterValue (fun kvp -> kvp.Value)
let getMaxes (state: CpuState) =
    (largestRegisterValue state, highestRegisterValue state)

let testInput = """
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""

let testLines = splitIntoRows testInput
let testProgram = parseInput testLines |> List.ofSeq

let inputProgram =
    getEmbeddedRows ()
    |> parseInput

[<EntryPoint>]
let main argv =
    // Parser testing
    testProgram.[0] =!
        {
            targetRegister = "b"; operation = Inc; sourceOperand = Literal 5
            condition = { left = Register "a"; comparisonType = GreaterThan; right = Literal 1 }
        }
    testProgram.[1] =!
        {
            targetRegister = "a"; operation = Inc; sourceOperand = Literal 1
            condition = { left = Register "b"; comparisonType = LessThan; right = Literal 5 }
        }
    testProgram.[2] =!
        {
            targetRegister = "c"; operation = Dec; sourceOperand = Literal -10
            condition = { left = Register "a"; comparisonType = GreaterThanOrEqual; right = Literal 1 }
        }
    testProgram.[3] =!
        {
            targetRegister = "c"; operation = Inc; sourceOperand = Literal -20
            condition = { left = Register "c"; comparisonType = EqualTo; right = Literal 10 }
        }

    let testProgramSteps = executeProgram testProgram |> List.ofSeq
    printfn "%A" testProgramSteps
    testProgramSteps.[0].registerValues.Count =! 0
    testProgramSteps.[1].registerValues.Count =! 1
    testProgramSteps.[1].registerValues.["a"] =! 1
    testProgramSteps.[2].registerValues.Count =! 2
    testProgramSteps.[2].registerValues.["a"] =! 1
    testProgramSteps.[2].registerValues.["c"] =! 10
    testProgramSteps.[3].registerValues.Count =! 2
    testProgramSteps.[3].registerValues.["a"] =! 1
    testProgramSteps.[3].registerValues.["c"] =! -10

    let testFinalState = List.last testProgramSteps
    let ((testLargestReg, testLargestValue), (testHighestReg, testHighestValue)) = getMaxes testFinalState
    testLargestValue =! -10
    testLargestReg =! "c"
    testHighestValue =! 1
    testHighestReg =! "a"

    let inputProgramSteps = executeProgram inputProgram
    let inputFinalState = Seq.last inputProgramSteps
    let ((part1LargestReg, part1LargestValue), (part1HighestReg, part1HighestValue)) = getMaxes inputFinalState

    printfn "Part 1: highest value %d was in register %s" part1HighestValue part1HighestReg
    printfn "Part 1: largest value %d was in register %s" part1LargestValue part1LargestReg
    0 // return an integer exit code

// Learn more about F# at http://fsharp.org

open System

open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

type SourceOperand =
    | Register of name:char
    | Value of bigint

type Instruction =
    | Snd of freq:SourceOperand
    | Set of x:char * y:SourceOperand
    | Add of x:char * y:SourceOperand
    | Mul of x:char * y:SourceOperand
    | Mod of x:char * y:SourceOperand
    | Rcv of x:char
    | JumpIfGreaterThanZero of x:SourceOperand * y:SourceOperand

let pRegisterName<'a> : Parser<char, 'a> = anyChar
let pValueOperand<'a> : Parser<SourceOperand, 'a> = pint32 |>> bigint |>> Value
let pRegisterOperand<'a> : Parser<SourceOperand, 'a> = pRegisterName |>> Register


let pSourceOperand<'a> : Parser<SourceOperand, 'a> = pValueOperand <|> pRegisterOperand
let pBinary<'a> (name: string) ctor : Parser<Instruction, 'a>  =
    pipe2
        (pstring name >>. spaces >>. pRegisterName .>> spaces)
        pSourceOperand
        (fun x y -> ctor (x, y))

let pSet<'a> : Parser<Instruction, 'a> = pBinary "set" Set
let pAdd<'a> : Parser<Instruction, 'a> = pBinary "add" Add
let pMul<'a> : Parser<Instruction, 'a> = pBinary "mul" Mul
let pMod<'a> : Parser<Instruction, 'a> = pBinary "mod" Mod
let pJumpIfGreaterThanZero<'a> : Parser<Instruction, 'a> =
    pipe2
        (pstring "jgz" >>. spaces >>. pSourceOperand .>> spaces)
        pSourceOperand
        (fun x y -> JumpIfGreaterThanZero (x, y))

let pSound<'a> : Parser<Instruction, 'a> = (pstring "snd" >>. spaces >>. pSourceOperand) |>> Snd
let pRecover<'a> : Parser<Instruction, 'a> = (pstring "rcv" >>. spaces >>. pRegisterName) |>> Rcv

let pInstruction<'a> : Parser<Instruction, 'a> = pSet <|> pAdd <|> pMul <|> pMod <|> pJumpIfGreaterThanZero <|> pSound <|> pRecover

let parseLine = testp pInstruction


type ProcessState =
    {
        ip: bigint
        regs: Map<char, bigint>
        lf: bigint option
        rf: bigint option

        inputQueue: bigint list
    }
let initialState = { ip = 0I; regs = Map.empty; lf = None; rf = None; inputQueue = [] }

let getRegisterValue state rn =
    match Map.tryFind rn state.regs with
    | Some value -> value
    | None -> 0I


// Process a single instruction, with paramaterization for the Snd and Rcv instructions, since
// those do completely different things in parts 1 and 2.
// This then takes an instruction and a current state, and produces a tuple.
// The tuple's first value is a ProcessState option - normally this will be the next state, but
// if the Rcv instruction ended up blocking (because we're using the part 2 implementation, and
// the input queue is empty) it will be None.
// The tuple's second value is a bigint option which, if non-None, is an output produced by this
// instruction to go onto the other task's input queue.
let processInstructionCore
    (handleSnd: bigint -> ProcessState -> ProcessState * bigint option)
    (handleRcv: char -> ProcessState -> ProcessState option)
    (instruction: Instruction)
    (state:ProcessState) =
    let getRegisterValue = getRegisterValue state
    let getSourceOperandValue op =
        match op with
        | Register rn -> getRegisterValue rn
        | Value v -> v

    let setRegAndIncrementIp x value =
        (
            Some
                {
                    state with
                        ip = state.ip + 1I
                        regs = Map.add x value state.regs
                        rf = None
                },
            None
        )
    let binaryOp x y fn =
        let currentValue = getRegisterValue x
        let valueToAdd = getSourceOperandValue y
        let newValue = fn currentValue valueToAdd
        setRegAndIncrementIp x newValue
    match instruction with
    | Set (x, y) -> setRegAndIncrementIp x (getSourceOperandValue y)
    | Add (x, y) -> binaryOp x y (+)
    | Mul (x, y) -> binaryOp x y (*)
    | Mod (x, y) -> binaryOp x y (%)
    | Snd x ->
        let v = getSourceOperandValue x
        let nextState, output = handleSnd v state
        (Some nextState, output)
    | Rcv rn -> (handleRcv rn state, None)
    | JumpIfGreaterThanZero (x, y) ->
        let v = getSourceOperandValue x
        let nextState =
            if v <= 0I then
                {
                    state with
                        ip = state.ip + 1I
                        rf = None
                }
            else
                {
                    state with
                        ip = state.ip + (getSourceOperandValue y)
                        rf = None
                }
        (Some nextState, None)

let sound fv state =
    (
        {
            state with
                ip = state.ip + 1I
                lf = Some fv
                rf = None
        },
        None
    )

let recover rn state =
    let rv = getRegisterValue state rn
    Some
        {
            state with
                ip = state.ip + 1I
                rf = if rv = 0I then None else state.lf
        }

let processInstruction = processInstructionCore sound recover

let send v state =
    (
        {
            state with
                ip = state.ip + 1I
        },
        Some v
    )

let receive rn state =
    match state.inputQueue with
    | [] -> None
    | h::remainingInput ->
        Some
            {
                state with
                    ip = state.ip + 1I
                    regs = Map.add rn h state.regs
                    inputQueue = remainingInput
            }

let processInstruction2 = processInstructionCore send receive

// Part 1 execution - doesn't handle blocking.
let run (instructions: Instruction[]) =
    Seq.unfold
        (fun (state: ProcessState) ->
            if state.ip > (bigint instructions.Length) then None
            else
                let (nextState, _) = processInstruction instructions.[int state.ip] state
                Some (nextState, nextState.Value))
        initialState



// Returns (ProcessState option * bigint list), where the list is the output
// sent during execution.
let runOneThreadUntilBlocked (instructions: Instruction[]) (state: ProcessState) =
    let rec processStep (state: ProcessState) (outputList: bigint list) =
        if state.ip > (bigint instructions.Length) then (None, outputList)
        else
            let (nextStateOpt, outputItem) = processInstruction2 instructions.[int state.ip] state
            match nextStateOpt with
            | Some nextState ->
                let output =
                    match outputItem with
                    | Some item -> item::outputList
                    | None -> outputList
                processStep nextState output
            | None -> (Some state, outputList)
    let finalState, output = processStep state []
    (finalState, List.rev output)
    
type ThreadPairState =
    {
        ps1: ProcessState option
        ps2: ProcessState option
    }
let initialPairState = { ps1 = Some initialState; ps2 = Some { initialState with regs = (Map.ofSeq [('p',1I)]) } }

let run2Threads (instructions: Instruction[]) =
    let run1 = runOneThreadUntilBlocked instructions
    let addInput s i = { s with inputQueue = List.append s.inputQueue i }
    let runState stateToRun (stateToUpdateWithInput: ProcessState option)=
        match stateToRun with
        | Some s ->
            let outcome, output = run1 s
            let state = stateToUpdateWithInput |> Option.map (fun s -> addInput s output)
            (outcome, output, state)
        | None -> (None, [], None)
    Seq.unfold
        (fun (state: ThreadPairState) ->
            let p1StateAfterRun, p1Output, p2StateWithNewInput = runState state.ps1 state.ps2
            let p2StateAfterRun, p2Output, p1StateWithNewInput = runState p2StateWithNewInput p1StateAfterRun

            if List.isEmpty p1Output && List.isEmpty p2Output then None
            else
                Some ((p1Output, p2Output), { ps1 = p1StateWithNewInput; ps2 = p2StateAfterRun}))
        initialPairState


let testInput = """
set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"""

let testInput2 = """
snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d"""

[<EntryPoint>]
let main argv =
    let testProgram = splitIntoRows testInput |> Array.map parseLine

    testProgram.[0] =! Set ('a', Value 1I)
    testProgram.[1] =! Add ('a', Value 2I)
    testProgram.[2] =! Mul ('a', Register 'a')
    testProgram.[3] =! Mod ('a', Value 5I)
    testProgram.[4] =! Snd (Register 'a')
    testProgram.[5] =! Set ('a', Value 0I)
    testProgram.[6] =! Rcv ('a')
    testProgram.[7] =! JumpIfGreaterThanZero (Register 'a', Value -1I)
    testProgram.[8] =! Set ('a', Value 1I)
    testProgram.[9] =! JumpIfGreaterThanZero (Register 'a', Value -2I)

    let testSteps = run testProgram |> Seq.take 12 |> Array.ofSeq

    // set a 1
    testSteps.[0] =! Some { ip = 1I; regs = Map.ofSeq [('a', 1I)]; lf = None; rf = None; inputQueue = [] }
    // add a 2
    testSteps.[1] =! Some { ip = 2I; regs = Map.ofSeq [('a', 3I)]; lf = None; rf = None; inputQueue = [] }
    // mul a a
    testSteps.[2] =! Some { ip = 3I; regs = Map.ofSeq [('a', 9I)]; lf = None; rf = None; inputQueue = [] }
    // mod a 5
    testSteps.[3] =! Some { ip = 4I; regs = Map.ofSeq [('a', 4I)]; lf = None; rf = None; inputQueue = [] }
    // snd a
    testSteps.[4] =! Some { ip = 5I; regs = Map.ofSeq [('a', 4I)]; lf = Some 4I; rf = None; inputQueue = [] }
    // set a 0
    testSteps.[5] =! Some { ip = 6I; regs = Map.ofSeq [('a', 0I)]; lf = Some 4I; rf = None; inputQueue = [] }
    // rcv a
    testSteps.[6] =! Some { ip = 7I; regs = Map.ofSeq [('a', 0I)]; lf = Some 4I; rf = None; inputQueue = [] }
    // jgz a -1
    testSteps.[7] =! Some { ip = 8I; regs = Map.ofSeq [('a', 0I)]; lf = Some 4I; rf = None; inputQueue = [] }
    // set a 1
    testSteps.[8] =! Some { ip = 9I; regs = Map.ofSeq [('a', 1I)]; lf = Some 4I; rf = None; inputQueue = [] }
    // jgz a -2
    testSteps.[9] =! Some { ip = 7I; regs = Map.ofSeq [('a', 1I)]; lf = Some 4I; rf = None; inputQueue = [] }
    // jgz a -1
    testSteps.[10] =! Some { ip = 6I; regs = Map.ofSeq [('a', 1I)]; lf = Some 4I; rf = None; inputQueue = [] }
    // rcv a
    testSteps.[11] =! Some { ip = 7I; regs = Map.ofSeq [('a', 1I)]; lf = Some 4I; rf = Some 4I; inputQueue = [] }


    let program = getEmbeddedRows () |> Array.map parseLine
    
    let steps = run program
    let firstRecovered =
        steps
        |> Seq.choose id
        |> Seq.choose (fun (s: ProcessState) -> s.rf)
        |> Seq.take 1
        |> Seq.exactlyOne

    printfn "Part 1: %A" firstRecovered


    let testProgram2 = splitIntoRows testInput2 |> Array.map parseLine
    let test2Outputs = run2Threads testProgram2 |> Array.ofSeq
    test2Outputs.[0] =! ([1I;2I;0I],[1I;2I;1I])
    test2Outputs.Length =! 1

    let steps2 = run2Threads program
    let numberOfOutputsFrom1 =
        steps2
        |> Seq.sumBy (fun (_, o1) -> o1.Length)

    printfn "Part 2: %d" numberOfOutputsFrom1
    0

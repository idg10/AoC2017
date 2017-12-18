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
    | Sound of freq:SourceOperand
    | Set of x:char * y:SourceOperand
    | Add of x:char * y:SourceOperand
    | Mul of x:char * y:SourceOperand
    | Mod of x:char * y:SourceOperand
    | Recover of x:char
    | JumpIfGreaterThanZero of x:char * y:SourceOperand

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
let pJumpIfGreaterThanZero<'a> : Parser<Instruction, 'a> = pBinary "jgz" JumpIfGreaterThanZero

let pSound<'a> : Parser<Instruction, 'a> = (pstring "snd" >>. spaces >>. pSourceOperand) |>> Sound
let pRecover<'a> : Parser<Instruction, 'a> = (pstring "rcv" >>. spaces >>. pRegisterName) |>> Recover

let pInstruction<'a> : Parser<Instruction, 'a> = pSet <|> pAdd <|> pMul <|> pMod <|> pJumpIfGreaterThanZero <|> pSound <|> pRecover

let parseLine = testp pInstruction


type ProcessState =
    {
        ip: bigint
        regs: Map<char, bigint>
        lf: bigint option
        rf: bigint option
    }
let initialState = { ip = 0I; regs = Map.empty; lf = None; rf = None }

let processInstruction (instruction: Instruction) (state:ProcessState) =
    let getRegisterValue rn =
        match Map.tryFind rn state.regs with
        | Some value -> value
        | None -> 0I
    let getSourceOperandValue op =
        match op with
        | Register rn -> getRegisterValue rn
        | Value v -> v

    let setRegAndIncrementIp x value =
        {
            state with
                ip = state.ip + 1I
                regs = Map.add x value state.regs
                rf = None
        }
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
    | Sound freq ->
        let fv = getSourceOperandValue freq
        {
            state with
                ip = state.ip + 1I
                lf = Some fv
                rf = None
        }
    | Recover rn ->
        let rv = getRegisterValue rn
        {
            state with
                ip = state.ip + 1I
                rf = if rv = 0I then None else state.lf
        }
    | JumpIfGreaterThanZero (x, y) ->
        let cond = getRegisterValue x
        if cond = 0I then
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

let run (instructions: Instruction[]) =
    Seq.unfold
        (fun (state: ProcessState) ->
            if state.ip > (bigint instructions.Length) then None
            else
                let nextState = processInstruction instructions.[int state.ip] state
                Some (nextState, nextState))
        initialState

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


[<EntryPoint>]
let main argv =
    let testProgram = splitIntoRows testInput |> Array.map parseLine

    testProgram.[0] =! Set ('a', Value 1I)
    testProgram.[1] =! Add ('a', Value 2I)
    testProgram.[2] =! Mul ('a', Register 'a')
    testProgram.[3] =! Mod ('a', Value 5I)
    testProgram.[4] =! Sound (Register 'a')
    testProgram.[5] =! Set ('a', Value 0I)
    testProgram.[6] =! Recover ('a')
    testProgram.[7] =! JumpIfGreaterThanZero ('a', Value -1I)
    testProgram.[8] =! Set ('a', Value 1I)
    testProgram.[9] =! JumpIfGreaterThanZero ('a', Value -2I)

    let testSteps = run testProgram |> Seq.take 12 |> Array.ofSeq

    // set a 1
    testSteps.[0] =! { ip = 1I; regs = Map.ofSeq [('a', 1I)]; lf = None; rf = None }
    // add a 2
    testSteps.[1] =! { ip = 2I; regs = Map.ofSeq [('a', 3I)]; lf = None; rf = None }
    // mul a a
    testSteps.[2] =! { ip = 3I; regs = Map.ofSeq [('a', 9I)]; lf = None; rf = None }
    // mod a 5
    testSteps.[3] =! { ip = 4I; regs = Map.ofSeq [('a', 4I)]; lf = None; rf = None }
    // snd a
    testSteps.[4] =! { ip = 5I; regs = Map.ofSeq [('a', 4I)]; lf = Some 4I; rf = None }
    // set a 0
    testSteps.[5] =! { ip = 6I; regs = Map.ofSeq [('a', 0I)]; lf = Some 4I; rf = None }
    // rcv a
    testSteps.[6] =! { ip = 7I; regs = Map.ofSeq [('a', 0I)]; lf = Some 4I; rf = None }
    // jgz a -1
    testSteps.[7] =! { ip = 8I; regs = Map.ofSeq [('a', 0I)]; lf = Some 4I; rf = None }
    // set a 1
    testSteps.[8] =! { ip = 9I; regs = Map.ofSeq [('a', 1I)]; lf = Some 4I; rf = None }
    // jgz a -2
    testSteps.[9] =! { ip = 7I; regs = Map.ofSeq [('a', 1I)]; lf = Some 4I; rf = None }
    // jgz a -1
    testSteps.[10] =! { ip = 6I; regs = Map.ofSeq [('a', 1I)]; lf = Some 4I; rf = None }
    // rcv a
    testSteps.[11] =! { ip = 7I; regs = Map.ofSeq [('a', 1I)]; lf = Some 4I; rf = Some 4I }


    let program = getEmbeddedRows () |> Array.map parseLine
    
    let steps = run program
    let firstRecovered =
        steps
        |> Seq.choose (fun (s: ProcessState) -> s.rf)
        |> Seq.take 1
        |> Seq.exactlyOne

    // 8789 was too high
    printfn "Part 1: %A" firstRecovered
    0 // return an integer exit code

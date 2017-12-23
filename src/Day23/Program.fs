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
    | Set of x:char * y:SourceOperand
    | Sub of x:char * y:SourceOperand
    | Mul of x:char * y:SourceOperand
    | JumpIfNotZero of x:SourceOperand * y:SourceOperand

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
let pSub<'a> : Parser<Instruction, 'a> = pBinary "sub" Sub
let pMul<'a> : Parser<Instruction, 'a> = pBinary "mul" Mul
let pJumpIfNotZero<'a> : Parser<Instruction, 'a> =
    pipe2
        (pstring "jnz" >>. spaces >>. pSourceOperand .>> spaces)
        pSourceOperand
        (fun x y -> JumpIfNotZero (x, y))

let pInstruction<'a> : Parser<Instruction, 'a> = pSet <|> pSub <|> pMul  <|> pJumpIfNotZero

let parseLine = testp pInstruction

type ProcessState =
    {
        ip: bigint
        regs: Map<char, bigint>
    }
let initialState = { ip = 0I; regs = Map.empty }

let getRegisterValue state rn =
    match Map.tryFind rn state.regs with
    | Some value -> value
    | None -> 0I


// Process a single instruction
let processInstruction
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
                },
            None
        )
    let binaryOp x y fn =
        let currentValue = getRegisterValue x
        let valueToUse = getSourceOperandValue y
        let newValue = fn currentValue valueToUse
        setRegAndIncrementIp x newValue
    match instruction with
    | Set (x, y) -> setRegAndIncrementIp x (getSourceOperandValue y)
    | Sub (x, y) -> binaryOp x y (-)
    | Mul (x, y) -> binaryOp x y (*)
    | JumpIfNotZero (x, y) ->
        let v = getSourceOperandValue x
        let nextState =
            if v = 0I then
                {
                    state with
                        ip = state.ip + 1I
                }
            else
                {
                    state with
                        ip = state.ip + (getSourceOperandValue y)
                }
        (Some nextState, None)


let run (instructions: Instruction[]) =
    Seq.unfold
        (fun (state: ProcessState) ->
            if state.ip >= (bigint instructions.Length) then None
            else
                let i = instructions.[int state.ip]
                //printfn "%A" state
                //printfn "%A" i
                //printfn ""
                let (nextState, _) = processInstruction instructions.[int state.ip] state
                Some (nextState, nextState.Value))
        initialState


// Sieve of Eratosthenes from http://fsharpnews.blogspot.co.uk/2010/02/sieve-of-eratosthenes.html
// Not sure this is working - doesn't seem to producing anything like enough primes
let primesUpTo (n: int) =
    let a = ResizeArray<int>(n)
    a.Add(2)
    let grow() =
      let p0 = a.[a.Count-1]+1
      let b = Array.create p0 true
      for di in a do
        let rec loop i =
          if i<b.Length then
            b.[i] <- false
            loop(i+di)
        let i0 = p0/di*di
        loop(if i0<p0 then i0+di-p0 else i0-p0)
      for i=0 to b.Length-1 do
        if b.[i] then a.Add(p0+i)
    seq
        {
            let mutable i = 0
            while (a.[i]) <= n do
                yield a.[i]
                i <- i + 1
                if i = a.Count then grow()
        }


let countNonPrimes start skip repeats =
    let final = start + (skip * repeats)
    let primesInRange =
        primesUpTo final
        |> Seq.filter (fun p -> p >= start)
        |> Set.ofSeq
    seq { 0..repeats }
    |> Seq.filter (fun i -> not <| Set.contains (start + i * 17) primesInRange)
    |> Seq.length


[<EntryPoint>]
let main argv =
    let program = getEmbeddedRows ()  |> Array.map parseLine

    let finalState = run program |> Seq.last
    printf "Final state: %A" finalState

    let mulCount =
        run program
        |> Seq.takeWhile (fun (s: ProcessState option) -> s.IsSome)
        |> Seq.choose id
        |> Seq.filter
            (fun (state: ProcessState) ->
                if state.ip >= (bigint program.Length) then false
                else
                    match program.[int state.ip] with
                    | Mul (_, _) -> true
                    | _ -> false)
        |> Seq.length

    printfn "Part 1: %d" mulCount

    // Rather crazily, it turns out that the code we are supplied with
    // looks at number even spaced by an interval of 17, starting from
    // 107900 (so it looks at 107900, 107917, 107934, etc) and counts
    // how many of these are not prime numbers, stopping at some upper
    // limit, which is 1000 numbers later in our case (17000 higher)

    // With the 'debug' mode (starting with a=1) the start and stop
    // are both 79, so there is only one number checked, and it's prime,
    // which is why h=0 after the program completes in debug mode.
    // Check our fast implementation concurs.
    countNonPrimes 79 79 0 =! 0

    printfn "Part 2: %d" (countNonPrimes 107900 17 1000)

    0

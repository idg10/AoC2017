// Learn more about F# at http://fsharp.org

open System

open Swensen.Unquote
open FParsec

open ParsingHelpers
open TextHandling

type Direction = Left | Right

type Rule =
    {
        write: bool
        move: Direction
        nextState: char
    }

type StateRules =
    {
        ifCurrentTrue: Rule
        ifCurrentFalse: Rule
    }

type Machine =
    {
        initialState: char
        diagnosticAfter: int
        rules: Map<char, StateRules>
    }

let runMachine (m:Machine) =
    Seq.unfold
        (fun (tape: Set<int>, pos:int, state:char) ->
            let currentValue = tape.Contains(pos)
            let rules = m.rules.[state]
            let rule = if currentValue then rules.ifCurrentTrue else rules.ifCurrentFalse
            let newTape =
                tape |> if rule.write then Set.add pos else Set.remove pos
            let newPos =
                match rule.move with
                | Left -> pos - 1
                | Right -> pos + 1
            let newState = rule.nextState
            let r = (newTape, newPos, newState)
            Some (r, r))
        (Set.empty, 0, m.initialState)

let pBeginIn<'a> : Parser<char, 'a> = pstring "Begin in state " >>. anyChar .>> pchar '.'
let pDiagnosticAfter<'a> : Parser<int, 'a> = pstring "Perform a diagnostic checksum after " >>. pint32 .>> pstring " steps."
let pZeroOrOne<'a> : Parser<bool, 'a> = (pchar '0' >>% false) <|> (pchar '1' >>% true)
let pDirection<'a> : Parser<Direction, 'a> = (pstring "left" >>% Left) <|> (pstring "right" >>% Right)

let pRule<'a> : Parser<bool*Rule, 'a> =
    pipe4
        (pstring "If the current value is " >>. pZeroOrOne .>> pchar ':' .>> spaces)
        (pstring "- Write the value " >>. pZeroOrOne .>> pchar '.' .>> spaces)
        (pstring "- Move one slot to the " >>. pDirection .>> pchar '.' .>> spaces)
        (pstring "- Continue with state " >>. anyChar .>> pchar '.' .>> spaces)
        (fun test write direction next ->
            (test,
             {
                write = write
                move = direction
                nextState = next
             }
            ))

let pStateDef<'a> : Parser<char*StateRules, 'a> =
    pipe3
        (pstring "In state " >>. anyChar .>> pchar ':' .>> spaces)
        pRule
        pRule
        (fun state (r1c, rule1) (r2c, rule2) ->
            if r1c = r2c then failwithf "Rules for state '%c' are both for when current value is %b" state r1c
            let tRule, fRule =
                if r1c then (rule1, rule2) else (rule2, rule1)
            (
                state,
                { ifCurrentTrue = tRule; ifCurrentFalse = fRule}
            ))

let pStateDefs<'a> : Parser<Map<char, StateRules>, 'a> =
    many (pStateDef .>> spaces) |>> Map.ofSeq
            

let pMachine<'a> : Parser<Machine, 'a> =
    pipe3
        (spaces >>. pBeginIn .>> spaces)
        (pDiagnosticAfter .>> spaces)
        pStateDefs
        (fun initialState diagnosticAfter stateDefs ->
            {
                initialState = initialState
                diagnosticAfter = diagnosticAfter
                rules = stateDefs
            })

let testInput = """Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A."""

let parseMachine = testp pMachine

[<EntryPoint>]
let main argv =
    let testMachine = parseMachine testInput

    let testSteps = runMachine testMachine |> Seq.take testMachine.diagnosticAfter

    for step in testSteps do
        printfn "%A" step

    let (testFinal, _, _) = testSteps |> Seq.last
    Seq.length testFinal =! 3

    let machine = getEmbeddedInput () |> parseMachine
    let steps = runMachine machine |> Seq.take machine.diagnosticAfter
    let (final, _, _) = steps |> Seq.last

    printfn "Part 1: %d" (Seq.length final)
    0 // return an integer exit code

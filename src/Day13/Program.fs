open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling


// Scanner loop length is 2*(h-1). (*2 because it goes up then down,
// but -1 because it doesn't repeat the endpoints when it turns around.)

let isScannerAtTopAtTick height tick =
    let cycleLength = 2 * (height - 1)
    let positionInCycle = tick % cycleLength
    positionInCycle = 0

let severity (heights: (int * int) seq) startTime =
    heights
    |> Seq.sumBy
        (fun (position, height) ->
            let tickWhenAtPosition = startTime + position
            let isCaught = isScannerAtTopAtTick height tickWhenAtPosition
            if isCaught then position * height
            else 0)


let pInputLine<'a> : Parser<int * int, 'a> =
    pipe2
        (pint32 .>> pstring ":" .>> spaces)
        pint32
        (fun position height -> (position, height))

let parseInputLine s = testp pInputLine s
let parseInput rows = Seq.map parseInputLine rows

let testInput = """
0: 3
1: 2
4: 4
6: 4"""
let testLines = splitIntoRows testInput

[<EntryPoint>]
let main argv =
    let testHeights = parseInput testLines |> List.ofSeq

    // Parser tests
    testHeights.[0] =! (0, 3)
    testHeights.[1] =! (1, 2)
    testHeights.[2] =! (4, 4)
    testHeights.[3] =! (6, 4)

    let testSeverity = severity testHeights 0
    testSeverity =! 24

    let heights = getEmbeddedRows () |> parseInput
    let severity = severity heights 0
    printfn "Part 1: %d" severity
    0

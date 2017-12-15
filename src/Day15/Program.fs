open Swensen.Unquote

let factorA = 16807L
let factorB = 48271L

let genNext factor prev =
    let x = prev * factor
    x % 2147483647L

let genNextA = genNext factorA
let genNextB = genNext factorB

let generateSequence predicate genNext start =
    Seq.unfold
        (fun prev ->
            let next = genNext prev
            Some (next, next))
        start
    |> Seq.filter predicate

let pany _ = true

let generateSequenceA = generateSequence pany genNextA
let generateSequenceB = generateSequence pany genNextB

let isMultipleOf (n: int64) (v:int64) = (v % n) = 0L

let generateSequenceA2 = generateSequence (isMultipleOf 4L) genNextA
let generateSequenceB2 = generateSequence (isMultipleOf 8L) genNextB

let generatePairs startA startB =
    Seq.zip (generateSequenceA startA) (generateSequenceB startB)

let generatePairs2 startA startB =
    Seq.zip (generateSequenceA2 startA) (generateSequenceB2 startB)

let countMatchesInBottom16Bits count (pairs: (int64 * int64) seq) =
    pairs
    |> Seq.take count
    |> Seq.filter
        (fun (a, b) ->
            let ls16A = a % 65536L
            let ls16B = b % 65536L
            ls16A = ls16B)
    |> Seq.length


let startA = 591L
let startB = 393L

[<EntryPoint>]
let main argv =
    let testPairs = generatePairs 65L 8921L |> Seq.take 5 |> List.ofSeq

    testPairs.[0] =! (1092455L, 430625591L)
    testPairs.[1] =! (1181022009L, 1233683848L)
    testPairs.[2] =! (245556042L, 1431495498L)
    testPairs.[3] =! (1744312007L, 137874439L)
    testPairs.[4] =! (1352636452L, 285222916L)

    generatePairs 65L 8921L |> countMatchesInBottom16Bits 5 =! 1

    let testPairs2 = generatePairs2 65L 8921L |> Seq.take 5 |> List.ofSeq

    testPairs2.[0] =! (1352636452L, 1233683848L)
    testPairs2.[1] =! (1992081072L, 862516352L)
    testPairs2.[2] =! (530830436L, 1159784568L)
    testPairs2.[3] =! (1980017072L, 1616057672L)
    testPairs2.[4] =! (740335192L, 412269392L)

    generatePairs2 65L 8921L |> countMatchesInBottom16Bits 1055 =! 0
    generatePairs2 65L 8921L |> countMatchesInBottom16Bits 1056 =! 1

    let pairs = generatePairs startA startB
    let matches = pairs |> countMatchesInBottom16Bits 40000000
    printfn "Part 1: %d" matches

    let pairs2 = generatePairs2 startA startB
    let matches2 = pairs2 |> countMatchesInBottom16Bits 5000000
    printfn "Part 2: %d" matches2
    0

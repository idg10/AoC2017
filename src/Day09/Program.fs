// Learn more about F# at http://fsharp.org

open System
open Swensen.Unquote

open TextHandling

type StreamState =
    {
        nextIsCanceled: bool
        inGarbage: bool
        currentDepth: int
        totalScore: int
        nonCancelledGarbageCount: int
    }

let processStream (s : char seq) =
    s
    |> Seq.fold
        (fun (s: StreamState) c ->
            if s.nextIsCanceled then
                { s with nextIsCanceled = false }
            else if c = '!' then
                { s with nextIsCanceled = true }
            else if s.inGarbage then
                if c = '>' then
                    { s with inGarbage = false }
                else
                    { s with nonCancelledGarbageCount = s.nonCancelledGarbageCount + 1 }
            else
                match c with
                | '<' ->
                    { s with inGarbage = true }
                | '{' ->
                    let depth = s.currentDepth + 1
                    {
                        s with
                            totalScore = s.totalScore + depth
                            currentDepth = depth
                    }
                | '}' ->
                    { s with currentDepth = s.currentDepth - 1 }
                | _ -> s)
        {
            nextIsCanceled = false
            inGarbage = false
            currentDepth = 0
            totalScore = 0
            nonCancelledGarbageCount = 0
        }

let getScore (s : char seq) =
    processStream s
    |> (fun x -> x.totalScore)

let getScoreAndGarbage (s : char seq) =
    processStream s
    |> (fun x -> (x.totalScore, x.nonCancelledGarbageCount))


[<EntryPoint>]
let main argv =
    getScore "{}" =! 1
    getScore "{{{}}}" =! 6
    getScore "{{},{}}" =! 5
    getScore "{{{},{},{{}}}}" =! 16
    getScore "{<a>,<a>,<a>,<a>}" =! 1
    getScore "{{<ab>},{<ab>},{<ab>},{<ab>}}" =! 9
    getScore "{{<!!>},{<!!>},{<!!>},{<!!>}}" =! 9
    getScore "{{<a!>},{<a!>},{<a!>},{<ab>}}" =! 3

    let inputSeq = getInputAsCharSequence ()
    // Part 1 version:
    //let score = getScore inputSeq
    let (score, garbage) = getScoreAndGarbage inputSeq

    getScoreAndGarbage "<>" =! (0, 0)
    getScoreAndGarbage "<random characters>" =! (0, 17)
    getScoreAndGarbage "<<<<>" =! (0, 3)
    getScoreAndGarbage "<{!>}>" =! (0, 2)
    getScoreAndGarbage "<!!>" =! (0, 0)
    getScoreAndGarbage "<!!!>>" =! (0, 0)
    getScoreAndGarbage "<{o\"i!a,<{i<a>" =! (0, 10)
    

    printfn "Part 1: %d" score
    printfn "Part 2: %d" garbage
    0 // return an integer exit code

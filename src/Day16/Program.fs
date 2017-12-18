// Learn more about F# at http://fsharp.org

open System

open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling
open KnotHashing

type DanceStep =
    | Spin of int
    | Exchange of int*int
    | Partner of char*char


let pSpin<'a> : Parser<DanceStep, 'a> =  (pstring "s" >>. pint32) |>> Spin
let pExchange<'a> : Parser<DanceStep, 'a> =
    pipe2
        (pstring "x" >>. pint32)
        (pstring "/" >>. pint32)
        (fun a b -> Exchange (a, b))
let pPartner<'a> : Parser<DanceStep, 'a> =
    pipe2
        (pstring "p" >>. anyChar)
        (pstring "/" >>. anyChar)
        (fun a b -> Partner (a, b))

let pStep<'a> : Parser<DanceStep, 'a> = pSpin <|> pExchange <|> pPartner

let parseSteps (input: string) =
    input.Split(',')
    |> Seq.map (testp pStep)
    |> List.ofSeq

let spin size (ps: char[]) =
    let length = ps.Length
    let n = length - size
    Seq.append (Seq.skip n ps) (Seq.take n ps)
    |> Array.ofSeq

let exchange a b (ps: char[]) =
    let result = Array.copy ps
    result.[a] <- ps.[b]
    result.[b] <- ps.[a]
    result

let partner nameA nameB (ps: char[]) =
    let (a, b, _) =
        ps
        |> Seq.fold
            (fun (a: int, b: int, i: int) (name: char) ->
                (
                    (if name = nameA then i else a),
                    (if name = nameB then i else b),
                    (i + 1))
                )
            (-1, -1, 0)
    exchange a b ps

let performStep positions step =
    match step with
    | Spin i -> spin i positions
    | Exchange (a, b) -> exchange a b positions
    | Partner (na, nb) -> partner na nb positions

let dance (steps: DanceStep seq) (initialPositions: char[]) =
    steps
    |> Seq.scan performStep initialPositions
    |> Seq.skip 1

let danceRepeatedly (steps: DanceStep seq) (initialPositions: char[]) =
    let oneDance = dance steps >> Seq.last
    Seq.unfold
        (fun ps ->
            let nextPs = oneDance ps
            Some (nextPs, nextPs))
        initialPositions

let findRep (steps: DanceStep seq) (initialPositions: char[]) =
    danceRepeatedly steps initialPositions
    |> Seq.mapi (fun (i: int) (ps: char[]) -> (ps |> String, i))
    |> Seq.scan
        (fun (seen: Map<string, int>, _: bool, _: string, _: int) (pos, i) ->
            let isRep = Map.containsKey pos seen
            let newMap =
                if isRep then seen else Map.add pos i seen
            (newMap, isRep, pos, i))
        (Map.empty, false, "", 0)
    |> Seq.find (fun (_, seenBefore, _, _) -> seenBefore)

[<EntryPoint>]
let main argv =
    let shortLine = [|'a'..'e'|]
    let fullLine = [|'a'..'p'|]

    //let testSteps = [Spin 1; Exchange (3, 4); Partner ('e', 'b')]
    let testSteps = parseSteps "s1,x3/4,pe/b"
    let testDance = dance testSteps shortLine |> Array.ofSeq

    testDance.[0] =! Array.ofSeq "eabcd"
    testDance.[1] =! Array.ofSeq "eabdc"
    testDance.[2] =! Array.ofSeq "baedc"

    let input = getEmbeddedInput ()
    let inputSteps = parseSteps input
    let dancePositions = dance inputSteps fullLine
    let finalPosition = dancePositions |> Seq.last
    let finalPositonAsString = finalPosition |> String

    printfn "Part 1: %s" finalPositonAsString

    let (posToIndex, _, repeatingPosition, secondIndex) = findRep inputSteps fullLine
    let firstIndex = posToIndex.[repeatingPosition] 
    let repLength = secondIndex - firstIndex
    printfn "Part 2: position '%s' appears at %d and %d, a period of %d" repeatingPosition secondIndex firstIndex repLength
    let requiredReps = 1000000000 % repLength
    let billionthPosition =
        danceRepeatedly inputSteps fullLine
        |> Seq.skip (requiredReps - 1)
        |> Seq.take 1
        |> Seq.exactlyOne
        |> String
    printfn "Part 2: %s" billionthPosition
    0

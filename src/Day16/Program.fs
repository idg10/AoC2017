// Learn more about F# at http://fsharp.org

open System

open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

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

[<EntryPoint>]
let main argv =
    let shortLine = [|'a'..'e'|]

    //let testSteps = [Spin 1; Exchange (3, 4); Partner ('e', 'b')]
    let testSteps = parseSteps "s1,x3/4,pe/b"
    let testDance = dance testSteps shortLine |> Array.ofSeq

    testDance.[0] =! Array.ofSeq "eabcd"
    testDance.[1] =! Array.ofSeq "eabdc"
    testDance.[2] =! Array.ofSeq "baedc"

    let input = getEmbeddedInput ()
    let inputSteps = parseSteps input
    let dancePositions = dance inputSteps [|'a'..'p'|]
    let finalPosition = dancePositions |> Seq.last
    let finalPositonAsString = finalPosition |> String

    printfn "Part 1: %s" finalPositonAsString
    0

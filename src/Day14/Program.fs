open System
open Swensen.Unquote

open KnotHashing
open TextHandling

let toBinary (digits: int) (value: int) =
    [1..digits]
    |> Seq.fold
        (fun (digits: bool list, v, digitsRemaining) _ ->
            let digit = v % 2 = 1
            (digit :: digits, v / 2, digitsRemaining - 1))
        ([], value, digits)
    |> fun (v, _, _) -> v
 
let toBinary4 = toBinary 4

let makeBinary (hexText: string) =
    hexText
    |> Seq.map (fun hexDigit -> Int32.Parse(string hexDigit, Globalization.NumberStyles.HexNumber) |> toBinary4)
    |> Seq.concat
let makeRow hashInput =
    let hash = hashString hashInput
    makeBinary hash

let makeRowsForInput input =
    [0..127]
    |> Seq.map
        (fun i ->
            let text = sprintf "%s-%d" input i
            makeRow text)


let makeBinaryString t = 
    makeBinary t |> Seq.map (fun b -> if b then '1' else '0') |> Array.ofSeq |> String;;


let countUsed (grid: (bool seq) seq) =
    grid
    |> Seq.sumBy (Seq.sumBy (fun b -> if b then 1 else 0))

[<EntryPoint>]
let main argv =

    makeBinaryString "a0c2017" =! "1010000011000010000000010111"
    let testInput = "flqrgnkx"
    let testRows = makeRowsForInput testInput
    let croppedTestGrid =
        testRows
        |> Seq.map (Seq.take 8 >> List.ofSeq >> Seq.map (fun b -> if b then '#' else '.') >> Array.ofSeq >> String)
        |> Seq.take 8
        |> List.ofSeq
    
    croppedTestGrid.[0] =! "##.#.#.."
    croppedTestGrid.[1] =! ".#.#.#.#"
    croppedTestGrid.[2] =! "....#.#."
    croppedTestGrid.[3] =! "#.#.##.#"
    croppedTestGrid.[4] =! ".##.#..."
    croppedTestGrid.[5] =! "##..#..#"
    croppedTestGrid.[6] =! ".#...#.."
    croppedTestGrid.[7] =! "##.#.##."

    countUsed testRows =! 8108
    
    let input = "jxqlasbh"
    let rows = makeRowsForInput input

    printfn "Part 1: %d" (countUsed rows)
    0

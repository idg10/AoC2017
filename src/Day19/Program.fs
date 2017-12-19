open System

open Swensen.Unquote

open TextHandling

type MapElement =
    | Bar
    | Corner
    | Letter of char


let processLines (lines: string seq) =
    lines
    |> Seq.mapi (fun y cs -> cs |> Seq.mapi (fun x c -> (x, y, c)))
    |> Seq.collect id
    |> Seq.fold
        (fun (m: Map<int*int, MapElement>) (x, y, c) ->
            if c = ' ' then m
            else
                let elem =
                    match c with
                    | '|' | '-' -> Bar
                    | '+' -> Corner
                    | c -> Letter c
                Map.add (x, y) elem m)
        Map.empty

let findStart (m: Map<int*int, MapElement>) =
    let x =
        seq { 0..(Int32.MaxValue) }
        |> Seq.find (fun x -> Map.containsKey (x, 0) m)
    (x, 0)

type Direction = Up | Down | Left | Right
let dv d =
    match d with
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)
let move d (x, y) =
    let (dx, dy) = dv d
    (x + dx, y + dy)
   
let next (m: Map<int*int, MapElement>) (p:int*int) (d: Direction) =
    match Map.tryFind p m with
    | None -> None
    | Some currentElement ->
        match currentElement with
        | Bar -> Some (None, move d p, d)
        | Letter c -> Some (Some c, move d p , d)
        | Corner ->
            let (d1, d2) =
                match d with
                | Up | Down -> (Left, Right)
                | Left | Right -> (Up, Down)

            let t1 = move d1 p
            if Map.containsKey t1 m then Some (None, t1, d1)
            else
                let t2 = move d2 p
                if Map.containsKey t2 m then Some (None, t2, d2)
                else None

let findPath m =
    let start = findStart m
    Seq.unfold
        (fun (p:int*int, d: Direction) ->
            match next m p d with
            | Some (c, np, nd) -> Some ((c, np), (np, nd))
            | None -> None)
        (start, Down)
        
let findPathLetters m =
    findPath m
    |> Seq.choose fst
    |> Array.ofSeq
    |> String

let pathLength m =
    findPath m
    |> Seq.length

let testInput = """     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+"""

let testRows = splitIntoRows testInput

[<EntryPoint>]
let main argv =

    let testMap = processLines testRows
    findPathLetters testMap =! "ABCDEF"

    let testInput = getEmbeddedRows ()
    let map = processLines testInput
    let result = findPathLetters map

    printfn "Part 1: %s" result

    pathLength testMap =! 38

    let realLength = pathLength map
    printfn "Part 2: %d" realLength
    0

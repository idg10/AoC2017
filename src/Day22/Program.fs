// Learn more about F# at http://fsharp.org

open System

open Swensen.Unquote

open Geometry
open TextHandling

type Carrier =
    {
        d: Direction
        p: int*int
    }

let loadMap (rows: string seq) =
    rows
    |> Seq.mapi (fun y row -> (y, row))
    |> Seq.collect (fun (y, row) -> row |> Seq.mapi (fun x c -> (x, y, c)))        
    |> Seq.fold
        (fun (m: Set<int*int>) (x:int, y: int, c: char) ->
            if c = '#' then
                Set.add (x, y) m
            else
                m)
        Set.empty

let move (m: Set<int*int>, c: Carrier) =
    let infected = Set.contains c.p m
    let newDirection =
        match c.d with
        | Up -> if infected then Right else Left
        | Down -> if infected then Left else Right
        | Left -> if infected then Up else Down
        | Right -> if infected then Down else Up
    let dx, dy =
        match newDirection with
        | Up -> (0, -1)
        | Down -> (0, 1)
        | Left -> (-1, 0)
        | Right -> (1, 0)
    let newMap =
        if infected then
            Set.remove c.p m
        else
            Set.add c.p m
    let cx, cy = c.p
    let newCarrier =
        {
            p = (cx + dx, cy + dy)
            d = newDirection
        }
    let didInfect = not infected
    (newMap, newCarrier, didInfect)

   
let testInput = """..#
#..
..."""

let getMapSize (m: Set<int*int>) =
    m
    |> Seq.fold
        (fun (minX, maxX, minY, maxY) (x, y) ->
            (min x minX, max x maxX, min y minY, max y maxY))
        (Int32.MaxValue, Int32.MinValue, Int32.MaxValue, Int32.MinValue)

let printMap (m: Set<int*int>, c: Carrier, _: bool) =
    let minX, maxX, minY, maxY = getMapSize m
    let (cx, cy) = c.p
    let minX = min minX cx
    let minY = min minY cy
    let maxX = max maxX cx
    let maxY = max maxY cy
    for y = minY to maxY do
        for x = minX to maxX do
            let infected = Set.contains (x, y) m
            let padL, padR = if (x, y) = c.p then ('[',']') else (' ',' ')
            printf "%c%c%c" padL (if infected then '#' else '.') padR
        printfn ""

let getStart (m: Set<int*int>) =
    let minX, maxX, minY, maxY = getMapSize m
    let width = maxX - minX
    let height = maxY - minY
    {
        p = 
            if width = 2 && height = 1 then
                // Hack for initial input, because our load function considers the width and
                // height to be determined by the outermost #, and not the actual size...
                (1,1)
            else
                (minX + width / 2, minY + height / 2)
        d = Up
    }
    
let moves (m: Set<int * int>) =
    Seq.unfold
        (fun (m: Set<int*int>, c: Carrier, _: bool) ->
            let next = move (m, c)
            Some (next, next))
        (m, getStart m, false)

let countInfectionsAfter (moves: (Set<int*int> * Carrier * bool) seq) (after: int) =
    moves
    |> Seq.take after
    |> Seq.filter (fun (_, _, didInfect) -> didInfect)
    |> Seq.length

[<EntryPoint>]
let main argv =
    let testMap = splitIntoRows testInput |> loadMap

    printMap (testMap, getStart testMap, false)

    let testMoves = moves testMap

    for move in (testMoves |> Seq.take 7) do
        printfn ""
        printMap move

    let after70 = testMoves |> Seq.skip 69 |> Seq.take 1 |> Seq.exactlyOne
    printfn ""
    printfn ""
    printMap after70

    //let after10000 = testMoves |> Seq.skip 10000 |> Seq.take 1 |> Seq.exactlyOne
    //printfn ""
    //printfn ""
    //printMap after10000

    countInfectionsAfter testMoves 70 =! 41
    countInfectionsAfter testMoves 10000 =! 5587


    let realMap = getEmbeddedRows () |> loadMap
    let moves = moves realMap
    let infectionsAfter10000 = countInfectionsAfter moves 10000

    //let after10000 = moves |> Seq.skip 10000 |> Seq.take 1 |> Seq.exactlyOne
    //printfn ""
    //printfn ""
    //printMap after10000

    printfn "Part 1: %d" infectionsAfter10000
    0

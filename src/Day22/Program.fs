// Learn more about F# at http://fsharp.org

open System

open Swensen.Unquote

open Geometry
open TextHandling

type NodeState = Clean | Weakened | Infected | Flagged

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
        (fun (m: Map<int*int, NodeState>) (x:int, y: int, c: char) ->
            let state = 
                if c = '#' then Infected
                else Clean
            Map.add (x, y) state m)
        Map.empty

let getValue (p:int*int) (m: Map<int*int, NodeState>) =
    match Map.tryFind p m with
    | Some state -> state
    | None -> Clean

let move turnPolicy statePolicy (m: Map<int*int, NodeState>, c: Carrier) =
    let state = (getValue c.p m)
    let newDirection = turnPolicy state c.d
    let newState = statePolicy state

    let dx, dy =
        match newDirection with
        | Up -> (0, -1)
        | Down -> (0, 1)
        | Left -> (-1, 0)
        | Right -> (1, 0)
    let newMap = Map.add c.p newState m
    let cx, cy = c.p
    let newCarrier =
        {
            p = (cx + dx, cy + dy)
            d = newDirection
        }
    let didInfect = newState = Infected
    (newMap, newCarrier, didInfect)


let move1 =
    let turnPolicy (state: NodeState) (d: Direction) =
        let infected = state = Infected
        match d with
        | Up -> if infected then Right else Left
        | Down -> if infected then Left else Right
        | Left -> if infected then Up else Down
        | Right -> if infected then Down else Up
    let statePolicy (state: NodeState) = if state = Clean then Infected else Clean
    move turnPolicy statePolicy


let move2 =
    let turnPolicy (state: NodeState) (d: Direction) =
        let turns =
            match state with
            | Clean -> [false]           // Left
            | Weakened -> []            // Continue
            | Infected -> [true]       // Right
            | Flagged -> [true; true]   // Reverse
        turns
        |> Seq.fold
            (fun (d: Direction) (left: bool) ->
                match d with
                | Up -> if left then Right else Left
                | Down -> if left then Left else Right
                | Left -> if left then Up else Down
                | Right -> if left then Down else Up)
            d
    let statePolicy (state: NodeState) =
        match state with
        | Clean -> Weakened
        | Weakened -> Infected
        | Infected -> Flagged
        | Flagged -> Clean
    move turnPolicy statePolicy
   
let testInput = """..#
#..
..."""

let getMapSize (m: Map<int*int, NodeState>) =
    m
    |> Seq.fold
        (fun (minX, maxX, minY, maxY) kv ->
            let x, y = kv.Key
            (min x minX, max x maxX, min y minY, max y maxY))
        (Int32.MaxValue, Int32.MinValue, Int32.MaxValue, Int32.MinValue)

let printMap (m: Map<int*int, NodeState>, c: Carrier, _: bool) =
    let minX, maxX, minY, maxY = getMapSize m
    let (cx, cy) = c.p
    let minX = min minX cx
    let minY = min minY cy
    let maxX = max maxX cx
    let maxY = max maxY cy
    for y = minY to maxY do
        for x = minX to maxX do
            let item =
                match getValue (x, y) m with
                | Infected -> '#'
                | Clean -> '.'
                | Weakened -> 'W'
                | Flagged -> 'F'
            let padL, padR = if (x, y) = c.p then ('[',']') else (' ',' ')
            printf "%c%c%c" padL item padR
        printfn ""

let getStart (m: Map<int*int, NodeState>) =
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
    
let moves movePolicy (m: Map<int*int, NodeState>) =
    Seq.unfold
        (fun (m: Map<int*int, NodeState>, c: Carrier, _: bool) ->
            let next = movePolicy (m, c)
            Some (next, next))
        (m, getStart m, false)

let moves1 = moves move1
let moves2 = moves move2


let countInfectionsAfter (moves: (Map<int*int, NodeState> * Carrier * bool) seq) (after: int) =
    moves
    |> Seq.take after
    |> Seq.filter (fun (_, _, didInfect) -> didInfect)
    |> Seq.length

[<EntryPoint>]
let main argv =
    let testMap = splitIntoRows testInput |> loadMap

    printMap (testMap, getStart testMap, false)

    let testMoves = moves1 testMap

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
    let moves = moves1 realMap
    let infectionsAfter10000 = countInfectionsAfter moves 10000

    //let after10000 = moves |> Seq.skip 10000 |> Seq.take 1 |> Seq.exactlyOne
    //printfn ""
    //printfn ""
    //printMap after10000

    printfn "Part 1: %d" infectionsAfter10000

    printfn ""
    printfn ""
    printfn "--------------"
    printfn ""
    printfn ""

    let testMoves2 = moves2 testMap

    for move in (testMoves2 |> Seq.take 7) do
        printfn ""
        printMap move


    // These 10,000,000 iteration cases for part 2 are quite slow - over half
    // a minute for the test, and then a few minutes for the real answer - but
    // not quite slow enough to necessitate a more clever solution.
    countInfectionsAfter testMoves2 100 =! 26
    countInfectionsAfter testMoves2 10000000 =! 2511944

    let part2moves = moves2 realMap
    printfn "Part 2: %d" (countInfectionsAfter part2moves 10000000)
    0

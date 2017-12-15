open System
open Swensen.Unquote

open KnotHashing

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

let gridAsCoordinateSet (grid: (bool seq) seq) =
    grid
    |> Seq.mapi
        (fun j row -> row |> Seq.mapi (fun i v -> if v then Some (i, j) else None))
    |> Seq.collect id
    |> Seq.choose id
    |> Seq.fold (fun (s: Set<int*int>) (c: int*int) -> Set.add c s) Set.empty

let getPointsInSameRegion (grid: Set<int*int>) (p: int*int) =
    Seq.unfold
        (fun (regionSoFar: Set<int*int>, pointsStillToProcess: Set<int*int>) ->
            if Set.isEmpty pointsStillToProcess then None
            else
                let currentPoint = pointsStillToProcess |> Seq.take 1 |> Seq.exactlyOne
                let (x, y) = currentPoint
                let adjacentPoints =
                    [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
                let pointIsUsed ((x, y): int*int) =
                    if x < 0 || y < 0 || x >= 128 || y >= 128 then false
                    else Set.contains (x, y) grid
                let usedAdjacentPoints = adjacentPoints |> Seq.filter pointIsUsed |> Set.ofSeq
                let newlyDiscoveredPointsInRegion =
                    Set.difference usedAdjacentPoints regionSoFar
                let updatedRegion = Set.union regionSoFar newlyDiscoveredPointsInRegion
                let updatedPointsToProcess =
                    pointsStillToProcess
                    |> Set.remove currentPoint
                    |> Set.union newlyDiscoveredPointsInRegion
                Some (updatedRegion, (updatedRegion, updatedPointsToProcess)))
        (Set.singleton p, Set.singleton p)
    |> Seq.last

let getRegions (grid: Set<int*int>) =
    let pointIsNotUsed p = Set.contains p grid |> not
    seq { for j in [0..127] do for i in [0..127] -> (i, j) }
    |> Seq.fold
        (fun (pointToRegion: Map<int*int,int>, nextRegionNumber: int) p ->
            let pointIsAlreadyInRegion = Map.containsKey p pointToRegion
            if pointIsAlreadyInRegion || pointIsNotUsed p
            then (pointToRegion, nextRegionNumber)
            else
                let newRegion = getPointsInSameRegion grid p
                let updatedPointToRegion =
                    newRegion
                    |> Seq.fold
                        (fun (m: Map<int*int, int>) p -> Map.add p nextRegionNumber m)
                        pointToRegion
                (updatedPointToRegion, nextRegionNumber + 1))
        (Map.empty, 1)
    |> fst

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

    let testGridAsSet = gridAsCoordinateSet testRows
    let firstRegion = getPointsInSameRegion testGridAsSet (0,0)
    printfn "%A" firstRegion
    let testRegions = getRegions testGridAsSet
    for j in [0..7] do
        for i in [0..7] do
            let columnText:string option = Map.tryFind (i,j) testRegions |> Option.map string
            printf "%s" ((defaultArg columnText ".").PadLeft(3))
            printf ""
        printfn ""
    
    let input = "jxqlasbh"
    let rows = makeRowsForInput input

    printfn "Part 1: %d" (countUsed rows)

    let gridAsSet = gridAsCoordinateSet rows
    let regions = getRegions gridAsSet
    let regionCount =
        regions
        |> Seq.distinctBy (fun x -> x.Value)
        |> Seq.length
    printfn "Part 2: %d" regionCount
    0

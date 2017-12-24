open System

open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

type Components =
    {
        items: (int * int)[]
        byType: Map<int, int list>
    }

let pComponent<'a> : Parser<int*int, 'a> =
    pipe2
        (pint32 .>> spaces .>> pchar '/')
        (spaces >>. pint32)
        (fun x y -> (x,y))

let loadComponents (xs: string seq) =
    let addComponentOfType (t: int) (index: int) (m:Map<int, int list>) =
        let values =
            match Map.tryFind t m with
            | None -> [index]
            | Some existingIndexes -> index::existingIndexes
        Map.add t values m
    let _, items, byType =
        xs
        |> Seq.map (testp pComponent)
        |> Seq.fold
            (fun (i: int, items: (int*int) list, m:Map<int, int list>) (c: int*int) ->
                let t0, t1 = c
                (i + 1, c::items, m |> (addComponentOfType t0 i >> addComponentOfType t1 i)))
            (0, [], Map.empty)
    {
        items = items |> Array.ofList |> Array.rev
        byType = byType
    }

let testInput = """0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10"""

type BridgeElement =
    {
        itemIndex: int
        flipped: bool
    }

let rec findChains (c: Components) (bridgeSoFar: BridgeElement list) =
    let typeRequired =
        match bridgeSoFar with
        | [] -> 0
        | latestComponent::_ ->
            let (t1, t2) = c.items.[latestComponent.itemIndex]
            if latestComponent.flipped then t2 else t1
    match Map.tryFind typeRequired c.byType with
    | None -> Seq.singleton bridgeSoFar
    | Some indexes ->
        Seq.singleton bridgeSoFar
        |> Seq.append
            (indexes
             |> Seq.filter (fun i ->
                let alreadyInBridge = bridgeSoFar |> Seq.exists (fun e -> e.itemIndex = i)
                not alreadyInBridge)
             |> Seq.collect
                (fun i ->
                    let e0 = fst c.items.[i]
                    let flipped = e0 = typeRequired
                    let nextElement = { itemIndex = i; flipped = flipped }
                    findChains c (nextElement::bridgeSoFar)))

let bridgeStrength (c:Components) (bridge: BridgeElement list) =
    bridge
    |> List.fold
        (fun (s: int) (e: BridgeElement) ->
            let t0, t1 = c.items.[e.itemIndex]
            t0 + t1 + s)
        0

let findStrongestBridge (c:Components) (bridges: (BridgeElement list) seq) =
    bridges |> Seq.maxBy (bridgeStrength c)

let printBridgeToString (c:Components) (bridge: BridgeElement list) =
    for element in bridge do
        let t0, t1 = c.items.[element.itemIndex]
        printf "%d/%d  " t0 t1
    printfn ""

[<EntryPoint>]
let main argv =
    let testComponents = splitIntoRows testInput |> loadComponents
    let testBridges = findChains testComponents []

    for c in testBridges do
        printBridgeToString testComponents c

    let strongestBridgeTest = findStrongestBridge testComponents testBridges

    printfn "Strongest (test):"
    printBridgeToString testComponents strongestBridgeTest
    bridgeStrength testComponents strongestBridgeTest =! 31

    let components = getEmbeddedRows () |> loadComponents
    let bridges = findChains components []
    let strongestBridge = findStrongestBridge components bridges
    printfn "Part 1: %d" (bridgeStrength components strongestBridge)
    
    0

// Learn more about F# at http://fsharp.org

open System
open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

let pConnections<'a> : Parser<int * (int list), 'a> =
    pipe2
        (pint32 .>> spaces .>> pstring "<->" .>> spaces)
        (sepBy pint32 (spaces >>. pstring "," .>> spaces))
        (fun id list -> (id, list))


let parseInputLine s = testp pConnections s
let parseLines (lines: string seq) : (int * (int list)) seq =
    lines
    |> Seq.map parseInputLine

let testInput = """
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"""

let testLines = testInput.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)

let testConnections = parseLines testLines |> List.ofSeq

let processGroup (groups: Map<int, Set<int>>) ((id, connections):(int * (int list))) =
    let idsExplicitlyInThisGroup = Set.ofList (id :: connections)

    // Pick up all the ids
    let idsForThisGroup =
        connections
        |> Seq.fold
            (fun (ids: Set<int>) connection ->
                match Map.tryFind connection groups with
                | Some idsFromConnection ->
                        Set.union ids idsFromConnection
                | None -> ids)
            idsExplicitlyInThisGroup

    // Now that we've worked out the currently visible transitive closure of
    // membership for this group, we need to go through all the ids we found
    // and ensure that the target now also lists all the same ids.
    // (E.g., if we just found that 3 is connected to 1, 2, and 5, we now need
    // to make sure that 1, 2, and 5 all say they're connected to 1, 2, 3, and 5.
    let groups =
        idsForThisGroup
        |> Seq.fold
            (fun (groups: Map<int, Set<int>>) connection ->
                let idsAlreadyInTarget =
                    match Map.tryFind connection groups with
                    | Some existingIds -> existingIds
                    | None -> Set.empty
                let updatedIdsForTarget = Set.union idsAlreadyInTarget idsForThisGroup
                Map.add connection updatedIdsForTarget groups)
            groups

    Map.add id idsForThisGroup groups

let findGroups (input: (int * (int list)) seq) =
    input
    |> Seq.fold processGroup Map.empty


let inputRows = getEmbeddedRows ()
let inputConnections = parseLines inputRows

[<EntryPoint>]
let main argv =

    parseInputLine "0 <-> 2" =! (0, [2])
    parseInputLine "1 <-> 1" =! (1, [1])
    parseInputLine "2 <-> 0, 3, 4" =! (2, [0; 3; 4])

    let testGroups = findGroups testConnections
    let group0 = testGroups.[0]

    Set.contains 0 group0 =! true
    Set.contains 2 group0 =! true
    Set.contains 3 group0 =! true
    Set.contains 4 group0 =! true
    Set.contains 5 group0 =! true
    Set.contains 6 group0 =! true
    Set.count group0 =! 6

    let groupsFromInput = findGroups inputConnections
    let group0 = groupsFromInput.[0]

    printfn "Part 1: %d" (Set.count group0)
    0

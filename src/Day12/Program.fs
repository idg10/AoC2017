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

let processGroup
    (groups: Map<int, Set<int>>, groupMembership: Map<int, int>)
    ((id, connections): (int * (int list))) =

    // Go through the elements that this element is connected to, and work out which
    // groups its connectees belong to, creating new groups for them if necessary
    let (groups, currentlyKnownGroupsJoinedByThis, groupMembership) =
        connections
        |> Seq.fold
            (fun (groups: Map<int, Set<int>>, groupIds: Set<int>, groupMembership: Map<int, int>) connectedId ->
                match Map.tryFind connectedId groupMembership with
                | Some groupId ->
                    // We've already seen this connectee, so just add the current
                    // element's id to that conectee's group, and add the group
                    // to the set of groups we're saying this item is connected to.
                    let groupMembers =
                        groups.[groupId]
                        |> Set.add id
                    (
                        Map.add groupId groupMembers groups,
                        Set.add groupId groupIds,
                        groupMembership |> Map.add id groupId
                    )
                | None ->
                    // We've never seen this connectee before, so add it to the
                    // group for this element, creating that group if it doesn't
                    // already exist.
                    let groupIdForThisItem =
                        match Map.tryFind id groupMembership with
                        | Some existingGroupId -> existingGroupId
                        | None -> id
                    let groups =
                        let items =
                            match Map.tryFind groupIdForThisItem groups with
                            | Some g -> Set.add connectedId g
                            | None -> Set.ofList [groupIdForThisItem; connectedId]
                        groups |> Map.add groupIdForThisItem items
                    (
                        groups,
                        Set.add groupIdForThisItem groupIds,
                        groupMembership
                        |> Map.add id groupIdForThisItem
                        |> Map.add connectedId groupIdForThisItem
                    ))
            (groups, Set.empty, groupMembership)


    // How many groups did we connect with? If it's just one, then we're already done -
    // the previous step will have updated the maps correctly.
    // However, if it turns out that this element's connections belong to multiple
    // different groups, we now need to merge those into a single group.
    let (groups, groupMembership) =
        match List.ofSeq currentlyKnownGroupsJoinedByThis with
        | [] -> failwith "Expecting to find or create at least one group"
        | [groupId] -> (groups, groupMembership)
        | groupIds ->
            // This element connects to multiple existing groups, we
            // need to merge them into one new group.
            match List.sort groupIds with
            | idToKeep::idsToMerge ->
                idsToMerge
                |> Seq.fold
                    (fun (groups: Map<int, Set<int>>, groupMembership: Map<int, int>) (id: int) ->
                        let targetGroup = groups.[idToKeep]
                        let groupToMerge = groups.[id]
                        let mergedGroup = Set.union targetGroup groupToMerge
                        (
                            groups
                            |> Map.remove id
                            |> Map.add idToKeep mergedGroup,
                            groupToMerge
                            |> Seq.fold
                                (fun (groupMembership: Map<int, int>) idToMerge -> groupMembership |> Map.add idToMerge idToKeep)
                                groupMembership
                        ))
                    (groups, groupMembership)
            | _ -> failwith "Expecting list with at least two entries"

    (groups, groupMembership)

let findGroups (input: (int * (int list)) seq) =
    input
    |> Seq.fold processGroup (Map.empty, Map.empty)


let inputRows = getEmbeddedRows ()
let inputConnections = parseLines inputRows

[<EntryPoint>]
let main argv =

    parseInputLine "0 <-> 2" =! (0, [2])
    parseInputLine "1 <-> 1" =! (1, [1])
    parseInputLine "2 <-> 0, 3, 4" =! (2, [0; 3; 4])

    let (testGroups, groupMembership) = findGroups testConnections
    let group0 = testGroups.[0]

    Set.contains 0 group0 =! true
    Set.contains 2 group0 =! true
    Set.contains 3 group0 =! true
    Set.contains 4 group0 =! true
    Set.contains 5 group0 =! true
    Set.contains 6 group0 =! true
    Set.count group0 =! 6

    let (groupsFromInput, membershipFromInput) = findGroups inputConnections
    let group0 = groupsFromInput.[0]

    printfn "Part 1: %d" (Set.count group0)

    printfn "Part 2: %d" (Map.count groupsFromInput)
    0

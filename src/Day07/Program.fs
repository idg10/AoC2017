// Learn more about F# at http://fsharp.org

open System

open Swensen.Unquote
open FParsec

open TextHandling

let pWeight<'a> : Parser<int32, 'a> = pstring "(" >>. pint32 .>> pstring ")"
let pProgramName<'a> : Parser<string, 'a> = many1Satisfy isLetter
let pProgramList<'a> : Parser<string list, 'a> = sepBy (pProgramName) (pstring "," .>> spaces)

type Program =
    {
        name: string
        weight: int
        onTop: string list
    }

let pInputLine<'a> : Parser<Program, 'a> =
    pipe3
        (pProgramName  .>> spaces) (pWeight .>> spaces) (opt (pstring "->" .>> spaces >>. pProgramList))
        (fun n w ot ->
            {
                name = n;
                weight = w;
                onTop =
                    match ot with
                    | Some ps -> ps
                    | None -> []
            })

let testp (p : Parser<'TResult, unit>) s =
    match run p s with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwithf "Parse failed: %s" error


// Maintain a set of candidate roots
// , childToParentMap : Map<string, string>)
// , childToParentMap
// , Map.empty
let findRoots (input : Program seq) =
    let (programsByName, unparented, _) =
        input
        |> Seq.fold
            (fun (allPrograms : Map<string, Program>, unparented : Set<string>, knownToHaveParents : Set<string>) (p : Program) ->
                // For each of this program's children ('onTop') we need to do two things:
                //  If we didn't previously know the parent for that program, we should now remove it from the unparented list
                //  We should add an entry to our set of elements known to have parents
                let (unparented, knownToHaveParents) =
                    p.onTop
                    |> Seq.fold
                        (fun (unparented : Set<string>, knownToHaveParents : Set<string>) child ->
                            let unparented =
                                if Set.contains child unparented then
                                    Set.remove child unparented
                                else
                                    unparented
                            (unparented, Set.add child knownToHaveParents))
                        (unparented, knownToHaveParents)
                let unparented =
                    if Seq.contains p.name knownToHaveParents then unparented
                    else Set.add p.name unparented
                (Map.add p.name p allPrograms, unparented, knownToHaveParents))
            (Map.empty, Set.empty, Set.empty)
    (programsByName, List.ofSeq unparented)

let testInput = """
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"""

let testPrograms =
    splitIntoRows testInput
    |> Seq.map (testp pInputLine)

[<EntryPoint>]
let main argv =
    // Parser tests    
    testp pWeight "(123)" =! 123
    let p = testp pInputLine "fwft (72) -> ktlj, cntj, xhth"
    p.name =! "fwft"
    p.weight =! 72
    p.onTop =! ["ktlj"; "cntj"; "xhth"]

    let p2 = testp pInputLine "pbga (66)"
    p2.name =! "pbga"
    p2.weight =! 66
    p2.onTop =! []

    let (_, unparented) = findRoots testPrograms
    unparented =! ["tknk"]

    let input =
        getEmbeddedRows ()
        |> Seq.map (testp pInputLine)
    let (_, unparented) = findRoots input
    let root = Seq.exactlyOne unparented
    printfn "Part 1: %s" root
    0

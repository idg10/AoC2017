open Swensen.Unquote

open TextHandling

let execute (offsets : int seq) =
    let offsetMap = offsets |> Seq.mapi (fun i v -> (i, v)) |> Map.ofSeq
    Seq.unfold
        (fun (pos, offsets : Map<int, int>) ->
            if pos >= offsets.Count then None
            else
                let offset = offsets.[pos]
                let nextPos = pos + offset
                let result = (nextPos, Map.add pos (offset + 1) offsets)
                in Some (result, result))
        (0, offsetMap)

let testInput = """
0
3
0
1
-3"""

let parseInput (rows : string seq) = Seq.map int rows

let testOffsets = splitIntoRows testInput |> parseInput

let stepCount offsets = execute offsets |> Seq.length

let input = getEmbeddedRows () |> parseInput

[<EntryPoint>]
let main argv =
    stepCount testOffsets =! 5


    printfn "Part 1: %d" (stepCount input)
    0 // return an integer exit code

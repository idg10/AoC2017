open Swensen.Unquote

open TextHandling

let execute (jumpRule : int -> int) (offsets : int seq) =
    let offsetMap = offsets |> Seq.mapi (fun i v -> (i, v)) |> Map.ofSeq
    Seq.unfold
        (fun (pos, offsets : Map<int, int>) ->
            if pos < 0 ||  pos >= offsets.Count then None
            else
                let offset = offsets.[pos]
                let nextPos = pos + offset
                let change = jumpRule offset
                let result = (nextPos, Map.add pos (offset + change) offsets)
                in Some (result, result))
        (0, offsetMap)

let execute1 : int seq -> (int * Map<int,int>) seq = execute (fun _ -> 1)
let execute2 : int seq -> (int * Map<int,int>) seq = execute (fun offset -> if offset >=3 then -1 else 1)


// The code above works but it's pretty slow - takes several minutes to get the answer to part 2.
// Below is a modified version that builds an array holding the offsets, and modifies the offsets
// in place in the array.
// This mutable version runs in around 1 second, making it a couple of orders magnitude faster than
// the version above. (It also produces a sequence of just positions; the version above produces a sequence
// showing not just the current position, but also the full list of current jump offsets, making it much
// easier to observe what it's doing.)

let executem (jumpRule : int -> int) (offsets : int seq) =
    let offsetArray = offsets |> Array.ofSeq
    Seq.unfold
        (fun pos ->
            if pos < 0 ||  pos >= offsetArray.Length then None
            else
                let offset = offsetArray.[pos]
                let nextPos = pos + offset
                let change = jumpRule offset
                offsetArray.[pos] <- (offset + change)
                Some (nextPos, nextPos))
        0

let execute1m : int seq -> int seq = executem (fun _ -> 1)
let execute2m : int seq -> int seq = executem (fun offset -> if offset >=3 then -1 else 1)

let testInput = """
0
3
0
1
-3"""

let parseInput (rows : string seq) = Seq.map int rows

let testOffsets = splitIntoRows testInput |> parseInput

// Choose here between pure or mutable implementations

//let stepCount1 offsets = execute1 offsets |> Seq.length
//let stepCount2 offsets = execute2 offsets |> Seq.length

let stepCount1 offsets = execute1m offsets |> Seq.length
let stepCount2 offsets = execute2m offsets |> Seq.length

let input = getEmbeddedRows () |> parseInput

[<EntryPoint>]
let main argv =
    stepCount1 testOffsets =! 5
    printfn "Part 1: %d" (stepCount1 input)

    stepCount2 testOffsets =! 10
    printfn "Part 1: %d" (stepCount2 input)
    0

open System

open Swensen.Unquote


// Given the current buffer size and last insertion point, this works out
// the next buffer size (1 larger) and where the next item would be inserted.
// We don't actually maintain the buffer itself in here because Part 2 doesn't
// need it, and keeping track of the actual buffer is a lot of work.
let iterate (stepSize: int) (bufferLength: int, index: int) =
    let indexToInsertAfter = (index + stepSize) % bufferLength
    (bufferLength + 1, indexToInsertAfter + 1)


// Produces an infinite sequence of insertion indexes for a given step count.
let spin stepSize =
    Seq.unfold
        (fun (bufferLength: int, index: int) ->
            let next = iterate stepSize (bufferLength, index)
            Some (snd next, next))
        (1, 0)

// In Part 1, we do actually need to see what's in the buffer in order to
// perform some of the tests that compare our list with what the instructions
// say should be in the list. So this reconstructs the list from a sequence
// of insertion indexes.
let buildList (insertionIndexes: int seq) =
    insertionIndexes
    |> Seq.mapi (fun i v -> (i + 1, v))
    |> Seq.fold
        (fun (list: int list) (v: int, index: int) ->
            let before, after =
                match list with
                | [] -> ([], [])
                | _ -> List.splitAt index list
            List.append
                before
                (v::after))
        [0]


[<EntryPoint>]
let main argv =
    let testPositions = spin 3 |> Seq.take 2017

    let first9InsertionPositions =  testPositions |> Seq.take 9 |> List.ofSeq

    first9InsertionPositions =! [1;1;2;2;1;5;2;6;1]
    let first9 = buildList first9InsertionPositions
    first9 =! [0;9;5;7;2;4;3;8;6;1]

    let fullList = buildList testPositions
    let indexOf2017 = List.findIndex ((=) 2017) fullList
    let testSegment =
        let start = indexOf2017 - 3
        fullList
        |> Seq.skip start
        |> Seq.take 7
        |> List.ofSeq

    testSegment =! [1512; 1134; 151; 2017; 638; 1513; 851]

    let listAfter2017Insertions = spin 344 |> Seq.take 2017 |> buildList
    let indexOf2017 = List.findIndex ((=) 2017) listAfter2017Insertions
    let valueAfter2017 = listAfter2017Insertions.[indexOf2017 + 1]

    printfn "Part 1: %d" valueAfter2017


    // In Part 2 we need to find the item inserted after 0. By inspection, 0 is
    // always the first item in the list, so we just need to run through all
    // the insertion indexes produced by spin and keep track of what's in the
    // second position in the list, and that'll be the result.
    let (p0, p1) =
        spin 344
        |> Seq.take 50000000
        |> Seq.mapi (fun value insertPosition -> (value + 1, insertPosition))
        |> Seq.fold
            (fun (p0: int, p1: int) (value: int, insertPosition: int) ->
                match insertPosition with
                | 0 -> (value, p1)
                | 1 -> (p0, value)
                | _ -> (p0, p1))
            (0, -1)
    
    // Verify that that the 0 really does remain in position 0 throughout
    p0 =! 0
    printfn "Part 2: %d, %d" p0 p1
    0

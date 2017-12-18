open System

open Swensen.Unquote


let iterate (stepSize: int) (buffer: int list, index: int) =
    let bufferLength = buffer.Length
    let indexToInsertAfter = (index + stepSize) % bufferLength
    let newBuffer =
        List.append
            (List.take (indexToInsertAfter + 1) buffer)
            (bufferLength :: (List.skip (indexToInsertAfter + 1) buffer))
    (newBuffer, indexToInsertAfter + 1)

let spin stepSize =
    Seq.unfold
        (fun (buffer: int list, index: int) ->
            let next = iterate stepSize (buffer, index)
            Some (next, next))
        ([0], 0)

[<EntryPoint>]
let main argv =
    let testPostions = spin 3 |> Seq.take 9 |> List.ofSeq

    testPostions.[0] =! ([0;1], 1)
    testPostions.[1] =! ([0;2;1], 1)
    testPostions.[2] =! ([0;2;3;1], 2)
    testPostions.[3] =! ([0;2;4;3;1], 2)
    testPostions.[4] =! ([0;5;2;4;3;1], 1)
    testPostions.[5] =! ([0;5;2;4;3;6;1], 5)
    testPostions.[6] =! ([0;5;7;2;4;3;6;1], 2)
    testPostions.[7] =! ([0;5;7;2;4;3;8;6;1], 6)
    testPostions.[8] =! ([0;9;5;7;2;4;3;8;6;1], 1)


    let (after2017Buffer, after2017Index) = spin 344 |> Seq.take 2017 |> Seq.last

    printfn "Part 1: %d" after2017Buffer.[after2017Index + 1]
    0

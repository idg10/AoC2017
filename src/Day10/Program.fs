open Swensen.Unquote

open KnotHashing
open TextHandling

let testInput = [3;4;1;5]
let testInitialPosition = makeInitialPosition 5

let input = getInputAsIntList () 

    
[<EntryPoint>]
let main argv =
    let testSteps =
        List.scan processMove testInitialPosition testInput
        
    testSteps.[1] =!
        {
            currentPosition = 3
            skipSize = 1
            elements = [2; 1; 0; 3; 4]
            elementCount = 5
        }
    testSteps.[2] =!
        {
            currentPosition = 3
            skipSize = 2
            elements = [4; 3; 0; 1; 2]
            elementCount = 5
        }
    testSteps.[3] =!
        {
            currentPosition = 1
            skipSize = 3
            elements = [4; 3; 0; 1; 2]
            elementCount = 5
        }
    testSteps.[4] =!
        {
            currentPosition = 4
            skipSize = 4
            elements = [3; 4; 2; 1; 0]
            elementCount = 5
        }

    let testFinalElements = (List.last testSteps).elements
    testFinalElements.[0] * testFinalElements.[1] =! 12

    let inputFinalStep = Seq.fold processMove fullInitialPosition input
    let finalElements = inputFinalStep.elements

    printfn "Part 1: %d" (finalElements.[0] * finalElements.[1])

    convertInputToPart2List "1,2,3" =! [49;44;50;44;51;17;31;73;47;23]

    let testSparseHash = [65; 27; 9; 1; 4; 3; 40; 50; 91; 7; 6; 0; 2; 5; 68; 22]
    makeDenseHash 64 testSparseHash =! [64]

    hashString "" =! "a2582a3a0e66e6e86e3812dcb672a272"
    hashString "AoC 2017" =! "33efeb34ea91902bb2f59c9920caa6cd"
    hashString "1,2,3" =! "3efbe78a8d82f29979031a4aa0b16a9d"
    hashString "1,2,4" =! "63960835bcdc130f0b66d7ff4f6a5a8e"

    let inputText = getEmbeddedInput ()
    let hashedInput = hashString inputText
    printfn "Part 2: %s" hashedInput
    0

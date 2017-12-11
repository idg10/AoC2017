// Learn more about F# at http://fsharp.org

open Swensen.Unquote

open ParsingHelpers
open TextHandling


type Step =
    {
        currentPosition: int
        skipSize: int
        elements: int list
        elementCount: int   // Could calculate from elements, but that has O(N) for a list
    }
let makeInitialPosition elementCount =
    {
        currentPosition = 0
        skipSize = 0
        elements = [0..elementCount]
        elementCount = elementCount
    }


let processMove previousStep length =
    let elementsWithWrap = Seq.concat [previousStep.elements; previousStep.elements]

    let wrappedElementsInTarget =
        let excess = (previousStep.currentPosition + length) - previousStep.elementCount
        if excess < 0 then 0 else excess
    let reversedTargetSection =
        elementsWithWrap
        |> Seq.skip previousStep.currentPosition
        |> Seq.take length
        |> Seq.rev

    let preWrapReversedTargetSection =
        reversedTargetSection
        |> Seq.take (length - wrappedElementsInTarget)

    let postWrapReversedTargetSection =
        reversedTargetSection
        |> Seq.skip (length - wrappedElementsInTarget)

    let initialSection =
        Seq.append
            postWrapReversedTargetSection
            (Seq.take
                (previousStep.currentPosition - wrappedElementsInTarget)
                (Seq.skip wrappedElementsInTarget elementsWithWrap))

    let sectionAfterTarget =
        if wrappedElementsInTarget > 0 then Seq.empty
        else
            elementsWithWrap
            |> Seq.skip (previousStep.currentPosition + length)
            |> Seq.take (previousStep.elementCount - (previousStep.currentPosition + length))
    {
        currentPosition = (previousStep.currentPosition + length + previousStep.skipSize) % previousStep.elementCount
        skipSize = previousStep.skipSize + 1
        elements =
            List.collect List.ofSeq [initialSection; preWrapReversedTargetSection; sectionAfterTarget]
        elementCount = previousStep.elementCount
    }

let testInput = [3;4;1;5]
let testInitialPosition = makeInitialPosition 5

let fullInitialPosition = makeInitialPosition 256
let input = getInputAsIntList () 


let treatInputAsAscii (input : string) =
    input
    |> Seq.map int
    |> List.ofSeq
let inputAsAscii = getEmbeddedInput () |> treatInputAsAscii

let convertInputToPart2List input =
    List.append
        (treatInputAsAscii input)
        [17; 31; 73; 47; 23]

let getSparseHash count inputText =
    let lengths = convertInputToPart2List inputText
    seq { 1..count }
    |> Seq.fold
        (fun (s: Step) _ -> Seq.fold processMove s lengths)
        fullInitialPosition
    |> (fun x -> x.elements)

let makeDenseHash (blockSize: int) (sparseHash: int list) =
    sparseHash
    |> Seq.chunkBySize blockSize
    |> Seq.map (Seq.fold (^^^) 0)
    |> List.ofSeq
    //|> Seq.map (fun block -> block |> Seq.fold (^^^) 0 )

let denseHashToString (denseHash: int list) =
    String.concat
        ""
        (denseHash |> Seq.map (fun v -> v.ToString("x2")))

let hashString s =
    let sparseHash = getSparseHash 64 s
    let denseHash = makeDenseHash 16 sparseHash
    denseHashToString denseHash
    
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
    0 // return an integer exit code

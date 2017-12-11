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
    printfn "Processing %d" length
    let elementsWithWrap = Seq.concat [previousStep.elements; previousStep.elements]

    let wrappedElementsInTarget =
        let excess = (previousStep.currentPosition + length) - previousStep.elementCount
        if excess < 0 then 0 else excess
    let reversedTargetSection =
        elementsWithWrap
        |> Seq.skip previousStep.currentPosition
        |> Seq.take length
        |> Seq.rev
    
    printfn "%A" reversedTargetSection

    let preWrapReversedTargetSection =
        reversedTargetSection
        |> Seq.take (length - wrappedElementsInTarget)

    printfn "%A" preWrapReversedTargetSection

    let postWrapReversedTargetSection =
        reversedTargetSection
        |> Seq.skip (length - wrappedElementsInTarget)
    printfn "%A" postWrapReversedTargetSection

    let initialSection =
        Seq.append
            postWrapReversedTargetSection
            (Seq.take
                (previousStep.currentPosition - wrappedElementsInTarget)
                (Seq.skip wrappedElementsInTarget elementsWithWrap))
    printfn "%A" initialSection

    let sectionAfterTarget =
        if wrappedElementsInTarget > 0 then Seq.empty
        else
            elementsWithWrap
            |> Seq.skip (previousStep.currentPosition + length)
            |> Seq.take (previousStep.elementCount - (previousStep.currentPosition + length))
    printfn "%A" sectionAfterTarget
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
    0 // return an integer exit code

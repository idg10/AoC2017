module KnotHashing

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

let treatInputAsAscii (input : string) =
    input
    |> Seq.map int
    |> List.ofSeq

let convertInputToPart2List input =
    List.append
        (treatInputAsAscii input)
        [17; 31; 73; 47; 23]


let fullInitialPosition = makeInitialPosition 256

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

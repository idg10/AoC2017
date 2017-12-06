// Learn more about F# at http://fsharp.org

open Swensen.Unquote

let getFullestBlockIndexAndValueAndCountBlocks (banks : int list) =
    banks
    |> Seq.mapi (fun i v -> (i, v))
    |> Seq.fold
        (fun (indexOfHighestValue, highestValue, bankCount) (i, value) ->
            if value > highestValue then (i, value, bankCount + 1)
            else (indexOfHighestValue, highestValue, bankCount + 1))
        (-1, -1, 0)

let testInput = [0;2;7;0]

// Rather than iterating through the banks multiple times, we redistribute in one pass.
// If the number blocks divides evenly into the number of banks, we add the same amount
// to each block (or, in the case of the bank being redistributed, we don't add that
// amount, it just becomes the new amount).
// If it doesn't divide equally there will be a remainder, N, and the first N blocks
// after the one being redistributed will get one more than the rest. E.g., if there
// are 6 blocks to redistribute across 4 banks, and the bank being redistributed was
// index 2 (i.e., the 3rd), then the blocks are redistributed thus: [+2;+1;1;+2].

let redistribute (banks : int list) =
    let (indexToRedistribute, amountToRedistribute, bankCount) = getFullestBlockIndexAndValueAndCountBlocks banks
    let amountPerBlock = amountToRedistribute / bankCount
    let remainder = amountToRedistribute % bankCount
    banks
    |> List.mapi
        (fun i v ->
            // In cases with a non-zero remainder, it's the blocks immediately after the one being
            // redistributed that get the extra, so we need to calculate the position relative
            // to the block after the one being redistributed.
            let indexRelativeToBlockAfterRedistributee = (i - 1 + bankCount - indexToRedistribute) % bankCount
            let amountToAdd =
                if indexRelativeToBlockAfterRedistributee < remainder then amountPerBlock + 1 else amountPerBlock
            if i = indexToRedistribute then amountToAdd
            else v + amountToAdd)

let rawRedistributions (banks : int list) =
    Seq.unfold
        (fun currentBanks ->
            let newDistribution = redistribute currentBanks
            Some (currentBanks, newDistribution))
        banks

let distributionsAndLastSeenIndex (banks : int list) =
    rawRedistributions banks
    |> Seq.scan
        (fun (previousI : int, _ : int option, _ : int list, alreadySeen : Map<int list, int>) (distribution : int list) ->
            let i = previousI + 1
            let newDistribution = redistribute distribution
            let previousIndex = Map.tryFind newDistribution alreadySeen
            let alreadySeen =
                match previousIndex with
                | Some _ -> alreadySeen
                | None -> Map.add newDistribution i alreadySeen
            (i, previousIndex, newDistribution, alreadySeen))
        (0, None, banks, Map.add banks 0 Map.empty)

let redistributions (banks : int list) =
    distributionsAndLastSeenIndex banks
    |> Seq.takeWhile (fun (_, previous, _, _) -> previous.IsNone)
    |> Seq.map (fun (_, _, d, _) -> d)

let findLoopLength (banks : int list) =
    let (i, previousIndex, _, _) = distributionsAndLastSeenIndex banks |> Seq.find (fun (_, previous, _, _) -> previous.IsSome)
    i - previousIndex.Value

let input = "14	0	15	12	11	11	3	5	1	6	8	4	9	1	8	4"
let inputList = input.Split(' ', '\t') |> Seq.map int |> List.ofSeq

[<EntryPoint>]
let main argv =
    let testRedistributions = redistributions testInput |> Array.ofSeq
    testRedistributions.Length =! 5
    testRedistributions.[0] =! testInput
    testRedistributions.[1] =! [2;4;1;2]
    testRedistributions.[2] =! [3;1;2;3]
    testRedistributions.[3] =! [0;2;3;4]
    testRedistributions.[4] =! [1;3;4;1]

    printfn "Part 1: %d" (redistributions inputList |> Seq.length)
    
    findLoopLength testInput =! 4
    printfn "Part 2: %d" (findLoopLength inputList)
    0

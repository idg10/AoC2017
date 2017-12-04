open Swensen.Unquote

let getPosition (i : int) =
    if i = 1 then (0, 0) else
    let squareNumber : int = ((sqrt (double i) - 1.0) / 2.0) |> ceil |> int
    let squareEdgeLength = (squareNumber * 2) + 1
    let valueOfPrecedingCorner = (squareEdgeLength - 2) * (squareEdgeLength - 2)
    let diffFromPrecedingCorner = i - valueOfPrecedingCorner
    let numberssPerSide = squareEdgeLength - 1
    let side = (diffFromPrecedingCorner - 1) / numberssPerSide
    let offsetWithinSide = ((diffFromPrecedingCorner - 1) % numberssPerSide) + 1
    let sideStart = side * numberssPerSide + valueOfPrecedingCorner

    let (right, bottom) =
        let distanceFromCentreToCorner = (squareEdgeLength - 1) / 2
        (distanceFromCentreToCorner, -distanceFromCentreToCorner)
    let (left, top) = (right - numberssPerSide, bottom + numberssPerSide)
    let position =
        match side with
        | 0 ->
            // Right edge
            (right, bottom + offsetWithinSide)
        | 1 ->
            // Top edge
            (right - offsetWithinSide, bottom + numberssPerSide)
        | 2 ->
            // Left edge
            (left, top - offsetWithinSide)
        | 3 ->
            // Bottom edge
            (left + offsetWithinSide, bottom)
        | _ -> failwithf "Unexpected side %d" side
    position

let manhattanDistance (x, y) = (abs x) + (abs y)

let distanceForSquare = getPosition >> manhattanDistance

let testSequence =
    Seq.unfold
        (fun (i, values : Map<int*int, int>) ->
            let next = i + 1
            let (nx, ny) = getPosition next
            let getValue p =
                match Map.tryFind p values with
                | Some v -> v
                | None -> 0
            let sum =
                (getValue (nx - 1, ny - 1)) +
                (getValue (nx,     ny - 1)) +
                (getValue (nx + 1, ny - 1)) +
                (getValue (nx - 1, ny)) +
                (getValue (nx + 1, ny)) +
                (getValue (nx- 1, ny + 1)) +
                (getValue (nx, ny + 1)) +
                (getValue (nx + 1, ny + 1))

            Some (values.[getPosition i], (next, Map.add (nx, ny) sum values)))
        (1, (Map.empty |> Map.add (0, 0) 1))

[<EntryPoint>]
let main argv =
    distanceForSquare 1 =! 0
    distanceForSquare 12 =! 3
    distanceForSquare 23 =! 2
    distanceForSquare 1024 =! 31
    printfn "Part 1: %d" (distanceForSquare 347991)

    let initialExpectedTestSequence = [1; 1; 2; 4; 5; 10; 11; 23; 25; 26; 54; 57; 59; 122; 133; 142; 147; 304; 330; 351; 362; 747; 806]
    testSequence |> Seq.take (List.length initialExpectedTestSequence) |> List.ofSeq =! initialExpectedTestSequence

    let firstLarger =
        testSequence
        |> Seq.find (fun v -> v > 347991)
    printfn "Part 2: %d" firstLarger
    0 // return an integer exit code

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

[<EntryPoint>]
let main argv =
    distanceForSquare 1 =! 0
    distanceForSquare 12 =! 3
    distanceForSquare 23 =! 2
    distanceForSquare 1024 =! 31
    printfn "%d" (distanceForSquare 347991)
    0 // return an integer exit code

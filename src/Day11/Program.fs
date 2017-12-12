// Learn more about F# at http://fsharp.org

open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

// So apparently the standard solution to this is to treat the grid as
// though it were an isometric view of a q-bert style pyramid of cubes.
// (Each cube, when projected, becomes a hexagon.)
// Except of course he's gone with one where such a few needs to be
// rotated through 90 degrees, because in q-bert there are no hexagons
// directly on top of one another, but there are ones adjacent. In this
// challenge it's the other way around.
// So, here's how a hexagon looks with its cube faces:
// 
//   +----+
//  /    / \
// +----+   +
//  \    \ /
//   +----+
//
// Imagine that right-facing face is the 'top' of our pyramid (which has
// been rotated 90 degrees clockwise). Now let's have our axes look like this:
//
// X
//  \
//   \
//    +-----> Z
//   /
//  /
// Y
//
// What does a move 'N' (up the page) mean? If we consider a block positioned on the Y axis,
// at the bottom of those axes as shown, the next block above it on the page is one closer to
// Y=0, and one further from X=0, and with no change to Z.
// A move NE is a move from Y towards Z while keeing constant X.
// A move SE is a move from X towards Z while keeping constant Y.

// The distance from the origin is simply max(abs(x), abs(y)
// In this world:
//  N  = (+1,-1, 0)
//  NE = ( 0,-1,+1)
//  SE = (-1, 0,+1)
//  S =  (-1,+1, 0)
//  SW = ( 0,+1,-1)
//  NW = (+1, 0,-1)

type Direction = | N | NE | E | SE | S | SW | W | NW

let processMove (u, v, w) d =
    match d with
    | N ->  (u + 1, v - 1, w)
    | NE->  (u,     v - 1, w + 1)
    | SE -> (u - 1, v,     w + 1)
    | S ->  (u - 1, v + 1, w)
    | SW -> (u,     v + 1, w - 1)
    | NW -> (u + 1, v,     w - 1)

let processMoves (ms: Direction seq) =
    ms
    |> Seq.fold processMove (0,0,0)

let distanceFromPos ((u,v,w): int*int*int) = max (max (abs(u)) (abs(v))) (abs(w))

let distanceHomeAfterMoves (ms: Direction seq) =
    let finalPosition = processMoves ms
    distanceFromPos finalPosition

let pDirection<'a> : Parser<Direction, 'a> = 
        (stringReturn "ne" NE)
    <|> (stringReturn "nw" NW)
    <|> (stringReturn "n" N)
    <|> (stringReturn "sw" SW)
    <|> (stringReturn "se" SE)
    <|> (stringReturn "s" S)
    <|> (stringReturn "e" E)
    <|> (stringReturn "w" W)

let pMoves<'a> : Parser<Direction list, 'a> = sepBy pDirection (spaces >>. pstring "," .>> spaces)

let streamToMoves st =
    match runParserOnStream pMoves () "input" st System.Text.Encoding.UTF8 with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwithf "Parse failed: %s" error


let testString text = testp pMoves text |> distanceHomeAfterMoves

let produceMoves (ms: Direction seq) =
    ms
    |> Seq.scan processMove (0,0,0)



[<EntryPoint>]
let main argv =
    testString "ne,ne,ne" =! 3
    testString "ne,ne,sw,sw" =! 0
    testString "ne,ne,s,s" =! 2
    testString "se,sw,se,sw,sw" =! 3

    let parsedInput =
        getEmbeddedStream ()
        |> streamToMoves

    let result = distanceHomeAfterMoves parsedInput
    printfn "Part 1: %d" result

    let allMoves = produceMoves parsedInput
    let maxDistance =
        allMoves
        |> Seq.maxBy distanceFromPos
    printfn "Part 2: %d" (distanceFromPos maxDistance)
    0 // return an integer exit code

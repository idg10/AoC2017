// Learn more about F# at http://fsharp.org

open System

open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

type Particle =
    {
        p: int*int*int
        v: int*int*int
        a: int*int*int
    }

let parseVector<'a> : Parser<int*int*int, 'a> =
    pipe3
        (pchar '<' >>. spaces >>. pint32 .>> spaces .>> pchar ',')
        (spaces >>. pint32 .>> spaces .>> pchar ',')
        (spaces >>. pint32 .>> spaces .>> pchar '>')
        (fun x y z -> (x, y, z))


let parseParticle<'a> : Parser<Particle, 'a> =
    pipe3
        (pchar 'p' >>. spaces >>. pchar '=' >>. spaces >>. parseVector .>> spaces .>> pchar ',')
        (spaces >>. pchar 'v' >>. spaces >>. pchar '=' >>. spaces >>. parseVector .>> spaces .>> pchar ',')
        (spaces >>. pchar 'a' >>. spaces >>. pchar '=' >>. spaces >>. parseVector .>> spaces)
        (fun p v a -> { p = p; v = v; a = a })

let addVector (x1, y1, z1) (x2, z2, y2) = (x1 + x2, y1 + y2, z1 + z2)

let updateParticle pIn =
    let v = addVector pIn.v pIn.a
    {
        p = addVector pIn.p v
        v = v
        a = pIn.a
    }

let updateAll (ps: Particle seq) =
    ps |> Seq.map updateParticle

let run (ps: Particle seq) =
    Seq.unfold
        (fun ps ->
            let nextPs = updateAll ps
            Some (ps, nextPs))
        ps


let manhattanDistance (x,y,z) = (abs x) + (abs y) + (abs z)


let testInput = """p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"""

let parseLine = testp parseParticle

let parseRows (rs: string seq) = rs |> Seq.map parseLine

[<EntryPoint>]
let main argv =
    let testParticles = splitIntoRows testInput |> parseRows |> Array.ofSeq

    let testRun =
        run testParticles
        |> Seq.take 4
        |> Seq.map Array.ofSeq
        |> Array.ofSeq

    testRun.[0].[0] =! { p = ( 3,0,0); v = ( 2,0,0); a = (-1,0,0) }
    testRun.[0].[1] =! { p = ( 4,0,0); v = ( 0,0,0); a = (-2,0,0) }

    testRun.[1].[0] =! { p = ( 4,0,0); v = ( 1,0,0); a = (-1,0,0) }
    testRun.[1].[1] =! { p = ( 2,0,0); v = (-2,0,0); a = (-2,0,0) }

    testRun.[2].[0] =! { p = ( 4,0,0); v = ( 0,0,0); a = (-1,0,0) }
    testRun.[2].[1] =! { p = (-2,0,0); v = (-4,0,0); a = (-2,0,0) }

    testRun.[3].[0] =! { p = ( 3,0,0); v = (-1,0,0); a = (-1,0,0) }
    testRun.[3].[1] =! { p = (-8,0,0); v = (-6,0,0); a = (-2,0,0) }


    // Part 1 requires us to find which particle is closest to <0,0,0>
    // "in the long term". From the example it's clear they don't care
    // about who actually came closest at any point. It's about who
    // is closest after we've been running long enough that it has
    // stablised, and that's actually going to be a question of who
    // has the smallest acceleration.

    let indexUltimatelyClosestToOrigin (ps: Particle seq) =
        ps
        |> Seq.mapi (fun i p -> (i, p))
        |> Seq.minBy (fun (_, p) -> manhattanDistance p.a)
        |> fst
    
    indexUltimatelyClosestToOrigin testParticles =! 0

    let particles = getEmbeddedRows () |> parseRows
    let part1Result = indexUltimatelyClosestToOrigin particles


    printfn "Part 1: %d" part1Result
    0

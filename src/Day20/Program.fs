// Learn more about F# at http://fsharp.org

open System

open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling

type Particle =
    {
        i: int
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
        (fun p v a -> { i = -1; p = p; v = v; a = a })

let addVector (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
let subVector (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
let vecX (x, _, _) = x
let vecY (_, y, _) = y
let vecZ (_, _, z) = z

let updateParticle pIn =
    let v = addVector pIn.v pIn.a
    {
        pIn with
            p = addVector pIn.p v
            v = v
    }

let updateAll (ps: Particle seq) =
    ps |> Seq.map updateParticle

let run (ps: Particle seq) =
    Seq.unfold
        (fun ps ->
            let nextPs = updateAll ps
            Some (ps, nextPs))
        ps

let updateAndRemoveCollisions (ps: Particle seq) =
    let updated = updateAll ps
    let countMap =
        updated
        |> Seq.fold
            (fun (m: Map<int*int*int, int*(Particle list)>) (p: Particle) ->
                match Map.tryFind p.p m with
                | Some (i, ps) ->
                    Map.add p.p ((i + 1), p::ps) m
                | None -> Map.add p.p (1, [p]) m)
            Map.empty
    updated
    |> Seq.filter (fun p -> fst (countMap.[p.p]) < 2)

let run2 (ps: Particle seq) =
    Seq.unfold
        (fun (ps, i) ->
            let nextPs = updateAndRemoveCollisions ps
            Some (ps, (nextPs, i + 1)))
        (ps, 0)


// The tricky part of part 2 is: how do we know when we're done?
// (I think maybe that was supposed to be the hard part in part 1 too,
// except I sidestepped it by realising you can work out the answer without
// ever running the simulation.)
// As it happens, you hit the final state fairly quickly if you just let it run,
// but I want to *know* it's the final case.
// So what we're going to do is derive a three formulae for each particle, describing
// respectively its x, y, and z position in terms of the time. These will be quadratic
// formulae, making it easy to discover for any pair of points the instants in time at
// which they have the same position in any particular dimension. If any pair of points
// are at the same positions in all three dimensions for some t, then we know they will
// intersect at time t. And the crucial thing is that there will be no more than 2 points
// in time at which any pair of particles collide (unless they are on identical trajectories).
// This enables us to know when we're done.
//
// So, how do we express a point's position in closed form (i.e., without having to iterate)?
// The iteration gives us recurrence relations for position and velocity:
//  p(t+1) = p(n) + v(t+1)
//  v(t+1) = v(t) + a
// Velocity is easy to expres in closed form:
//  v(t) = v(0) + t*a
// This enables us to rewrite the recurrence for p as:
//  p(t+1) = p(n) + v(0) + (t+1)*a
// So the closed form will be
//  p(t+1) = p(0) + t*v(0) + Sum(t * a)
//         = p(0) + t*v(0) + a*Sum(t)
// The general form for a sum to t is t(t-1)/2. But because the recurrence for p uses v(t+1)
// we have to go one higher, so we end up with t(t+1)/2
// This gives us:
//  p(t) = p(0) + t*v0 + (a * (t*t + t) / 2)
// Since the RHS now only uses initial values, we'll just express those as p, a, and v,
// to make it easier to read:
//  p(t) = p + t*v + a*(t*t + t) / 2
//       = p + t*v + a*(t*t)/2 + a*t / 2
//       = p + t(v + a/2) + a*(t*t)/2
// So the positions are described by a quadratic a*t*t + b*t + c with:
//  a = a/2
//  b = v + a/2
//  c = p

// That /2 is a problem because we're working with ints, so instead, we
// double everything else. This gives us a quadratic that will calculate
// coordinates that are double what they should be, but that's fine - we
// ultimately just want to find where two different particles collide, so
// an extra factor of 2 isn't a problem, and it enables us to avoid any
// pesky fractions.
let make2xQuadraticFromPosVelAcc ((p,v,a):int*int*int) =
    (a, 2 * v + a, 2*p)

// Used in tests so we can verify that the quadratic we produced generates
// the same positions for a particle as the iterative approach.
let evaluate2xQuadraticForT ((a,b,c):int*int*int) t =
    let x2Result = ((t * t) * a + t * b + c)
    if x2Result % 2 <> 0 then failwithf "Non-integer result %d/2" x2Result
    x2Result / 2


// This produces the x2 quadratics in all 3 dimensions for a single particle.
let make2xQuadraticsFromParticle (p: Particle) =
    let (ax, ay, az) = p.a
    let (vx, vy, vz) = p.v
    let (px, py, pz) = p.p
    let xq = make2xQuadraticFromPosVelAcc (px, vx, ax)
    let yq = make2xQuadraticFromPosVelAcc (py, vy, ay)
    let zq = make2xQuadraticFromPosVelAcc (pz, vz, az)
    (xq, yq, zq)


// When calculating whether two particles intersect, the possibilities are:
//  They don't
//  They cross once
//  They cross twice
//  They follow exactly the same path
// We know it has to be one of these because the positions are descibed by
// a quadratic equation. If the motion were more complex, there might be
// more cases to consider, but fortunately, acceleration is constant in
// this particle simulation.
//
// This data type captures how many intersections there are and, where appropriate,
// the time at which the intersection(s) occur(s). Note, we only consider intersections
// for integer values of time, so you might get fewer intersections than you'd expect.
// (The discrete nature of the simulation makes it possible for particles to cross
// each others paths without being considered to have 'collided' because the collision
// would have happened somewhere in between two time ticks. Call it quantum tunneling.)
type Intersections =
    | NoIntersections
    | OneIntersection of int
    | TwoIntersections of int*int
    | Identical



// Having calculated the times at which two particles have the same value at the
// same time for some particular dimension of their position, we then need to
// determine whether this happens across all three. (E.g., some pair of particles
// might have the same x and y position at time t, but a different z, in which case
// it's not a collision.) So we need to be able to take the intersections reported
// for different dimensions and see if they have any times in common, producing an
// Intersections describing those common times if so, and NoInteresections if not.
let intersectIntersections (i1: Intersections) (i2: Intersections) =
    match i1 with
    | NoIntersections -> NoIntersections
    | Identical -> i2
    | OneIntersection x1 ->
        match i2 with
        | NoIntersections -> NoIntersections
        | Identical -> i1
        | OneIntersection x2 ->
            if x1 = x2 then OneIntersection x1 else NoIntersections
        | TwoIntersections (x2a, x2b) ->
            if x1 = x2a then OneIntersection x1
            else if x1 = x2b then OneIntersection x1 else NoIntersections
    | TwoIntersections (x1a, x1b) ->
        match i2 with
        | NoIntersections -> NoIntersections
        | Identical -> i1
        | OneIntersection x2 ->
            if x1a = x2 then OneIntersection x2
            else if x1b = x2 then OneIntersection x2 else NoIntersections
        | TwoIntersections (x2a, x2b) ->
            if x1a = x2a then
                if x1b = x2b then TwoIntersections (x1a, x1b)
                else OneIntersection x1a
            else if x1a = x2b then
                if x1b = x2a then TwoIntersections (x1a, x1b)
                else OneIntersection x1a
            else if x1b = x2a then OneIntersection x1b
            else if x1b = x2b then OneIntersection x1b
            else NoIntersections

// Find whether the quadratic equations describing two particles' progress in
// a particular dimension with respect to time intersect for any integer values
// of time.
let findIntersections (a1: int, b1: int, c1: int) (a2: int, b2: int, c2: int) =
    let a = a1 - a2
    let b = b1 - b2
    let c = c1 - c2
    if a = 0 then
        // No acceleration in this dimension, which will cause the quadratic
        // formula to fail (due to division by zero). So instead, we have to
        // solve the remaining linear equation.
        // Linear: bx + c = 0
        //             bx = -c
        //              x = -c/b
        if b = 0 then
            // The two particles have zero acceleration and the same velocity in
            // this dimension.
            // If they started in the same place, then they are identical (they
            // interesect for all t), otherwise they do not intersect at all.
            if c = 0 then Identical
            else NoIntersections
        else
            // The two particles have zero acceleration and different velocities
            // in this dimension,  so there will definitely be some moment in time
            // (possibly in the past) where their value in this dimension matches,
            // but we return that time only if it has an integer value.
            let x = -c / b
            // Only take working integer solutions
            if (b * x) + c = 0 then OneIntersection x
            else NoIntersections
    else
        let b2 = b * b
        let ac4 = 4 * a * c

        // The quadratic formula is producing only imaginary solutions,
        // meaning the two particles do not have the same position in this
        // dimension for any real value of time. (And we're not doing
        // imaginary time.)
        if ac4 > b2 then NoIntersections
        else
            let fSqrtPart = sqrt (double (b2 - ac4)) |> int
            let solve s = (-b + (s * fSqrtPart)) / (2 * a)
            let s1 = solve 1
            let s2 = solve -1
            // Again, we only want solutions for integer values
            // of time.
            let s1IsExact = ((a * s1 * s1) + (b * s1) + c) = 0
            let s2IsExact = ((a * s2 * s2) + (b * s2) + c) = 0
            if s1IsExact then
                if s2IsExact then
                    if s1 = s2 then OneIntersection s1 else TwoIntersections (s1, s2)
                else OneIntersection s1
            else
                if s2IsExact then OneIntersection s2
                else NoIntersections


// Find the moments in time at which two particles are in exactly the
// same location in all three dimensions.
let findParticleIntersections (p1: Particle) (p2: Particle) =
    let xq1, yq1, zq1 = make2xQuadraticsFromParticle p1
    let xq2, yq2, zq2 = make2xQuadraticsFromParticle p2
    let xIntersections = findIntersections xq1 xq2
    let yIntersections = findIntersections yq1 yq2
    let zIntersections = findIntersections zq1 zq2
    let xyIntersections = intersectIntersections xIntersections yIntersections
    intersectIntersections xyIntersections zIntersections


// Determines all of the particles whose paths collide with other particles
// at some integer moment in time. (Returns a list of Particle*int where
// the int is the time at which they collide.)
// Note that not all of these collisions will necessarily happen, because this
// function is effectively looking at the entire (infinite) paths taken by all
// particles and working out which of these paths intersect, and where. But
// particles that collide don't carry on after that. This means that if some
// paticular particle is determined to collide with two other particles at
// different times, the second collision will never actually happen, because
// the particle will be anihilated in its first collision, never going on to
// cause the second.
// This is fine because we can then filter out all the collisions that don't
// happen due to earlier anihilations. At this point, we just want to perform
// the (relatively) simple task of finding all collisions that might happen.
let calculatePotentialCollisions (ps: Particle seq) =
    ps
    |> Seq.fold
        (fun (collisions: (Particle * int) list) (p: Particle) ->
            ps
            |> Seq.fold
                (fun (collisions: (Particle * int) list) x ->
                    if x = p then collisions
                    else
                        match findParticleIntersections x p with
                        | NoIntersections -> collisions
                        | OneIntersection t ->
                            printfn "%A matches %A at %d" p.i x.i t
                            (p, t)::collisions
                        | TwoIntersections (t1, t2) ->
                            printfn "%A matches %A at %d and %d" p.i x.i t1 t2
                            (p, t1)::(p, t2)::collisions
                        | Identical ->
                            printfn "%A and %A are equivalent" p.i x.i
                            (p, 0)::collisions)
                collisions)
        []

let removeCollisions (ps: Particle seq) =
    // This finds all the collisions that would happen eventually if particles
    // were not anihilated in their first collision.
    let allPossibleCollisions = calculatePotentialCollisions ps

    // Order the possible collisions by time, so we can process them in time
    // order, enabling us to work out which possible collisions cannot happen
    // because at least one of the particles involved got wiped out earlier.
    let collisionsByTime =
        allPossibleCollisions
        |> Seq.groupBy snd
        |> Seq.sortBy fst

    collisionsByTime
    |> Seq.fold
        (fun (remainingParticles: Set<Particle>) (t: int, cps: (Particle*int) seq) ->
            let collisionsInvolvingParticlesNotAlreadyAnihilated =
                cps
                |> Seq.map fst
                |> Seq.filter (fun p -> Set.contains p remainingParticles)
                |> Set.ofSeq
            if not (Set.isEmpty collisionsInvolvingParticlesNotAlreadyAnihilated) then
                let idsOfParticlesBeingRemoved =
                    collisionsInvolvingParticlesNotAlreadyAnihilated
                    |> Seq.map (fun p -> p.i)
                    |> List.ofSeq
                printfn "Time: %d, removing %A" t idsOfParticlesBeingRemoved
            Set.difference remainingParticles collisionsInvolvingParticlesNotAlreadyAnihilated)
        (Set.ofSeq ps)


let manhattanDistance (x,y,z) = (abs x) + (abs y) + (abs z)

let testInput = """p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"""

let testInput2 = """p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"""

let parseLine = testp parseParticle

let parseRows (rs: string seq) =
    rs
    |> Seq.mapi
        (fun i s ->
            {
                parseLine s with
                    i = i
            })

[<EntryPoint>]
let main argv =
    let testParticles = splitIntoRows testInput |> parseRows |> Array.ofSeq

    let testRun =
        run testParticles
        |> Seq.take 4
        |> Seq.map Array.ofSeq
        |> Array.ofSeq

    testRun.[0].[0] =! { i = 0; p = ( 3,0,0); v = ( 2,0,0); a = (-1,0,0) }
    testRun.[0].[1] =! { i = 1; p = ( 4,0,0); v = ( 0,0,0); a = (-2,0,0) }

    testRun.[1].[0] =! { i = 0; p = ( 4,0,0); v = ( 1,0,0); a = (-1,0,0) }
    testRun.[1].[1] =! { i = 1; p = ( 2,0,0); v = (-2,0,0); a = (-2,0,0) }

    testRun.[2].[0] =! { i = 0; p = ( 4,0,0); v = ( 0,0,0); a = (-1,0,0) }
    testRun.[2].[1] =! { i = 1; p = (-2,0,0); v = (-4,0,0); a = (-2,0,0) }

    testRun.[3].[0] =! { i = 0; p = ( 3,0,0); v = (-1,0,0); a = (-1,0,0) }
    testRun.[3].[1] =! { i = 1; p = (-8,0,0); v = (-6,0,0); a = (-2,0,0) }

    let xq1, yq1, zq1 = make2xQuadraticsFromParticle testParticles.[0]
    let xq2, yq2, zq2 = make2xQuadraticsFromParticle testParticles.[1]
    evaluate2xQuadraticForT xq1 0 =! vecX testRun.[0].[0].p
    evaluate2xQuadraticForT xq1 1 =! vecX testRun.[1].[0].p
    evaluate2xQuadraticForT xq1 2 =! vecX testRun.[2].[0].p
    evaluate2xQuadraticForT yq1 0 =! vecY testRun.[0].[0].p
    evaluate2xQuadraticForT yq1 1 =! vecY testRun.[1].[0].p
    evaluate2xQuadraticForT yq1 2 =! vecY testRun.[2].[0].p
    evaluate2xQuadraticForT zq1 0 =! vecZ testRun.[0].[0].p
    evaluate2xQuadraticForT zq1 1 =! vecZ testRun.[1].[0].p
    evaluate2xQuadraticForT zq1 2 =! vecZ testRun.[2].[0].p

    evaluate2xQuadraticForT xq2 0 =! vecX testRun.[0].[1].p
    evaluate2xQuadraticForT xq2 1 =! vecX testRun.[1].[1].p
    evaluate2xQuadraticForT xq2 2 =! vecX testRun.[2].[1].p
    evaluate2xQuadraticForT yq2 0 =! vecY testRun.[0].[1].p
    evaluate2xQuadraticForT yq2 1 =! vecY testRun.[1].[1].p
    evaluate2xQuadraticForT yq2 2 =! vecY testRun.[2].[1].p
    evaluate2xQuadraticForT zq2 0 =! vecZ testRun.[0].[1].p
    evaluate2xQuadraticForT zq2 1 =! vecZ testRun.[1].[1].p
    evaluate2xQuadraticForT zq2 2 =! vecZ testRun.[2].[1].p


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

    let particles = getEmbeddedRows () |> parseRows |> Array.ofSeq
    let part1Result = indexUltimatelyClosestToOrigin particles


    printfn "Part 1: %d" part1Result


    let testParticles2 = splitIntoRows testInput2 |> parseRows |> Array.ofSeq
    let testRun2 =
        run2 testParticles2
        |> Seq.take 4
        |> Seq.map Array.ofSeq
        |> Array.ofSeq

    testRun2.[0].Length =! 4
    testRun2.[0].[0] =! { i = 0; p = (-6,0,0); v = ( 3,0,0); a = ( 0,0,0) }
    testRun2.[0].[1] =! { i = 1; p = (-4,0,0); v = ( 2,0,0); a = ( 0,0,0) }
    testRun2.[0].[2] =! { i = 2; p = (-2,0,0); v = ( 1,0,0); a = ( 0,0,0) }
    testRun2.[0].[3] =! { i = 3; p = ( 3,0,0); v = (-1,0,0); a = ( 0,0,0) }

    testRun2.[1].Length =! 4
    testRun2.[1].[0] =! { i = 0; p = (-3,0,0); v = ( 3,0,0); a = ( 0,0,0) }
    testRun2.[1].[1] =! { i = 1; p = (-2,0,0); v = ( 2,0,0); a = ( 0,0,0) }
    testRun2.[1].[2] =! { i = 2; p = (-1,0,0); v = ( 1,0,0); a = ( 0,0,0) }
    testRun2.[1].[3] =! { i = 3; p = ( 2,0,0); v = (-1,0,0); a = ( 0,0,0) }

    
    testRun2.[2].Length =! 1
    testRun2.[2].[0] =! { i = 3; p = ( 1,0,0); v = (-1,0,0); a = ( 0,0,0) }

    // Lots of tests because I made lots of little mistakes while writing this...

    let x2q1, _, _= make2xQuadraticsFromParticle testParticles2.[0]
    let x2q2, _, _= make2xQuadraticsFromParticle testParticles2.[1]
    let x2q3, _, _= make2xQuadraticsFromParticle testParticles2.[2]
    let x2q4, _, _= make2xQuadraticsFromParticle testParticles2.[3]
    evaluate2xQuadraticForT x2q1 0 =! vecX testRun2.[0].[0].p
    evaluate2xQuadraticForT x2q1 1 =! vecX testRun2.[1].[0].p
    evaluate2xQuadraticForT x2q1 2 =! 0

    evaluate2xQuadraticForT x2q2 0 =! vecX testRun2.[0].[1].p
    evaluate2xQuadraticForT x2q2 1 =! vecX testRun2.[1].[1].p
    evaluate2xQuadraticForT x2q2 2 =! 0

    evaluate2xQuadraticForT x2q3 0 =! vecX testRun2.[0].[2].p
    evaluate2xQuadraticForT x2q3 1 =! vecX testRun2.[1].[2].p
    evaluate2xQuadraticForT x2q3 2 =! 0

    evaluate2xQuadraticForT x2q4 0 =! vecX testRun2.[0].[3].p
    evaluate2xQuadraticForT x2q4 1 =! vecX testRun2.[1].[3].p
    evaluate2xQuadraticForT x2q4 2 =! vecX testRun2.[2].[0].p


    findParticleIntersections testParticles2.[0] testParticles2.[1] =! OneIntersection 2
    findParticleIntersections testParticles2.[1] testParticles2.[2] =! OneIntersection 2

    let test2NonCollidingParticles = removeCollisions testParticles2

    test2NonCollidingParticles.Count =! 1
    

    let xp0q, yp0q, zp0q = make2xQuadraticsFromParticle particles.[0]
    let xp1q, yp1q, zp1q = make2xQuadraticsFromParticle particles.[1]
    let testRunp =
        run [particles.[0];particles.[1]]
        |> Seq.take 42
        |> Seq.map Array.ofSeq
        |> Array.ofSeq
    for i in seq { 0..41 } do
        evaluate2xQuadraticForT xp0q i =! vecX testRunp.[i].[0].p
        evaluate2xQuadraticForT yp0q i =! vecY testRunp.[i].[0].p
        evaluate2xQuadraticForT zp0q i =! vecZ testRunp.[i].[0].p
        evaluate2xQuadraticForT xp1q i =! vecX testRunp.[i].[1].p
        evaluate2xQuadraticForT yp1q i =! vecY testRunp.[i].[1].p
        evaluate2xQuadraticForT zp1q i =! vecZ testRunp.[i].[1].p
    
    
    findParticleIntersections particles.[0] particles.[1] =! OneIntersection 39
    findParticleIntersections particles.[1] particles.[0] =! OneIntersection 39

    findParticleIntersections particles.[29] particles.[28] =! OneIntersection 10
    findParticleIntersections particles.[28] particles.[29] =! OneIntersection 10

    findParticleIntersections particles.[282] particles.[285] =! OneIntersection 11
    findParticleIntersections particles.[285] particles.[282] =! OneIntersection 11

    findParticleIntersections particles.[285] particles.[284] =! OneIntersection 11
    findParticleIntersections particles.[284] particles.[285] =! OneIntersection 11

    findParticleIntersections particles.[282] particles.[284] =! OneIntersection 11
    findParticleIntersections particles.[282] particles.[286] =! OneIntersection 11
    findParticleIntersections particles.[282] particles.[287] =! OneIntersection 11
    findParticleIntersections particles.[282] particles.[283] =! OneIntersection 11


    let part2NonCollidingParticles = removeCollisions particles

    printfn ""
    printfn "Part 2: %d" (part2NonCollidingParticles.Count)

    let part2Run = run2 particles

    printfn ""
    printfn ""
    printfn "You have your answer now, but just to verify that, we'll let the iterative approach run"
    printfn "Note: this never terminates - it will site in a loop looking for more collisions that never come"
    printfn "So once it has got down to the same number reported, you can kill it off"

    let part2LengthChanges =
        part2Run
        |> Seq.windowed 2
        |> Seq.filter (fun arr ->
            if Seq.length arr.[0] <> Seq.length arr.[1] then
                true
            else
                false)
        |> Seq.map (fun arr ->
            let idsOf ps =
                ps
                |> Seq.map (fun p -> p.i)
                |> Set.ofSeq
            let idsBefore = idsOf arr.[0]
            let idsAfter = idsOf arr.[1]
            let idsRemoved = Set.difference idsBefore idsAfter

            (idsAfter.Count, List.ofSeq idsRemoved))

    for x in part2LengthChanges do
        printfn "%A" x

    0

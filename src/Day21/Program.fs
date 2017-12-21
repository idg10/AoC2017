open System

open FParsec
open Swensen.Unquote

open ParsingHelpers
open TextHandling
open System.ComponentModel.DataAnnotations

type Row2 = Row2 of bool*bool
type Row3 = Row3 of bool*bool*bool
type Row4 = Row4 of bool*bool*bool*bool

type Square2x2 = Square2x2 of Row2*Row2
type Square3x3 = Square3x3 of Row3*Row3*Row3
type Square4x4 =Square4x4 of Row4*Row4*Row4*Row4


let pCell<'a> : Parser<bool, 'a> =
    (pchar '#' >>% true) <|> (pchar '.' >>% false)

let pRow2<'a> : Parser<Row2, 'a> =
    pipe2 pCell pCell (fun x y -> Row2 (x, y))

let pRow3<'a> : Parser<Row3, 'a> =
    pipe3 pCell pCell pCell (fun x y z -> Row3 (x, y, z))

let pRow4<'a> : Parser<Row4, 'a> =
    pipe4 pCell pCell pCell pCell (fun x y z q -> Row4 (x, y, z, q))

let parse2x2<'a> : Parser<Square2x2, 'a> =
    pipe2
        (pRow2 .>> pchar '/')
        pRow2
        (fun x y -> Square2x2 (x, y))

let parse3x3<'a> : Parser<Square3x3, 'a> =
    pipe3
        (pRow3 .>> pchar '/')
        (pRow3 .>> pchar '/')
        pRow3
        (fun x y z -> Square3x3 (x, y, z))

let parse4x4<'a> : Parser<Square4x4, 'a> =
    pipe4
        (pRow4 .>> pchar '/')
        (pRow4 .>> pchar '/')
        (pRow4 .>> pchar '/')
        pRow4
        (fun x y z q -> Square4x4 (x, y, z, q))

type Rule =
    | RuleFrom2x2 of Square2x2*Square3x3
    | RuleFrom3x3 of Square3x3*Square4x4

let parseRuleFrom2x2<'a> : Parser<Rule, 'a> =
    pipe2
        (parse2x2 .>> spaces .>> pstring "=>" .>> spaces)
        parse3x3
        (fun k v -> RuleFrom2x2 (k, v))

let parseRuleFrom3x3<'a> : Parser<Rule, 'a> =
    pipe2
        (parse3x3 .>> spaces .>> pstring "=>" .>> spaces)
        parse4x4
        (fun k v -> RuleFrom3x3 (k, v))

let parseRule<'a> : Parser<Rule, 'a> =
    (attempt parseRuleFrom3x3) <|> parseRuleFrom2x2
    
let parseRuleLine = testp parseRule
let parseRules (xs: string seq) =
    xs
    |> Seq.map parseRuleLine
    |> Array.ofSeq

let testInput = """../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#"""

type RuleSet =
    {
        rules2x2: Map<Square2x2, Square3x3>
        rules3x3: Map<Square3x3, Square4x4>
    }

let makeRuleSet (rules: Rule seq) =
    rules
    |> Seq.fold
        (fun (rs: RuleSet) (r: Rule) ->
            match r with
            | RuleFrom2x2 (k,v) ->
                { rs with rules2x2 = Map.add k v rs.rules2x2 }
            | RuleFrom3x3 (k,v) ->
                { rs with rules3x3 = Map.add k v rs.rules3x3 }
            )
        { rules2x2 = Map.empty; rules3x3 = Map.empty }


let lookupRule2x2 (rs: RuleSet) (k: Square2x2) =
    let rotate (Square2x2 (Row2 (v00, v01), Row2 (v10, v11))) =
        Square2x2 (Row2 (v01, v11), Row2 (v00, v10))
    let flip (Square2x2 (Row2 (v00, v01), Row2 (v10, v11))) =
        Square2x2 (Row2 (v10, v11), Row2 (v00, v01))
    let attemptSequence = [rotate;rotate;rotate;rotate;flip;rotate;rotate;rotate]
    attemptSequence
    |> Seq.scan
        (fun (_, k: Square2x2) (tx: Square2x2 -> Square2x2) ->
            let newKey = tx k
            (Map.tryFind newKey rs.rules2x2, newKey))
        (None, k)
    |> Seq.choose fst
    |> Seq.take 1
    |> Seq.exactlyOne

let lookupRule3x3 (rs: RuleSet) (k: Square3x3) =
    let rotate (Square3x3 (Row3 (v00, v01, v02), Row3 (v10, v11, v12), Row3 (v20, v21, v22))) =
        Square3x3 (Row3 (v02, v12, v22), Row3 (v01, v11, v21), Row3 (v00, v10, v20))
    let flip (Square3x3 (Row3 (v00, v01, v02), Row3 (v10, v11, v12), Row3 (v20, v21, v22))) =
        Square3x3 (Row3 (v20, v21, v22), Row3 (v10, v11, v12), Row3 (v00, v01, v02))
    let attemptSequence = [rotate;rotate;rotate;rotate;flip;rotate;rotate;rotate]
    attemptSequence
    |> Seq.scan
        (fun (_, k: Square3x3) (tx: Square3x3 -> Square3x3) ->
            let newKey = tx k
            (Map.tryFind newKey rs.rules3x3, newKey))
        (None, k)
    |> Seq.choose fst
    |> Seq.take 1
    |> Seq.exactlyOne

type Image =
    {
        pixels: bool[][]
    }

let startImagePixels =""".#.
..#
###"""

let loadImage (i: string) =
    let rows = splitIntoRows i
    {
        pixels =
            rows
            |> Array.map
                (fun s ->
                    s
                    |> Seq.map (fun c -> c = '#')
                    |> Array.ofSeq)
    }

let enhance (i:Image) (rs: RuleSet) =
    let size = i.pixels.Length
    let data =
        if size % 2 = 0 then
            seq { 0..(size / 2 - 1) }
            |> Seq.map
                (fun y ->
                    seq { 0..(size / 2 - 1) }
                    |> Seq.map
                        (fun x ->
                            let k =
                                Square2x2
                                    (Row2 (i.pixels.[y*2    ].[x*2], i.pixels.[y*2    ].[x*2 + 1]),
                                     Row2 (i.pixels.[y*2 + 1].[x*2], i.pixels.[y*2 + 1].[x*2 + 1]))
                            lookupRule2x2 rs k)
                    |> Seq.fold
                        (fun (row1: bool seq, row2: bool seq, row3: bool seq) (s: Square3x3) ->
                            let (Square3x3 (Row3 (v00, v01, v02), Row3 (v10, v11, v12), Row3 (v20, v21, v22))) = s
                            (Seq.append row1 [v00; v01; v02], Seq.append row2 [v10; v11; v12], Seq.append row3 [v20; v21; v22]))
                        (Seq.empty, Seq.empty, Seq.empty))
                    |> Seq.collect (fun (r1, r2, r3) -> [r1;r2;r3])
        else
            seq { 0..(size / 3 - 1) }
            |> Seq.map
                (fun y ->
                    seq { 0..(size / 3 - 1) }
                    |> Seq.map
                        (fun x ->
                            let k =
                                Square3x3
                                    (Row3 (i.pixels.[y*3    ].[x*3], i.pixels.[y*3    ].[x*3 + 1], i.pixels.[y*3    ].[x*3 + 2]),
                                     Row3 (i.pixels.[y*3 + 1].[x*3], i.pixels.[y*3 + 1].[x*3 + 1], i.pixels.[y*3 + 1].[x*3 + 2]),
                                     Row3 (i.pixels.[y*3 + 2].[x*3], i.pixels.[y*3 + 2].[x*3 + 1], i.pixels.[y*3 + 2].[x*3 + 2]))
                            lookupRule3x3 rs k)
                    |> Seq.fold
                        (fun (row1: bool seq, row2: bool seq, row3: bool seq, row4: bool seq) (s: Square4x4) ->
                            let (Square4x4 (Row4 (v00, v01, v02, v03), Row4 (v10, v11, v12, v13), Row4 (v20, v21, v22, v23), Row4 (v30, v31, v32, v33))) = s
                            (Seq.append row1 [v00; v01; v02; v03], Seq.append row2 [v10; v11; v12; v13], Seq.append row3 [v20; v21; v22; v23], Seq.append row4 [v30; v31; v32; v33]))
                        (Seq.empty, Seq.empty, Seq.empty, Seq.empty))
                    |> Seq.collect (fun (r1, r2, r3, r4) -> [r1;r2;r3;r4])
    {
        pixels =
            data
            |> Seq.map Array.ofSeq
            |> Array.ofSeq
    }

let printImage (i: Image) =
    for row in i.pixels do
        for p in row do
            printf "%c" (if p then '#' else '.')
        printfn ""

[<EntryPoint>]
let main argv =
    let rules = getEmbeddedRows () |> parseRules
    let ruleSet = makeRuleSet rules


    let testRules = splitIntoRows testInput |> parseRules
    let testRule1 = testRules.[0]
    let rule1Key = Square2x2 (Row2 (false, false), Row2 (false, true))
    let rule1Value = Square3x3 (Row3 (true, true, false), Row3 (true, false, false), Row3 (false, false, false))
    testRule1 =! RuleFrom2x2 (rule1Key, rule1Value)

    let testRule2 = testRules.[1]
    testRule2 =!
        RuleFrom3x3
            (Square3x3 (Row3 (false, true, false), Row3 (false, false, true), Row3 (true, true, true)),
             Square4x4 (Row4 (true, false, false, true), Row4 (false, false, false, false), Row4 (false, false, false, false), Row4 (true, false, false, true)))


    let testRs = makeRuleSet testRules
    lookupRule2x2 testRs rule1Key =! rule1Value
    lookupRule2x2 testRs (Square2x2 (Row2 (false, true), Row2 (false, false))) =! rule1Value
    lookupRule2x2 testRs (Square2x2 (Row2 (true, false), Row2 (false, false))) =! rule1Value
    lookupRule2x2 testRs (Square2x2 (Row2 (false, false), Row2 (true, false))) =! rule1Value

    let startImage = loadImage startImagePixels
    printImage startImage
    printfn ""
    let makeImageSteps rs =
        Seq.unfold
            (fun (i: Image) ->
                let nextImage = enhance i rs
                Some (nextImage, nextImage))
            startImage
    let testImageSteps =
        makeImageSteps testRs
        |> Seq.take 2
    for image in testImageSteps do
        printImage image
        printfn ""

    printfn ""
    printfn ""
    printfn "Part 1 steps:"
    printfn ""
    let imageSteps =
        makeImageSteps ruleSet
        |> Seq.take 5
    for image in imageSteps do
        printImage image
        printfn ""

    let finalImage = imageSteps |> Seq.last
    let count =
        finalImage.pixels
        |> Seq.collect id
        |> Seq.filter id
        |> Seq.length

    printfn "Part 1: %d" count
    0 // return an integer exit code

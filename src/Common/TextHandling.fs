module TextHandling

let splitIntoRows (sheet : string) = sheet.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)

let getEmbeddedStream () =
    let asm = System.Reflection.Assembly.GetEntryAssembly()
    let stream = asm.GetManifestResourceStream("Input.txt")
    if stream = null then asm.GetManifestResourceStream(asm.GetName().Name + ".Input.txt") else stream

let getEmbeddedInput () =
    let stream = getEmbeddedStream ()
    use reader = new System.IO.StreamReader(stream)
    reader.ReadToEnd()

let getEmbeddedRows () = getEmbeddedInput() |> splitIntoRows

let getInputAsCharSequence () =
    let stream = getEmbeddedStream()
    seq
        {
            use reader = new System.IO.StreamReader(stream)
            
            while not reader.EndOfStream do
                yield reader.Read() |> char
        }
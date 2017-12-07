module TextHandling

let splitIntoRows (sheet : string) = sheet.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)

let getEmbeddedInput () =
    let asm = System.Reflection.Assembly.GetEntryAssembly()
    let stream = asm.GetManifestResourceStream("Input.txt")
    let stream = if stream = null then asm.GetManifestResourceStream(asm.GetName().Name + ".Input.txt") else stream
    use reader = new System.IO.StreamReader(stream)
    reader.ReadToEnd()

let getEmbeddedRows () = getEmbeddedInput() |> splitIntoRows
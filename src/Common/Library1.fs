module TextHandling

let splitIntoRows (sheet : string) = sheet.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)

let getEmbeddedInput () =
    let asm = System.Reflection.Assembly.GetEntryAssembly()
    use reader = new System.IO.StreamReader(asm.GetManifestResourceStream("Input.txt"))
    reader.ReadToEnd()

let getEmbeddedRows () = getEmbeddedInput() |> splitIntoRows
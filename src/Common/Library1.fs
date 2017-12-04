module TextHandling

let splitIntoRows (sheet : string) = sheet.Split([|'\n';'\r'|], System.StringSplitOptions.RemoveEmptyEntries)

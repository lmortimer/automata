open System.IO
open FSharp.Formatting.Literate

[<EntryPoint>]
let main argv =
          
    let fsi = FSharp.Formatting.Literate.Evaluation.FsiEvaluator()

    let source = __SOURCE_DIRECTORY__
    let template = Path.Combine(source, "docs/template.html")

    let script = Path.Combine(source, "docs/numbers.fsx")

    Literate.ConvertScriptFile(script, template, lineNumbers=false, fsiEvaluator=fsi)
    Literate.ConvertScriptFile(script,  fsiEvaluator=fsi, outputKind=OutputKind.Markdown)
    0
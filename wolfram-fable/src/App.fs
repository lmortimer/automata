module App


open Fable.Import

let window = Browser.Dom.window

type Cell = Empty | Full

type Rule = (Cell * Cell * Cell) -> Cell

type Color = { R : int; G : int; B : int; A : int }


let rule90: Rule = function
    | (Full, Full, Full) -> Empty
    | (Full, Full, Empty) -> Full
    | (Full, Empty, Full) -> Empty
    | (Full, Empty, Empty) -> Full
    | (Empty, Full, Full) -> Full
    | (Empty, Full, Empty) -> Empty
    | (Empty, Empty, Full) -> Full
    | (Empty, Empty, Empty) -> Empty
    
let rule30: Rule = function
    | (Full, Full, Full) -> Empty
    | (Full, Full, Empty) -> Empty
    | (Full, Empty, Full) -> Empty
    | (Full, Empty, Empty) -> Full
    | (Empty, Full, Full) -> Full
    | (Empty, Full, Empty) -> Full
    | (Empty, Empty, Full) -> Full
    | (Empty, Empty, Empty) -> Empty

let rule222: Rule = function 
    | (Full, Full, Full) -> Full
    | (Full, Full, Empty) -> Full
    | (Full, Empty, Full) -> Empty
    | (Full, Empty, Empty) -> Full
    | (Empty, Full, Full) -> Full
    | (Empty, Full, Empty) -> Full
    | (Empty, Empty, Full) -> Full
    | (Empty, Empty, Empty) -> Empty
    
let rule110: Rule = function 
    | (Full, Full, Full) -> Empty
    | (Full, Full, Empty) -> Full
    | (Full, Empty, Full) -> Full
    | (Full, Empty, Empty) -> Empty
    | (Empty, Full, Full) -> Full
    | (Empty, Full, Empty) -> Full
    | (Empty, Empty, Full) -> Full
    | (Empty, Empty, Empty) -> Empty



/// Helper to generate a `Full` in the middle of a row of `Empty`
let generateStandardFirstRow (width: int) =
    
    if width % 2 = 0 then invalidArg "width" (sprintf "Value must be an odd number. Value passed was %d." width)
    
    Seq.init width (function
        | n when n = (width / 2) -> Full
        | _ -> Empty
    )
    
/// Given a rule and a row, generate the next row
let generateNextRow (rule: Rule) (row: Cell seq) =
    let generatedCells =
        row
        |> Seq.windowed 3
        |> Seq.map (fun v ->
            rule (v.[0], v.[1], v.[2])
        )
        
    // pad the first and last cells of the new row with Empty, as no value was generated for them above
    seq {
        yield Empty
        yield! generatedCells
        yield Empty
    }

let generatePattern (rule: Rule) (firstRow: Cell seq) = 
    firstRow
    |> Seq.unfold (fun row ->
        Some(row, (generateNextRow rule row))
        )

let drawOnCanvas cols rows pattern =
    let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"  // myCanvas is defined in public/index.html

    let ctx = myCanvas.getContext_2d()
    let imageData = ctx.createImageData(float cols, float rows)        


    printfn "Generating grid"

    let grid =
        generateStandardFirstRow cols
        |> generatePattern pattern
        |> Seq.take rows

    printfn "Grid generated. Starting imagedata loop"

    let withIndexes x = x |> Seq.mapi (fun index item -> (index, item))
    
    for (rowIndex, row) in (withIndexes grid) do
                
        for (cellIndex, cell) in (withIndexes row) do

            let imageDataIndex = (cellIndex + rowIndex * cols) * 4

            let colour = 
                match cell with
                | Full -> { R = 0; G = 0; B = 0; A = 255 }
                | Empty -> { R = 255; G = 255; B = 255; A = 255 }

            imageData.data.[imageDataIndex+0] <- uint8 colour.R
            imageData.data.[imageDataIndex+1] <- uint8 colour.G
            imageData.data.[imageDataIndex+2] <- uint8 colour.B
            imageData.data.[imageDataIndex+3] <- uint8 colour.A

    printfn "Calling putImageData"
    
    ctx.putImageData(imageData, 0., 0.)

    printfn "Grid rendered"

drawOnCanvas 301 400 rule90

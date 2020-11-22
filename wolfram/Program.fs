open System;
open SixLabors.ImageSharp;
open SixLabors.ImageSharp.PixelFormats;

type Cell = Empty | Full

type Rule = (Cell * Cell * Cell) -> Cell

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

let drawCell = function
| Full -> "X"
| Empty -> "."

/// Render the grid to an ASCII string
let drawGrid (grid: Cell seq seq) =
    grid
    |> Seq.map (fun row ->
        row
        |> Seq.map drawCell
        |> String.concat ""
    )
    |> String.concat "\n"


let gridToPrimitive (grid: Cell seq seq) =
    grid
    |> Seq.map (fun row ->
        row
        |> Seq.map drawCell
        |> Seq.toArray
    )
    |> Seq.toArray

let generatePng cols rows pattern filename =
    
    let image = new Image<Rgba32>(cols, rows);
    let white = new Rgba32(255F, 255F, 100F, 1F);
    let black = new Rgba32(0f, 0f, 0f, 1F);

    let firstRow = generateStandardFirstRow cols
    
    let grid =
        generatePattern pattern firstRow
        |> Seq.take rows
        |> gridToPrimitive

    for rowIndex in 0 .. (grid.Length - 1) do
        for columnIndex in 0 .. (grid.[rowIndex].Length - 1) do
            
            if grid.[rowIndex].[columnIndex] = "X" then
                image.[columnIndex, rowIndex] <- black
                
            if grid.[rowIndex].[columnIndex] = "." then
                image.[columnIndex, rowIndex] <- white
                
    image.Save(sprintf "%s.png" filename);


let generateRule(ruleNumber: byte) = 
    let ruleBitString = Convert.ToString(ruleNumber, 2).PadLeft(8, '0');

    let patternToBitStringIndex = function 
        | (Full, Full, Full) -> 0
        | (Full, Full, Empty) -> 1
        | (Full, Empty, Full) -> 2
        | (Full, Empty, Empty) -> 3
        | (Empty, Full, Full) -> 4
        | (Empty, Full, Empty) -> 5
        | (Empty, Empty, Full) -> 6
        | (Empty, Empty, Empty) -> 7

    let ruleFn (cells: Cell * Cell * Cell) = 
        let bitIndex = patternToBitStringIndex cells

        match ruleBitString.[bitIndex] with
        | '0' -> Empty
        | '1' -> Full
        | _ -> raise (Exception("Unexpected input"))
        
    ruleFn
    


[<EntryPoint>]
let main argv =
          
    let rule150 = generateRule(150uy)

    generatePng 1301 900 rule150 "rule150"
    
//    let firstRow = generateStandardFirstRow 51
//    let rows = generatePattern rule222 firstRow |> Seq.take 10
//                
////    printfn "%s" (drawGrid rows)
//    let primitive = gridToPrimitive rows
//    printfn "%A" primitive
    0
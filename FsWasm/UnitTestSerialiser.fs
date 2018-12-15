module UnitTestSerialiser

open System.Text
open FsWasmLibrary.Wasm

let ModuleToUnitTestString (fileName:string) (m:Module) =

    let sb = new StringBuilder ()

    let Text (t:string) = 
        sb.Append t |> ignore
        sb.AppendLine "" |> ignore

    let AddObject obj =
        sb.AppendLine "" |> ignore
        sb.AppendLine "" |> ignore
        sb.Append (sprintf "%A" obj) |> ignore
        sb.AppendLine "" |> ignore

    let AddQuickly optionalObj =
        match optionalObj with
            | None -> ()
            | Some(X) -> AddObject X

    let AddArraySection cs =
        // TODO: I want to dump the array with numbering down the side, and using %A for each element.
        match cs with   
            | [|xs|] -> AddQuickly (Some(xs))   // TODO: did I only match the first element?
            | _ -> ()

    let AddGeneric g =
        AddQuickly g

    let AddCodeSec (optionalCodeSec:CodeSec option) =

        // LOCALS

        let addLocals localsArray =

            let mutable localIndex = 0

            let rec addIndividualLocal repeatCounter t =
                if repeatCounter >= 1u then
                    Text (sprintf "Local #%d %A" localIndex t)
                    localIndex <- localIndex + 1
                    addIndividualLocal (repeatCounter - 1u) t

            let addLocals (locals:Locals) =
                match locals.NumRepeats with
                    | WasmU32(repeatCounter) -> addIndividualLocal repeatCounter locals.LocalsType

            localsArray |> Array.iter addLocals

        // BODY

        let addBody instructionsArray =

            let mutable ip = 0
            let mutable indent = 0

            let addLine s =
                sb.Append (sprintf "  %3d | " ip) |> ignore
                sb.Append (new string(' ', (indent * 2))) |> ignore
                sb.AppendLine s |> ignore
                ip <- ip + 1

            let rec addInstructions instructionsArray =

                let isShortForm instruction =
                    match instruction with 
                        | Block(_) -> false
                        | Loop(_) -> false
                        | If(_) -> false
                        | IfElse(_) -> false
                        | _ -> true

                let subList opCode blockType subInstructions =
                    let blockIndex = ip
                    addLine (sprintf "%s %A" opCode blockType)
                    indent <- indent + 1
                    addInstructions subInstructions
                    // addLine (sprintf "End %s (%d)" opCode blockIndex)
                    indent <- indent - 1

                let addLongForm instruction =
                    match instruction with 
                        | Block(b,a)    -> subList "Block" b a
                        | Loop(b,a)     -> subList "Loop" b a
                        | If(b,a)       -> subList "If" b a
                        | IfElse(b,i,e) -> 
                            subList "If..." b i
                            subList "...Else" b e
                        | _ -> ()

                let addInstruction instruction =
                    if isShortForm instruction
                    then addLine (sprintf "%A" instruction)
                    else addLongForm instruction

                instructionsArray |> Array.iter addInstruction

            addInstructions instructionsArray

        // CODE

        let addCodeArray codeArray =

            let addCodeDetail i (c:Code) =
                Text ""
                Text (sprintf "--- Code instance [%d] of %A bytes ---" i (c.CodeSize))
                addLocals c.Function.Locals
                addBody c.Function.Body

            codeArray |> Array.iteri addCodeDetail 

        match optionalCodeSec with
            | Some(WasmCodeSec(codeArray)) -> 
                addCodeArray codeArray
                ()
            | _ -> ()

    let AddModule theModule =
        AddArraySection theModule.Custom1   
        AddGeneric theModule.Types
        AddArraySection theModule.Custom2   
        AddGeneric theModule.Imports
        AddArraySection theModule.Custom3   
        AddGeneric theModule.Funcs
        AddArraySection theModule.Custom4   
        AddGeneric theModule.Tables
        AddArraySection theModule.Custom5   
        AddGeneric theModule.Mems
        AddArraySection theModule.Custom6   
        AddGeneric theModule.Globals
        AddArraySection theModule.Custom7   
        AddGeneric theModule.Exports
        AddArraySection theModule.Custom8   
        AddGeneric theModule.Start
        AddArraySection theModule.Custom9   
        AddGeneric theModule.Elems
        AddArraySection theModule.Custom10  
        AddCodeSec theModule.Codes
        AddArraySection theModule.Custom11  
        AddGeneric theModule.Datas
        AddArraySection theModule.Custom12  

    Text "Unit test serialisation for: "
    Text fileName
    AddModule m
    sb.ToString ()
      

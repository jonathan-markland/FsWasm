module UnitTestSerialiser

open System.Text
open FsWasmLibrary.Wasm

let ModuleToUnitTestString (fileName:string) (m:Module) =

    let sb = new StringBuilder ()

    let NewLine () =
        sb.AppendLine() |> ignore

    let Add (t:string) = 
        sb.Append t |> ignore

    let Text (t:string) = 
        sb.Append t |> ignore
        sb.AppendLine "" |> ignore

    let Title (t:string) =
        sb.AppendLine "" |> ignore
        sb.AppendLine "" |> ignore
        sb.Append "--- " |> ignore
        sb.Append t |> ignore
        sb.AppendLine " ---" |> ignore
        sb.AppendLine "" |> ignore

    let AddObject obj =
        sb.Append (sprintf "%A" obj) |> ignore

    let AddOptionalObject optionalObj =
        match optionalObj with
            | None -> ()
            | Some(X) -> AddObject X

    let AddArraySection sectionTitle sectionArray =
        sectionArray |> Array.iteri (fun i X -> 
            Title (sprintf "%s [%d]" sectionTitle i)
            AddObject X)

    let AddGenericSection sectionTitle genericSection =
        Title (sprintf "%s" sectionTitle)
        AddOptionalObject genericSection

    // TYPE SECTION

    let PrettyValType = 
        function
        | I32Type -> "I32"
        | I64Type -> "I64"
        | F32Type -> "F32"
        | F64Type -> "F64"

    let AddValType vt =
        Add (PrettyValType vt)
        Add " "

    let AddValTypes vts =
        vts |> Array.iter AddValType

    let AddFuncType ft =
        AddValTypes ft.ParameterTypes
        Add "-> "
        AddValTypes ft.ReturnTypes

    let AddTypeSecEntry i ft =
        Add (sprintf "TypeSec[%d] = " i)
        AddFuncType ft

    let AddTypeSec (optionalTypeSec:TypeSec option) =

        Title "Types section"

        let addArray a =
            a |> Array.iteri 
                (fun i ft ->
                    AddTypeSecEntry i ft
                    NewLine ())

        match optionalTypeSec with
            | Some(TypeSec(a)) -> addArray a
            | _ -> ()
        
    // FUNC SECTION  (which we indirect through the TypeSec to show the signatures)

    let AddFuncSecEntry i ti (fta:FuncType array) =
        Add (sprintf "FuncSec[%d] => " i)
        match ti with
            | TypeIdx(U32(j)) when int j < fta.Length -> 
                AddTypeSecEntry (int j) fta.[int j]
            | _ -> Add "Error: Out of range type index"

    let AddFuncSec optionalFuncSec optionalTypeSec =

        Title "Funcs section"

        let addArray (tia, fta) =
            tia |> Array.iteri 
                (fun i ti ->
                    AddFuncSecEntry i ti fta
                    NewLine ())

        match optionalFuncSec, optionalTypeSec with
            | Some(FuncSec(a)), Some(TypeSec(b)) -> addArray (a,b)
            | Some(FuncSec(_)), _ -> Text "Error: Missing TypeSec but FuncSec present"
            | _ -> ()
        

    // CODE SECTION

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
                    | U32(repeatCounter) -> addIndividualLocal repeatCounter locals.LocalsType

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
                Title (sprintf "Code section [%d] of %A bytes" i (c.CodeSize))
                addLocals c.Function.Locals
                addBody c.Function.Body

            codeArray |> Array.iteri addCodeDetail 

        match optionalCodeSec with
            | Some(CodeSec(codeArray)) -> addCodeArray codeArray
            | _ -> ()

    let AddModule theModule =

        AddArraySection "Custom section #1"  theModule.Custom1   
        AddTypeSec theModule.Types
        // AddGenericSection "Types section" theModule.Types
        AddArraySection "Custom section #2"  theModule.Custom2   
        AddGenericSection "Imports section" theModule.Imports
        AddArraySection "Custom section #3"  theModule.Custom3   
        AddFuncSec theModule.Funcs theModule.Types
        //AddGenericSection "Funcs section" theModule.Funcs
        AddArraySection "Custom section #4"  theModule.Custom4   
        AddGenericSection "Tables section" theModule.Tables
        AddArraySection "Custom section #5"  theModule.Custom5   
        AddGenericSection "Mems section" theModule.Mems
        AddArraySection "Custom section #6"  theModule.Custom6   
        AddGenericSection "Globals section" theModule.Globals
        AddArraySection "Custom section #7"  theModule.Custom7   
        AddGenericSection "Exports section" theModule.Exports
        AddArraySection "Custom section #8"  theModule.Custom8   
        AddGenericSection "Start section" theModule.Start
        AddArraySection "Custom section #9"  theModule.Custom9   
        AddGenericSection "Elems section" theModule.Elems
        AddArraySection "Custom section #10"  theModule.Custom10  
        AddCodeSec theModule.Codes
        AddArraySection "Custom section #11"  theModule.Custom11  
        AddGenericSection "Data section" theModule.Datas
        AddArraySection "Custom section #12"  theModule.Custom12  

    Title ("Unit test serialisation for: " + fileName)
    AddModule m
    sb.ToString ()

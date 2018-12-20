﻿module UnitTestSerialiser

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

    // COMMON

    let AddLimits l =
        match l with
            | { LimitMin=U32(m); LimitMax=None }         -> Add (sprintf "[|%d|]" m)
            | { LimitMin=U32(a); LimitMax=Some(U32(b)) } -> Add (sprintf "[|%d..%d|]" a b)

    let PrettyMutability =
        function 
        | Constant -> "const"
        | Variable -> "var"

    let PrettyValType = 
        function
        | I32Type -> "I32"
        | I64Type -> "I64"
        | F32Type -> "F32"
        | F64Type -> "F64"

    // TYPE SECTION

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

    let AddTypeSec tso =

        Title "Types section"

        match tso with

            | Some(TypeSec(a)) -> 
                a |> Array.iteri 
                    (fun i ft ->
                        AddTypeSecEntry i ft
                        NewLine ())

            | _ -> ()
        
    // FUNC SECTION  (which we indirect through the TypeSec to show the signatures)

    let AddFuncSecEntry i ti tso =

        Add (sprintf "FuncSec[%d] => " i)
    
        match ti,tso with

            | TypeIdx(U32(j)), Some(TypeSec(fta)) 
                when int j < fta.Length -> 
                    AddTypeSecEntry (int j) fta.[int j]

            | _ -> Add "Error: Out of range type index, or missing TypeSec"



    let AddFuncSec fso tso =

        Title "Funcs section"

        match fso with

            | Some(FuncSec(a)) -> 
                a |> Array.iteri (fun i ti ->
                    AddFuncSecEntry i ti tso
                    NewLine ())

            | _ -> ()
        

    // BODY

    let AddBody instructionsArray =

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


    // CODE SECTION

    let AddCodeSec cso =

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

        // CODE

        match cso with

            | Some(CodeSec(codeArray)) ->

                let addCodeDetail i (c:Code) =
                    Title (sprintf "Code section [%d] of %A bytes" i (c.CodeSize))
                    addLocals c.Function.Locals
                    AddBody c.Function.Body

                codeArray |> Array.iteri addCodeDetail 

            | _ -> ()

    // TABLE SECTION

    let AddTableType (t:TableType) =
        Add (sprintf "%A " t.TableElementType)
        AddLimits t.TableLimits

    let AddTableSecEntry i (ti:Table) =
        Add (sprintf "TableSec[%d] => " i)
        match ti with
            | {TableType=t} -> AddTableType t

    let AddTableSec optionalTableSec =

        Title "Tables section"

        match optionalTableSec with

            | Some(TableSec(a)) ->
                a |> Array.iteri 
                    (fun i ti ->
                        AddTableSecEntry i ti
                        NewLine ())

            | _ -> ()

    // MEMS SECTION

    let AddMemType (t:MemoryType) =
        AddLimits t.MemoryLimits

    let AddMemSecEntry i (ti:Mem) =
        Add (sprintf "MemSec[%d] => " i)
        match ti with
            | {MemType=t} -> AddMemType t

    let AddMemSec optionalMemSec =

        Title "Mems section"

        match optionalMemSec with

            | Some(MemSec(a)) ->
                a |> Array.iteri 
                    (fun i ti ->
                        AddMemSecEntry i ti
                        NewLine ())

            | _ -> ()

    // GLOBAL SECTION

    let AddGlobalType (t:GlobalType) =
        Add (PrettyMutability t.GlobalMutability)
        Add ":"
        Add (PrettyValType t.GlobalType)

    let AddGlobalTypeAndInstructions (t:GlobalType) (e:InstructionArray) =
        AddGlobalType t
        NewLine ()
        AddBody e
        NewLine ()
        NewLine ()

    let AddGlobalSecEntry i (ti:Global) =
        Add (sprintf "GlobalSec[%d] => " i)
        match ti with
            | {GlobalType=t; InitExpr=e} -> AddGlobalTypeAndInstructions t e

    let AddGlobalSec gso =

        Title "Globals section"

        match gso with

            | Some(GlobalSec(a)) ->
                a |> Array.iteri 
                    (fun i ti ->
                        AddGlobalSecEntry i ti
                        NewLine ())

            | _ -> ()


    // IMPORT SECTION

    let AddImportSecEntry i ti fso tso tao meo glo =

        Add (sprintf "ImportSec[%d] => " i)
        
        match ti with
            | {ImportModuleName=m; ImportName=n; ImportDesc=d} -> 
                Add (sprintf "from %s import %s == " m n)
                match d with

                    | ImportFunc(TypeIdx(U32(ti))) -> 
                        match tso with
                            | Some(TypeSec(ts)) -> AddTypeSecEntry (int ti) ts.[(int ti)]
                            | _ -> Add "Cannot show imported function because TypeSec is missing."

                    | ImportTable(tt)  -> AddTableType tt
                    | ImportMemory(mt) -> AddMemType mt
                    | ImportGlobal(gt) -> AddGlobalType gt 

    let AddImportSec imo fso tso tao meo glo =

        Title "Imports section"

        match imo with
            | Some(ImportSec(a)) ->
                a |> Array.iteri 
                    (fun i ti ->
                        AddImportSecEntry i ti fso tso tao meo glo
                        NewLine ())
            | _ -> ()

    // EXPORT SECTION

    let AddExportSecEntry i ti fso tso tao meo glo =

        Add (sprintf "ExportSec[%d] => " i)
        
        match ti with
            | {ExportName=n; ExportDesc=d} -> 
                Add (sprintf "%s == " n)
                match d with

                    | ExportFunc(FuncIdx(U32(fi))) -> 
                        match fso with
                            | Some(FuncSec(fs)) -> AddFuncSecEntry (int fi) fs.[(int fi)] tso
                            | _ -> Add "Cannot show exported function because FuncSec is missing."

                    | ExportTable(TableIdx(U32(ti))) -> 
                        match tao with
                            | Some(TableSec(ts)) -> AddTableSecEntry (int ti) ts.[(int ti)]
                            | _ -> Add "Cannot show exported table because TableSec is missing."

                    | ExportMemory(MemIdx(U32(mi))) -> 
                        match meo with
                            | Some(MemSec(ms)) -> AddMemSecEntry (int mi) ms.[(int mi)]
                            | _ -> Add "Cannot show exported memory because MemSec is missing."

                    | ExportGlobal(GlobalIdx(U32(gi))) -> 
                        match glo with
                            | Some(GlobalSec(gs)) -> AddGlobalSecEntry (int gi) gs.[(int gi)]
                            | _ -> Add "Cannot show exported global because GlobalSec is missing."

    let AddExportSec exo fso tso tao meo glo =

        Title "Exports section"

        match exo with
            | Some(ExportSec(a)) -> 
                a |> Array.iteri 
                    (fun i ti ->
                        AddExportSecEntry i ti fso tso tao meo glo
                        NewLine ())
            | _ -> ()

    // START

    let AddStartSec sto tso =

        Title "Start section"

        match sto with
            | Some(StartSec({ StartFuncIdx=FuncIdx(U32(sfi)) })) -> 
                match tso with
                    | Some(TypeSec(ts)) -> AddTypeSecEntry (int sfi) ts.[(int sfi)]
                    | _ -> Add "Cannot show start function because TypeSec is missing."
            | _ -> ()

    // ELEM SECTION

    let AddElemSecEntry i ti =

        Add (sprintf "ElemSec[%d] => " i)
        
        match ti with

            | { TableIndex=TableIdx(U32(ti)); OffsetExpr=e; Init=fa } ->
                
                Text (sprintf "In table %d, expr =" ti)
                AddBody e
                fa |> Array.iteri (fun i fidx -> 
                        match fidx with 
                            | FuncIdx(U32(fi)) -> 
                                Add (sprintf "Table[%d][expr+%d] = FuncSec[%d]" ti i fi))

    let AddElemSec eso =

        Title "Elems section"

        match eso with
            | Some(ElemSec(a)) -> 
                a |> Array.iteri 
                    (fun i ti ->
                        AddElemSecEntry i ti
                        NewLine ())
            | _ -> ()


    // MODULE        

    let AddModule theModule =

        AddArraySection "Custom section #1"  theModule.Custom1   
        AddTypeSec theModule.Types  // AddGenericSection "Types section" theModule.Types

        AddArraySection "Custom section #2"  theModule.Custom2   
        AddImportSec theModule.Imports theModule.Funcs theModule.Types theModule.Tables theModule.Mems theModule.Globals// AddGenericSection "Imports section" theModule.Imports

        AddArraySection "Custom section #3"  theModule.Custom3   
        AddFuncSec theModule.Funcs theModule.Types //AddGenericSection "Funcs section" theModule.Funcs

        AddArraySection "Custom section #4"  theModule.Custom4   
        AddTableSec theModule.Tables  // AddGenericSection "Tables section" theModule.Tables

        AddArraySection "Custom section #5"  theModule.Custom5   
        AddMemSec theModule.Mems  // AddGenericSection "Mems section" theModule.Mems

        AddArraySection "Custom section #6"  theModule.Custom6   
        AddGlobalSec theModule.Globals  // AddGenericSection "Globals section" theModule.Globals

        AddArraySection "Custom section #7"  theModule.Custom7   
        AddExportSec theModule.Exports theModule.Funcs theModule.Types theModule.Tables theModule.Mems theModule.Globals  // AddGenericSection "Exports section" theModule.Exports

        AddArraySection "Custom section #8"  theModule.Custom8   
        AddStartSec theModule.Start theModule.Types  // AddGenericSection "Start section" theModule.Start

        AddArraySection "Custom section #9"  theModule.Custom9   
        AddElemSec theModule.Elems  // AddGenericSection "Elems section" theModule.Elems

        AddArraySection "Custom section #10"  theModule.Custom10  
        AddCodeSec theModule.Codes

        AddArraySection "Custom section #11"  theModule.Custom11  
        AddGenericSection "Data section" theModule.Datas

        AddArraySection "Custom section #12"  theModule.Custom12  

    Title ("Unit test serialisation for: " + fileName)
    AddModule m
    sb.ToString ()
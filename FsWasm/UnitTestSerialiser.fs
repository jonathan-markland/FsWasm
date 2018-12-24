module UnitTestSerialiser

open System.Text
open Wasm

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

    let SingleLineFormatted obj =
        (sprintf "%A" obj)
            .Replace("\r", " ")
            .Replace("\n", " ")
            .Replace("  ", " ")
            .Replace("  ", " ")   // TODO: hackish
            .Replace("  ", " ")

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

    let AddByteArray a leftSideString =
        
        a |> Array.iteri (fun i b -> 
            if (i &&& 15) = 0 then Add leftSideString
            Add (sprintf "%02x " b)
            if (i &&& 15) = 15 then Text "")

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

    let AddTypeSecEntry i (types:FuncType[]) =
        Add (sprintf "TypeSec[%d] = " i)
        if i >= types.Length then Add "Error: TypeSec index out of range"
        else AddFuncType types.[i]

    let AddTypeSec types =
        Title "Types section"
        types |> Array.iteri 
            (fun i _ ->
                AddTypeSecEntry i types
                NewLine ())
        
    // FUNC SECTION  (which we indirect through the TypeSec to show the signatures)

    let AddFuncSecEntry i (funcs:TypeIdx[]) (types:FuncType[]) =
        Add (sprintf "FuncSec[%d] => " i)
        if i >= funcs.Length then Add "Error: Out of range FuncSec index"
        else 
            match funcs.[i] with
                | TypeIdx(U32(j)) -> AddTypeSecEntry (int j) types

    let AddFuncSec types funcs =
        Title "Funcs section"
        funcs |> Array.iteri (fun i _ ->
            AddFuncSecEntry i funcs types
            NewLine ())
        

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
                then addLine (SingleLineFormatted instruction)
                else addLongForm instruction

            instructionsArray |> Array.iter addInstruction

        addInstructions instructionsArray


    // CODE SECTION

    let AddCodeSec cso =

        Title "Code section"

        // LOCALS

        let addLocals localsArray =

            let mutable localIndex = 0

            let rec addIndividualLocal repeatCounter t =
                if repeatCounter >= 1u then
                    Text (sprintf "        Local[%d] %A" localIndex t)
                    localIndex <- localIndex + 1
                    addIndividualLocal (repeatCounter - 1u) t

            let addLocals (locals:Locals) =
                match locals.NumRepeats with
                    | U32(repeatCounter) -> addIndividualLocal repeatCounter locals.LocalsType
                NewLine ()

            localsArray |> Array.iter addLocals

        // CODE

        let addCodeDetail i (c:Code) =
            NewLine ()
            NewLine ()
            Text (sprintf "CodeSec[%d]  (%d bytes)" i (match c with { CodeSize=U32(n) } -> n))
            NewLine ()
            addLocals c.Function.Locals
            AddBody c.Function.Body

        cso |> Array.iteri addCodeDetail 

    // TABLE SECTION

    let AddTableType (t:TableType) =
        Add (sprintf "%A " t.TableElementType)
        AddLimits t.TableLimits

    let AddTableSecEntry i (tables:Table[]) =
        Add (sprintf "TableSec[%d] => " i)
        if i >= tables.Length then Add "Error: Tablesec index out of range"
        else
            match tables.[i] with
                | {TableType=t} -> AddTableType t

    let AddTableSec tables =
        Title "Tables section"
        tables |> Array.iteri 
            (fun i _ ->
                AddTableSecEntry i tables
                NewLine ())

    // MEMS SECTION

    let AddMemType (t:MemoryType) =
        AddLimits t.MemoryLimits
        Add " x 64KB"

    let AddMemSecEntry i (mems:Mem[]) =
        Add (sprintf "MemSec[%d] => " i)
        if i >= mems.Length then Add "Error: MemSec index out of range"
        else
            match mems.[i] with
                | {MemType=t} -> AddMemType t

    let AddMemSec mems =
        Title "Mems section"
        mems |> Array.iteri 
            (fun i _ ->
                AddMemSecEntry i mems
                NewLine ())

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

    let AddGlobalSecEntry i (globals:Global[]) =
        Add (sprintf "GlobalSec[%d] => " i)
        if i >= globals.Length then Add "Error: GlobalSec index out of range"
        else
            match globals.[i] with
                | {GlobalType=t; InitExpr=e} -> AddGlobalTypeAndInstructions t e

    let AddGlobalSec globals =
        Title "Globals section"
        globals |> Array.iteri 
            (fun i _ ->
                AddGlobalSecEntry i globals
                NewLine ())


    // IMPORT SECTION

    let AddImportSecEntry i (imports:Import[]) (types:FuncType[]) =
        Add (sprintf "ImportSec[%d] => " i)
        if i >= imports.Length then Add "Error: ImportSec index out of range."
        else
            match imports.[i] with
                | {ImportModuleName=m; ImportName=n; ImportDesc=d} -> 
                    Add (sprintf "from '%s' import '%s' == " m n)
                    match d with
                        | ImportFunc(TypeIdx(U32(ti))) -> AddTypeSecEntry (int ti) types
                        | ImportTable(tt)  -> AddTableType tt
                        | ImportMemory(mt) -> AddMemType mt
                        | ImportGlobal(gt) -> AddGlobalType gt 

    let AddImportSec types imports =
        Title "Imports section"
        imports |> Array.iteri 
            (fun i _ ->
                AddImportSecEntry i imports types
                NewLine ())

    // EXPORT SECTION

    let AddExportSecEntry i (exports:Export[]) (functions:TypeIdx[]) (types:FuncType[]) (tables:Table[]) (mems:Mem[]) (globals:Global[]) =
        Add (sprintf "ExportSec[%d] => " i)
        if (i >= exports.Length) then Add "Error: ExportSec index out of range"
        else
            match exports.[i] with
                | {ExportName=n; ExportDesc=d} -> 
                    Add (sprintf "%s == " n)
                    match d with
                        | ExportFunc(FuncIdx(U32(fi)))     -> AddFuncSecEntry   (int fi) functions types
                        | ExportTable(TableIdx(U32(ti)))   -> AddTableSecEntry  (int ti) tables
                        | ExportMemory(MemIdx(U32(mi)))    -> AddMemSecEntry    (int mi) mems
                        | ExportGlobal(GlobalIdx(U32(gi))) -> AddGlobalSecEntry (int gi) globals

    let AddExportSec functions types tables mems globals exports =
        Title "Exports section"
        exports |> Array.iteri 
            (fun i _ ->
                AddExportSecEntry i exports functions types tables mems globals
                NewLine ())

    // START

    let AddStartSec (types:FuncType[]) optionalStartSec =
        Title "Start section"
        match optionalStartSec with
            | Some({ StartFuncIdx=FuncIdx(U32(sfi)) }) -> 
                AddTypeSecEntry (int sfi) types
            | None -> ()

    // ELEM SECTION

    let AddElemSecEntry i (elems:Elem[]) =
        Add (sprintf "ElemSec[%d] => " i)
        if i >= elems.Length then Add "Error: ElemSec index out of range"
        else 
            match elems.[i] with
                | { TableIndex=TableIdx(U32(ti)); OffsetExpr=e; Init=fa } ->
                    Text (sprintf "In table %d, expr =" ti)
                    AddBody e
                    fa |> Array.iteri (fun i fidx -> 
                        match fidx with 
                            | FuncIdx(U32(fi)) -> 
                                Add (sprintf "Table[%d][expr+%d] = FuncSec[%d]" ti i fi))

    let AddElemSec elems =
        Title "Elems section"
        elems |> Array.iteri 
            (fun i _ ->
                AddElemSecEntry i elems
                NewLine ())

    // DATA SECTION

    let AddDataSecEntry i (datas:Data[]) =
        Add (sprintf "DataSec[%d] => " i)
        if i >= datas.Length then Add "Error: DataSec index out of range"
        else
            match datas.[i] with
                | { DataMemoryIndex=MemIdx(U32(mi)); OffsetExpr=e; InitImageBytes=imageData } ->
                    Text (sprintf "Fill memory[%d], where the offset-expr and image-data are as follows:" mi)
                    AddBody e
                    AddByteArray imageData "      | "
                    NewLine ()

    let AddDataSec datas =
        Title "Data section"
        datas |> Array.iteri 
            (fun i _ ->
                AddDataSecEntry i datas
                NewLine ())


    // MODULE        

    let AddModule theModule =

        theModule.Custom1 |> AddArraySection "Custom section #1"     
        theModule.Types |> AddTypeSec 

        theModule.Custom2 |> AddArraySection "Custom section #2"     
        theModule.Imports |> AddImportSec theModule.Types

        theModule.Custom3 |> AddArraySection "Custom section #3"     
        theModule.Funcs |> AddFuncSec theModule.Types

        theModule.Custom4 |> AddArraySection "Custom section #4"     
        theModule.Tables |> AddTableSec 

        theModule.Custom5 |> AddArraySection "Custom section #5"     
        theModule.Mems |> AddMemSec 

        theModule.Custom6 |> AddArraySection "Custom section #6"     
        theModule.Globals |> AddGlobalSec 

        theModule.Custom7 |> AddArraySection "Custom section #7"     
        theModule.Exports |> AddExportSec theModule.Funcs theModule.Types theModule.Tables theModule.Mems theModule.Globals

        theModule.Custom8 |> AddArraySection "Custom section #8"     
        theModule.Start |> AddStartSec theModule.Types

        theModule.Custom9 |> AddArraySection "Custom section #9"     
        theModule.Elems |> AddElemSec

        theModule.Custom10 |> AddArraySection "Custom section #10"    
        theModule.Codes |> AddCodeSec 

        theModule.Custom11 |> AddArraySection "Custom section #11"    
        theModule.Datas |> AddDataSec 

        theModule.Custom12 |> AddArraySection "Custom section #12"    

    Title ("Unit test serialisation for: " + fileName)
    m |> AddModule 
    sb.ToString ()

module UnitTestSerialiser

open System.Text
open FsWasmLibrary.Wasm
open FsWasmLibrary

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

    let AddByteArray a =
        
        a |> Array.iteri (fun i b -> 
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

    let AddTypeSecEntry i ft =
        Add (sprintf "TypeSec[%d] = " i)
        AddFuncType ft

    let AddTypeSec ts =
        Title "Types section"
        ts |> Array.iteri 
            (fun i ft ->
                AddTypeSecEntry i ft
                NewLine ())
        
    // FUNC SECTION  (which we indirect through the TypeSec to show the signatures)

    let AddFuncSecEntry i ti (ts:FuncType[]) =
        Add (sprintf "FuncSec[%d] => " i)
        match ti with
            | TypeIdx(U32(j))
                when int j < ts.Length -> 
                    AddTypeSecEntry (int j) ts.[int j]
            | _ -> Add "Error: Out of range type index, or missing TypeSec"

    let AddFuncSec fs ts =
        Title "Funcs section"
        fs |> Array.iteri (fun i ti ->
            AddFuncSecEntry i ti ts
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

        let addCodeDetail i (c:Code) =
            Title (sprintf "Code section [%d] of %A bytes" i (c.CodeSize))
            addLocals c.Function.Locals
            AddBody c.Function.Body

        cso |> Array.iteri addCodeDetail 

    // TABLE SECTION

    let AddTableType (t:TableType) =
        Add (sprintf "%A " t.TableElementType)
        AddLimits t.TableLimits

    let AddTableSecEntry i (thisTable:Table) =
        Add (sprintf "TableSec[%d] => " i)
        match thisTable with
            | {TableType=t} -> AddTableType t

    let AddTableSec tables =
        Title "Tables section"
        tables |> Array.iteri 
            (fun i ti ->
                AddTableSecEntry i ti
                NewLine ())

    // MEMS SECTION

    let AddMemType (t:MemoryType) =
        AddLimits t.MemoryLimits
        Add " x 64KB"

    let AddMemSecEntry i (thisMem:Mem) =
        Add (sprintf "MemSec[%d] => " i)
        match thisMem with
            | {MemType=t} -> AddMemType t

    let AddMemSec mems =
        Title "Mems section"
        mems |> Array.iteri 
            (fun i ti ->
                AddMemSecEntry i ti
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

    let AddGlobalSecEntry i (thisGlobal:Global) =
        Add (sprintf "GlobalSec[%d] => " i)
        match thisGlobal with
            | {GlobalType=t; InitExpr=e} -> AddGlobalTypeAndInstructions t e

    let AddGlobalSec globals =
        Title "Globals section"
        globals |> Array.iteri 
            (fun i thisGlobal ->
                AddGlobalSecEntry i thisGlobal
                NewLine ())


    // IMPORT SECTION

    let AddImportSecEntry i thisImport (types:FuncType[]) =
        Add (sprintf "ImportSec[%d] => " i)
        match thisImport with
            | {ImportModuleName=m; ImportName=n; ImportDesc=d} -> 
                Add (sprintf "from %s import %s == " m n)
                match d with
                    | ImportFunc(TypeIdx(U32(ti))) -> AddTypeSecEntry (int ti) types.[(int ti)]
                    | ImportTable(tt)  -> AddTableType tt
                    | ImportMemory(mt) -> AddMemType mt
                    | ImportGlobal(gt) -> AddGlobalType gt 

    let AddImportSec imports types =
        Title "Imports section"
        imports |> Array.iteri 
            (fun i thisImport ->
                AddImportSecEntry i thisImport types
                NewLine ())

    // EXPORT SECTION

    let AddExportSecEntry i ti (functions:TypeIdx[]) (types:FuncType[]) (tables:Table[]) (mems:Mem[]) (globals:Global[]) =
        Add (sprintf "ExportSec[%d] => " i)
        match ti with
            | {ExportName=n; ExportDesc=d} -> 
                Add (sprintf "%s == " n)
                match d with
                    | ExportFunc(FuncIdx(U32(fi)))     -> AddFuncSecEntry   (int fi) functions.[(int fi)] types
                    | ExportTable(TableIdx(U32(ti)))   -> AddTableSecEntry  (int ti) tables.[(int ti)]
                    | ExportMemory(MemIdx(U32(mi)))    -> AddMemSecEntry    (int mi) mems.[(int mi)]
                    | ExportGlobal(GlobalIdx(U32(gi))) -> AddGlobalSecEntry (int gi) globals.[(int gi)]

    let AddExportSec exports functions types tables mems globals =
        Title "Exports section"
        exports |> Array.iteri 
            (fun i ti ->
                AddExportSecEntry i ti functions types tables mems globals
                NewLine ())

    // START

    let AddStartSec sto (types:FuncType[]) =
        Title "Start section"
        match sto with
            | Some({ StartFuncIdx=FuncIdx(U32(sfi)) }) -> 
                AddTypeSecEntry (int sfi) types.[(int sfi)]
            | None -> ()

    // ELEM SECTION

    let AddElemSecEntry i thisElem =
        Add (sprintf "ElemSec[%d] => " i)
        match thisElem with
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
            (fun i thisElem ->
                AddElemSecEntry i thisElem
                NewLine ())

    // DATA SECTION

    let AddDataSecEntry i thisData =
        Add (sprintf "DataSec[%d] => " i)
        match thisData with
            | { DataMemoryIndex=MemIdx(U32(mi)); OffsetExpr=e; InitImageBytes=imageData } ->
                Text (sprintf "Memory [%d] offset expr =" mi)
                AddBody e
                AddByteArray imageData

    let AddDataSec datas =
        Title "Data section"
        datas |> Array.iteri 
            (fun i thisData ->
                AddDataSecEntry i thisData
                NewLine ())


    // MODULE        

    let AddModule theModule =

        let convenientTables = theModule |> WasmAlgorithms.GetConvenientLookupTables  // TODO: use to fix references

        AddArraySection "Custom section #1"  theModule.Custom1   
        AddTypeSec theModule.Types

        AddArraySection "Custom section #2"  theModule.Custom2   
        AddImportSec theModule.Imports theModule.Types

        AddArraySection "Custom section #3"  theModule.Custom3   
        AddFuncSec theModule.Funcs theModule.Types

        AddArraySection "Custom section #4"  theModule.Custom4   
        AddTableSec theModule.Tables

        AddArraySection "Custom section #5"  theModule.Custom5   
        AddMemSec theModule.Mems

        AddArraySection "Custom section #6"  theModule.Custom6   
        AddGlobalSec theModule.Globals

        AddArraySection "Custom section #7"  theModule.Custom7   
        AddExportSec theModule.Exports theModule.Funcs theModule.Types theModule.Tables theModule.Mems theModule.Globals

        AddArraySection "Custom section #8"  theModule.Custom8   
        AddStartSec theModule.Start theModule.Types

        AddArraySection "Custom section #9"  theModule.Custom9   
        AddElemSec theModule.Elems

        AddArraySection "Custom section #10"  theModule.Custom10  
        AddCodeSec theModule.Codes

        AddArraySection "Custom section #11"  theModule.Custom11  
        AddDataSec theModule.Datas

        AddArraySection "Custom section #12"  theModule.Custom12  

    Title ("Unit test serialisation for: " + fileName)
    AddModule m
    sb.ToString ()

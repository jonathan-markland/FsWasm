module WasmToWasm2

open Wasm
open Wasm2
open WasmAlgorithms


// -------------------------------------------------------------------------------------------------
//  Library
// -------------------------------------------------------------------------------------------------


let FindExport (wasmModule:Module) desc : Export2 option =

    let oldExportOpt = wasmModule.Exports |> Array.tryFind (fun exp ->
        match exp with
            | { ExportName=n; ExportDesc=d } when desc=d -> true
            | _ -> false)

    match oldExportOpt with
        | Some({ ExportName=n; ExportDesc=_ }) -> Some({ExportName=n})
        | _ -> None



let GetFuncTypeFromTypeIdx typeIdx (wasmModule:Module) =
    wasmModule.Types.[match typeIdx with TypeIdx(U32(i)) -> (int i)]


// -------------------------------------------------------------------------------------------------
//  Harvest from IMPORTs
// -------------------------------------------------------------------------------------------------



let HarvestTable2sFromImports (wasmModule:Module) =

    let mutable exportIndex = 0u

    wasmModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportTable(tableType)} 
                -> let exportOpt = FindExport wasmModule (ExportTable(TableIdx(U32(exportIndex))))
                   let import2 = { Export2=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedTable2({Import2=import2; TableType=tableType}))
            | _ -> None)


let HarvestMemory2sFromImports (wasmModule:Module) =

    let mutable exportIndex = 0u

    wasmModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportMemory(memoryType)} 
                -> let exportOpt = FindExport wasmModule (ExportMemory(MemIdx(U32(exportIndex))))
                   let import2 = { Export2=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedMemory2({Import2=import2; MemoryType=memoryType}))
            | _ -> None)


let HarvestGlobal2sFromImports (wasmModule:Module) =

    let mutable exportIndex = 0u

    wasmModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportGlobal(globalType)} 
                -> let exportOpt = FindExport wasmModule (ExportGlobal(GlobalIdx(U32(exportIndex))))
                   let import2 = { Export2=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedGlobal2({Import2=import2; GlobalType=globalType}))
            | _ -> None)


let HarvestFunction2sFromImports (wasmModule:Module) =

    let mutable exportIndex = 0u

    wasmModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportFunc(typeIdx)} 
                -> let exportOpt = FindExport wasmModule (ExportFunc(FuncIdx(U32(exportIndex))))
                   let import2 = { Export2=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   let funcType = wasmModule |> GetFuncTypeFromTypeIdx typeIdx
                   Some(ImportedFunction2({Import2=import2; FuncType=funcType}))
            | _ -> None)


// -------------------------------------------------------------------------------------------------
//  Harvest from respective sections
// -------------------------------------------------------------------------------------------------


let GetElemSecForTable (tableIndex:TableIdx) (wasmModule:Module) : Elem option =
    wasmModule.Elems |> Array.tryFind (fun elem -> elem.TableIndex = tableIndex)


let TranslateFuncIndexUnresolved (wasmModule:Module) (funcIndex:FuncIdx) : Function2 =
    FunctionNotYetResolved(funcIndex)


let TranslateFuncIndexArrayUnresolved wasmModule funcIndexArray =
    funcIndexArray |> Array.map (TranslateFuncIndexUnresolved wasmModule)


let rec TranslateInstr (wasmModule:Module) (ins:Instr) : Instr2 =
    Unreachable // TODO: sort out


and TranslateInstrArray wasmModule insArray =
    insArray |> Array.map (TranslateInstr wasmModule)




let HarvestInternalTables (newImportedTables:Table2[]) (wasmModule:Module) =

    // There is only max one Table in the Wasm 1.0 spec, but we allow for more.

    let mutable objectIndex = (uint32 newImportedTables.Length)

    wasmModule.Tables |> Array.map (
        fun oldTable -> 
            let exportOpt = FindExport wasmModule (ExportTable(TableIdx(U32(objectIndex))))
            let elemOpt = wasmModule |> GetElemSecForTable (TableIdx(U32(objectIndex)))
            let newInit = 
                match elemOpt with
                    | Some({TableIndex=_; OffsetExpr=oldOffsetExpr; Init=oldFuncIdxArray}) ->
                        let translatedOffsetExpr = oldOffsetExpr |> TranslateInstrArray wasmModule
                        let translatedFuncArray = oldFuncIdxArray |> TranslateFuncIndexArrayUnresolved wasmModule
                        (translatedOffsetExpr, translatedFuncArray)
                    | None -> 
                        ([||], [||])

            objectIndex <- objectIndex + 1u

            InternalTable2({ Export2=exportOpt; 
                TableType=oldTable.TableType; 
                InitOffsetExpr=fst newInit; 
                InitWith=snd newInit})
        )




let HarvestInternalGlobals (newImportedGlobals:Global2[]) (wasmModule:Module) =

    let mutable objectIndex = (uint32 newImportedGlobals.Length)

    wasmModule.Globals |> Array.map (
        fun oldGlobal -> 
            let exportOpt = FindExport wasmModule (ExportGlobal(GlobalIdx(U32(objectIndex))))
            let translatedInitExpr = oldGlobal.InitExpr |> TranslateInstrArray wasmModule
            objectIndex <- objectIndex + 1u
            InternalGlobal2({ Export2=exportOpt; GlobalType=oldGlobal.GlobalType; InitExpr=translatedInitExpr })
        )



let GetAllInitialisationsForThisMem wasmModule thisMemIdx =
    wasmModule.Datas |> Array.choose (fun oldData ->
        match oldData with
            | { DataMemoryIndex=i; OffsetExpr=e; InitImageBytes=bs } when i=thisMemIdx ->
                let translatedOffsetExpr = e |> TranslateInstrArray wasmModule
                Some(translatedOffsetExpr, bs)
            | _ -> None
        )



let HarvestInternalMems (newImportedMems:Memory2[]) (wasmModule:Module) =

    let mutable objectIndex = (uint32 newImportedMems.Length)

    wasmModule.Mems |> Array.map (
        fun oldMem -> 
            let thisMemIdx = MemIdx(U32(objectIndex))
            let exportOpt = FindExport wasmModule (ExportMemory(thisMemIdx))
            let translatedInitData = GetAllInitialisationsForThisMem wasmModule thisMemIdx
            objectIndex <- objectIndex + 1u
            InternalMemory2({ Export2=exportOpt; MemoryType=oldMem.MemType; InitData=translatedInitData })
        )



let GetFuncType (wasmModule:Module) (codeSecIndex:int) =
    match wasmModule.Funcs.[codeSecIndex] with
        | TypeIdx(U32(i)) -> wasmModule.Types.[int i]

    

let HarvestInternalFuncs (newImportedFuncs:Function2[]) (wasmModule:Module) =

    let mutable objectIndex = (uint32 newImportedFuncs.Length)
    let mutable codeSecIndex = 0

    wasmModule.Codes |> Array.map (fun oldCode -> 
        let thisFuncIdx = FuncIdx(U32(objectIndex))
        let exportOpt = FindExport wasmModule (ExportFunc(thisFuncIdx))
        let funcType = GetFuncType wasmModule codeSecIndex
        let translatedBodyWithoutFunctionReferences = 
            oldCode.Function.Body |> TranslateInstrArray wasmModule

        objectIndex <- objectIndex + 1u
        codeSecIndex <- codeSecIndex + 1

        InternalFunction2({ Export2 = exportOpt; 
            OriginalCodeSecIndex = U32(uint32 codeSecIndex); 
            CodeSize = oldCode.CodeSize; 
            FuncType = funcType; 
            Locals = oldCode.Function.Locals; 
            Body = translatedBodyWithoutFunctionReferences })
        )



// -------------------------------------------------------------------------------------------------
//  Main
// -------------------------------------------------------------------------------------------------


let TranslateWasmToWasm2 (wasmModule:Module) =

    let newImportedFuncs = wasmModule |> HarvestFunction2sFromImports 
    let newImportedMems = wasmModule |> HarvestMemory2sFromImports
    let newImportedTables = wasmModule |> HarvestTable2sFromImports
    let newImportedGlobals = wasmModule |> HarvestGlobal2sFromImports

    let newImportedFuncs = wasmModule |> HarvestInternalFuncs newImportedFuncs
    let newInternalMems = wasmModule |> HarvestInternalMems newImportedMems
    let newInternalTables = wasmModule |> HarvestInternalTables newImportedTables
    let newInternalGlobals = wasmModule |> HarvestInternalGlobals newImportedGlobals

    // [ ] Concatenate the arrays

    // [ ] 


    0


    //let lookupTables = wasmModule |> GetConvenientLookupTables

    // [ ] Do not fill in the Body on the first pass of the functions.  Leave empty.  Do later
    //     and return replacements.

    // [ ] Do Memory and Tables first

    // let newStart = { 
    //     StartFunction2=
    //         match wasmModule.Start with
    //             | None -> None
    //             | Some({StartFuncIdx=f}) -> look f up in the newFuncs[] }

    // {
    //     funcs=newFuncs;
    //     mems=newMems;
    //     tables=newTables;
    //     globals=newGlobals;
    //     start=newStart;
    // }

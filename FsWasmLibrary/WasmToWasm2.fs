module WasmToWasm2

open Wasm
open Wasm2
open WasmAlgorithms


// -------------------------------------------------------------------------------------------------
//  Library
// -------------------------------------------------------------------------------------------------


let FindExport (oldModule:Module) desc : Export2 option =

    let oldExportOpt = oldModule.Exports |> Array.tryFind (fun exp ->
        match exp with
            | { ExportName=n; ExportDesc=d } when desc=d -> true
            | _ -> false)

    match oldExportOpt with
        | Some({ ExportName=n; ExportDesc=_ }) -> Some({ExportName=n})
        | _ -> None



let GetFuncTypeFromTypeIdx typeIdx (oldModule:Module) =
    oldModule.Types.[match typeIdx with TypeIdx(U32(i)) -> (int i)]


// -------------------------------------------------------------------------------------------------
//  Harvest from IMPORTs
// -------------------------------------------------------------------------------------------------



let HarvestTable2sFromImports (oldModule:Module) =

    let mutable exportIndex = 0u

    oldModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportTable(tableType)} 
                -> let exportOpt = FindExport oldModule (ExportTable(TableIdx(U32(exportIndex))))
                   let import2 = { Export2=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedTable2({Import2=import2; TableType=tableType}))
            | _ -> None)


let HarvestMemory2sFromImports (oldModule:Module) =

    let mutable exportIndex = 0u

    oldModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportMemory(memoryType)} 
                -> let exportOpt = FindExport oldModule (ExportMemory(MemIdx(U32(exportIndex))))
                   let import2 = { Export2=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedMemory2({Import2=import2; MemoryType=memoryType}))
            | _ -> None)


let HarvestGlobal2sFromImports (oldModule:Module) =

    let mutable exportIndex = 0u

    oldModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportGlobal(globalType)} 
                -> let exportOpt = FindExport oldModule (ExportGlobal(GlobalIdx(U32(exportIndex))))
                   let import2 = { Export2=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedGlobal2({Import2=import2; GlobalType=globalType}))
            | _ -> None)


let HarvestFunction2sFromImports (oldModule:Module) =

    let mutable exportIndex = 0u

    oldModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportFunc(typeIdx)} 
                -> let exportOpt = FindExport oldModule (ExportFunc(FuncIdx(U32(exportIndex))))
                   let import2 = { Export2=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   let funcType = oldModule |> GetFuncTypeFromTypeIdx typeIdx
                   Some(ImportedFunction2({Import2=import2; FuncType=funcType}))
            | _ -> None)


// -------------------------------------------------------------------------------------------------
//  Harvest from respective sections
// -------------------------------------------------------------------------------------------------


let GetElemSecForTable (tableIndex:TableIdx) (oldModule:Module) : Elem option =
    oldModule.Elems |> Array.tryFind (fun elem -> elem.TableIndex = tableIndex)


let rec TranslateInstr (oldModule:Module) (ins:Instr) : Instr2 =
    Unreachable // TODO: sort out


and TranslateInstrArray oldModule insArray =
    insArray |> Array.map (TranslateInstr oldModule)




let HarvestInternalTables (newImportedTables:Table2[]) (oldModule:Module) =

    // There is only max one Table in the Wasm 1.0 spec, but we allow for more.

    let mutable objectIndex = (uint32 newImportedTables.Length)

    oldModule.Tables |> Array.map (
        fun oldTable -> 
            let exportOpt = FindExport oldModule (ExportTable(TableIdx(U32(objectIndex))))
            let elemOpt = oldModule |> GetElemSecForTable (TableIdx(U32(objectIndex)))
            let newInit = 
                match elemOpt with
                    | Some({TableIndex=_; OffsetExpr=oldOffsetExpr; Init=oldFuncIdxArray}) ->
                        let translatedOffsetExpr = oldOffsetExpr |> TranslateInstrArray oldModule
                        (translatedOffsetExpr, oldFuncIdxArray)
                    | None -> 
                        ([||], [||])

            objectIndex <- objectIndex + 1u

            InternalTable2({ Export2=exportOpt; 
                TableType=oldTable.TableType; 
                InitOffsetExpr=fst newInit; 
                InitWith=snd newInit})
        )




let HarvestInternalGlobals (newImportedGlobals:Global2[]) (oldModule:Module) =

    let mutable objectIndex = (uint32 newImportedGlobals.Length)

    oldModule.Globals |> Array.map (
        fun oldGlobal -> 
            let exportOpt = FindExport oldModule (ExportGlobal(GlobalIdx(U32(objectIndex))))
            let translatedInitExpr = oldGlobal.InitExpr |> TranslateInstrArray oldModule
            objectIndex <- objectIndex + 1u
            InternalGlobal2({ Export2=exportOpt; GlobalType=oldGlobal.GlobalType; InitExpr=translatedInitExpr })
        )



let GetAllInitialisationsForThisMem oldModule thisMemIdx =
    oldModule.Datas |> Array.choose (fun oldData ->
        match oldData with
            | { DataMemoryIndex=i; OffsetExpr=e; InitImageBytes=bs } when i=thisMemIdx ->
                let translatedOffsetExpr = e |> TranslateInstrArray oldModule
                Some(translatedOffsetExpr, bs)
            | _ -> None
        )



let HarvestInternalMems (newImportedMems:Memory2[]) (oldModule:Module) =

    let mutable objectIndex = (uint32 newImportedMems.Length)

    oldModule.Mems |> Array.map (
        fun oldMem -> 
            let thisMemIdx = MemIdx(U32(objectIndex))
            let exportOpt = FindExport oldModule (ExportMemory(thisMemIdx))
            let translatedInitData = GetAllInitialisationsForThisMem oldModule thisMemIdx
            objectIndex <- objectIndex + 1u
            InternalMemory2({ Export2=exportOpt; MemoryType=oldMem.MemType; InitData=translatedInitData })
        )



let GetFuncType (oldModule:Module) (codeSecIndex:int) =
    match oldModule.Funcs.[codeSecIndex] with
        | TypeIdx(U32(i)) -> oldModule.Types.[int i]

    

let HarvestInternalFuncs (newImportedFuncs:Function2[]) (oldModule:Module) =

    let mutable objectIndex = (uint32 newImportedFuncs.Length)
    let mutable codeSecIndex = 0

    oldModule.Codes |> Array.map (fun oldCode -> 
        let thisFuncIdx = FuncIdx(U32(objectIndex))
        let exportOpt = FindExport oldModule (ExportFunc(thisFuncIdx))
        let funcType = GetFuncType oldModule codeSecIndex
        let translatedBodyWithoutFunctionReferences = 
            oldCode.Function.Body |> TranslateInstrArray oldModule

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


let TranslateWasmToWasm2 (oldModule:Module) =

    let newImportedFuncs = oldModule |> HarvestFunction2sFromImports 
    let newImportedMems = oldModule |> HarvestMemory2sFromImports
    let newImportedTables = oldModule |> HarvestTable2sFromImports
    let newImportedGlobals = oldModule |> HarvestGlobal2sFromImports

    let newInternalFuncs = oldModule |> HarvestInternalFuncs newImportedFuncs
    let newInternalMems = oldModule |> HarvestInternalMems newImportedMems
    let newInternalTables = oldModule |> HarvestInternalTables newImportedTables
    let newInternalGlobals = oldModule |> HarvestInternalGlobals newImportedGlobals

    let finalFuncs = Array.append newImportedFuncs newInternalFuncs
    let finalMems = Array.append newImportedMems newInternalMems
    let finalTables = Array.append newImportedTables newInternalTables
    let finalGlobals = Array.append newImportedGlobals newInternalGlobals

    // TODO: Expand out the locals list with the repeats, so that the LocalIdx indices work.
    // TODO: Translate the instructions.

    {
        funcs   = finalFuncs;
        mems    = finalMems;
        tables  = finalTables;
        globals = finalGlobals;
        start   = oldModule.Start;
    }

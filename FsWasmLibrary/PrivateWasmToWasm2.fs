module PrivateWasmToWasm2

open Wasm
open Wasm2

// -------------------------------------------------------------------------------------------------
//  Completely Generic Library
// -------------------------------------------------------------------------------------------------

let MakeArrayUsing arrayMaker =
    let container = new ResizeArray<'recordType>()
    arrayMaker (container.Add)
    container.ToArray()
 



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



let GetElemSecForTable (tableIndex:TableIdx) (oldModule:Module) : Elem option =
    oldModule.Elems |> Array.tryFind (fun elem -> elem.TableIndex = tableIndex)



let GetFuncType (oldModule:Module) (codeSecIndex:int) =
    oldModule.Funcs.[codeSecIndex]



let FlattenLocalsToArray (oldLocals:Locals[]) =
    
    let arrayMaker add = oldLocals |> Array.iter (fun item ->
        let { NumRepeats=U32(numRepeats); LocalsType=theType; } = item
        for i in 0u..numRepeats do add theType)

    MakeArrayUsing arrayMaker

    

let GetAllInitialisationsForThisMem oldModule thisMemIdx =
    oldModule.Datas |> Array.choose (fun oldData ->
        match oldData with
            | { DataMemoryIndex=i; OffsetExpr=e; InitImageBytes=bs } when i=thisMemIdx ->
                Some(e, bs)
            | _ -> None
        )



// -------------------------------------------------------------------------------------------------
//  Harvest from IMPORTs
// -------------------------------------------------------------------------------------------------


let HarvestTable2sFromImports (oldModule:Module) =

    let mutable exportIndex = 0u

    oldModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportTable(tableType)} 
                -> let exportOpt = FindExport oldModule (ExportTable(TableIdx(U32(exportIndex))))
                   let import2 = { Export=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedTable2({Import=import2; TableType=tableType}))
            | _ -> None)


let HarvestMemory2sFromImports (oldModule:Module) =

    let mutable exportIndex = 0u

    oldModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportMemory(memoryType)} 
                -> let exportOpt = FindExport oldModule (ExportMemory(MemIdx(U32(exportIndex))))
                   let import2 = { Export=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedMemory2({Import=import2; MemoryType=memoryType}))
            | _ -> None)


let HarvestGlobal2sFromImports (oldModule:Module) =

    let mutable exportIndex = 0u

    oldModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportGlobal(globalType)} 
                -> let exportOpt = FindExport oldModule (ExportGlobal(GlobalIdx(U32(exportIndex))))
                   let import2 = { Export=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedGlobal2({Import=import2; GlobalType=globalType}))
            | _ -> None)


let HarvestFunction2sFromImports (oldModule:Module) =

    let mutable exportIndex = 0u

    oldModule.Imports |> Array.choose (fun imp ->
        match imp with
            | {ImportModuleName=m; ImportName=n; ImportDesc=ImportFunc(funcType)} 
                -> let exportOpt = FindExport oldModule (ExportFunc(FuncIdx(U32(exportIndex))))
                   let import2 = { Export=exportOpt; ImportModuleName=m; ImportName=n }
                   exportIndex <- exportIndex + 1u
                   Some(ImportedFunction2({Import=import2; FuncType=funcType}))
            | _ -> None)


// -------------------------------------------------------------------------------------------------
//  Harvest from respective sections
// -------------------------------------------------------------------------------------------------


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
                        (oldOffsetExpr, oldFuncIdxArray)
                    | None -> 
                        ([||], [||])

            objectIndex <- objectIndex + 1u

            InternalTable2({ Export=exportOpt; 
                TableType=oldTable.TableType; 
                InitOffsetExpr=fst newInit; 
                InitWith=snd newInit})
        )




let HarvestInternalGlobals (newImportedGlobals:Global2[]) (oldModule:Module) =

    let mutable objectIndex = (uint32 newImportedGlobals.Length)

    oldModule.Globals |> Array.map (fun oldGlobal -> 

            let exportOpt = FindExport oldModule (ExportGlobal(GlobalIdx(U32(objectIndex))))
            
            objectIndex <- objectIndex + 1u
            InternalGlobal2({ Export=exportOpt; GlobalType=oldGlobal.GlobalType; InitExpr=oldGlobal.InitExpr })
        )



let HarvestInternalMems (newImportedMems:Memory2[]) (oldModule:Module) =

    let mutable objectIndex = (uint32 newImportedMems.Length)

    oldModule.Mems |> Array.map (fun oldMem -> 

            let thisMemIdx = MemIdx(U32(objectIndex))
            let exportOpt = FindExport oldModule (ExportMemory(thisMemIdx))
            let translatedInitData = GetAllInitialisationsForThisMem oldModule thisMemIdx
    
            objectIndex <- objectIndex + 1u
            InternalMemory2({ Export=exportOpt; MemoryType=oldMem.MemType; InitData=translatedInitData })
        )



let HarvestInternalFuncs (newImportedFuncs:Function2[]) (oldModule:Module) =

    let mutable objectIndex = (uint32 newImportedFuncs.Length)
    let mutable codeSecIndex = 0

    oldModule.Codes |> Array.map (fun oldCode -> 

        let thisFuncIdx = FuncIdx(U32(objectIndex))
        let exportOpt = FindExport oldModule (ExportFunc(thisFuncIdx))
        let funcType = GetFuncType oldModule codeSecIndex
        let newLocals = oldCode.Function.Locals |> FlattenLocalsToArray 

        objectIndex <- objectIndex + 1u
        codeSecIndex <- codeSecIndex + 1

        InternalFunction2({ Export = exportOpt; 
            OriginalCodeSecIndex = U32(uint32 codeSecIndex); 
            CodeSize = oldCode.CodeSize; 
            FuncType = funcType; 
            Locals = newLocals; 
            Body = oldCode.Function.Body })
        )



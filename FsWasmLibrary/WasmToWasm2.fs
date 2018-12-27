module WasmToWasm2

open Wasm
open Wasm2
open WasmAlgorithms




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






let TranslateWasmToWasm2 (wasmModule:Module) =

    let newImportedFuncs = wasmModule |> HarvestFunction2sFromImports 
    let newImportedMems = wasmModule |> HarvestMemory2sFromImports
    let newImportedTables = wasmModule |> HarvestTable2sFromImports
    let newImportedGlobals = wasmModule |> HarvestGlobal2sFromImports

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

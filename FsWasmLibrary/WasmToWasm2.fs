module WasmToWasm2

open WasmFileTypes
open Wasm2
open PrivateWasmToWasm2

let TranslateWasmToWasm2 (oldModule:Module) =

    let raiseErrorIfExists (customArray:Custom[]) =
        if customArray.Length > 0
        then failwith "Cannot translate a module that has a Custom section.  The custom section is not understood by this translator."

    oldModule.Custom1  |> raiseErrorIfExists
    oldModule.Custom2  |> raiseErrorIfExists
    oldModule.Custom3  |> raiseErrorIfExists
    oldModule.Custom4  |> raiseErrorIfExists
    oldModule.Custom5  |> raiseErrorIfExists
    oldModule.Custom6  |> raiseErrorIfExists
    oldModule.Custom7  |> raiseErrorIfExists
    oldModule.Custom8  |> raiseErrorIfExists
    oldModule.Custom9  |> raiseErrorIfExists
    oldModule.Custom10 |> raiseErrorIfExists
    oldModule.Custom11 |> raiseErrorIfExists
    oldModule.Custom12 |> raiseErrorIfExists

    let newImportedFuncs   = oldModule |> HarvestFunction2sFromImports 
    let newImportedMems    = oldModule |> HarvestMemory2sFromImports
    let newImportedTables  = oldModule |> HarvestTable2sFromImports
    let newImportedGlobals = oldModule |> HarvestGlobal2sFromImports

    let newInternalFuncs   = oldModule |> HarvestInternalFuncs   newImportedFuncs.Length
    let newInternalMems    = oldModule |> HarvestInternalMems    newImportedMems.Length
    let newInternalTables  = oldModule |> HarvestInternalTables  newImportedTables.Length
    let newInternalGlobals = oldModule |> HarvestInternalGlobals newImportedGlobals.Length

    let finalFuncs   = Array.append newImportedFuncs   newInternalFuncs
    let finalMems    = Array.append newImportedMems    newInternalMems
    let finalTables  = Array.append newImportedTables  newInternalTables
    let finalGlobals = Array.append newImportedGlobals newInternalGlobals

    {
        Funcs   = finalFuncs;
        Mems    = finalMems;
        Tables  = finalTables;
        Globals = finalGlobals;
        Start   = oldModule.Start;
    }

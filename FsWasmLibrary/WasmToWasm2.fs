﻿module WasmToWasm2

open Wasm
open Wasm2
open PrivateWasmToWasm2


let TranslateWasmToWasm2 (oldModule:Module) =

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

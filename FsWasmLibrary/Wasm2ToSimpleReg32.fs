module Wasm2ToSimpleReg32

open Wasm2
open PrivateWasm2ToSimpleReg32



let TranslateWasm2ToSimpleReg32 (m:Module2) =   // TODO: rename because write out to text???

    // Set up conceptual output streams

    let writeOutData s = printfn "DATA> %s" s   // TODO: This is temporary while developing.  Want caller to pass this in???
    let writeOutCode s = printfn "CODE> %s" s

    // Start outputting ASM language text:

    writeOutData "// Translation of WASM module"
    writeOutData ""

    m.Tables |> Array.iteri (fun i t ->
        match t with
            | InternalTable2(tbl) -> tbl |> TranslateTable writeOutData i m 
            | ImportedTable2(tbl) -> () // TODO: Error?  Can't support importing, expect self-contained module.
        )

    m.Globals |> Array.iteri (fun i g ->
        match g with
            | InternalGlobal2(glo) -> glo |> TranslateGlobal writeOutData i m 
            | ImportedGlobal2(glo) -> () // TODO: Error?  Can't support importing, expect self-contained module.
        )

    m.Mems |> Array.iteri (fun i me ->
        match me with
            | InternalMemory2(mem) -> mem |> TranslateMemory writeOutData i 
            | ImportedMemory2(mem) -> () // TODO: Error?  Can't support importing, expect self-contained module.
        )

    let mutable moduleTranslationState = ModuleTranslationState(0)  // TODO: hide ideally

    m.Funcs |> Array.iteri (fun i g ->
        match g with 
            | InternalFunction2(g) -> 
                moduleTranslationState <- g |> 
                    TranslateFunctionAndBranchTables writeOutCode writeOutData i m moduleTranslationState
            | ImportedFunction2(g) -> () // TODO:  Error?  Can't support importing, expect self-contained module.
        )

    TranslateStart writeOutData m.Start

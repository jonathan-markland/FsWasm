module Wasm2ToSimpleReg32

open Wasm2
open PrivateWasm2ToSimpleReg32



let TranslateWasm2ToSimpleReg32 (m:Module2) =   // TODO: rename because write out to text???

    let writeOut s = printfn "%s" s   // TODO: This is temporary while developing.  Want caller to pass this in???

    writeOut "// Translation of WASM module"
    writeOut ""

    // ASM DATA section

    m.Funcs |> Array.iteri (fun i f ->
        match f with 
            | InternalFunction2(g) -> TablesOfAddressesToDataSectionText writeOut m g    // ie: used by WASM "switch"
            | ImportedFunction2(g) -> () // TODO: Error?  Can't support importing, expect self-contained module.
        )

    m.Tables |> Array.iteri (fun i t ->
        match t with
            | InternalTable2(tbl) -> TranslateTable writeOut i m tbl
            | ImportedTable2(tbl) -> () // TODO: Error?  Can't support importing, expect self-contained module.
        )

    m.Globals |> Array.iteri (fun i g ->
        match g with
            | InternalGlobal2(glo) -> TranslateGlobal writeOut i m glo
            | ImportedGlobal2(glo) -> () // TODO: Error?  Can't support importing, expect self-contained module.
        )

    m.Mems |> Array.iteri (fun i me ->
        match me with
            | InternalMemory2(mem) -> TranslateMemory writeOut i mem
            | ImportedMemory2(mem) -> () // TODO: Error?  Can't support importing, expect self-contained module.
        )

    // ASM CODE section

    m.Funcs |> Array.iteri (fun i g ->
        match g with 
            | InternalFunction2(g) -> TranslateFunction writeOut i m g
            | ImportedFunction2(g) -> () // TODO:  Error?  Can't support importing, expect self-contained module.
        )

    TranslateStart writeOut m.Start


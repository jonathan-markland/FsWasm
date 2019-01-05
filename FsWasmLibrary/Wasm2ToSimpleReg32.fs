module Wasm2ToSimpleReg32

open Wasm
open Wasm2
open PrivateWasm2ToSimpleReg32



let TranslateWasm2ToSimpleReg32 (m:Module2) =   // TODO: rename because write out to text???

    let writeOut s = printfn "%s" s   // TODO: This is temporary while developing.  Want caller to pass this in???

    writeOut "// Translation of WASM module"
    writeOut ""

    m.Funcs |> Array.iteri (fun i g ->
        match g with 
            | ImportedFunction2(g) -> () // TODO
            | InternalFunction2(g) -> TranslateFunction writeOut i m g)

    TranslateStart writeOut m.Start


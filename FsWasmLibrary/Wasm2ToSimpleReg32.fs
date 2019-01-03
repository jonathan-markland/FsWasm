module Wasm2ToSimpleReg32

open Wasm
open Wasm2
open PrivateWasm2ToSimpleReg32


let TranslateWasm2ToSimpleReg32 (m:Module2) =

    m.Funcs 
        |> Array.map (fun f -> 
            match f with 
                | ImportedFunction2(g) -> [||]
                | InternalFunction2(g) -> g.Body |> TranslateInstructionsMaster
            )
        |> Array.map Optimise

       



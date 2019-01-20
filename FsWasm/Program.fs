﻿
open System.IO
open WasmSerialiser
open WasmFileReader
open Wasm2ToSimpleReg32
open Wasm2ToSimpleReg32ConfigTypes

// Main

[<EntryPoint>]
let main argv =

    let fileName = "program (5).wasm"  // TODO: pass on command line
    let fileImage = File.ReadAllBytes fileName
    let r = new BinaryReader(fileImage)

    let thisModule = r |> Module
    let unitTestSerialisation = thisModule |> UnitTestSerialiser.ModuleToUnitTestString fileName

    let translatedToWasm2 = thisModule |> WasmToWasm2.TranslateWasmToWasm2

    let config = WriteOutFunctionConfig(WithBarriers, FullyOptimised)

    let translatedToSimpleReg32 = translatedToWasm2 |> WriteOutWasm2AsJonathansAssemblerText config
    
    0 // return an integer exit code
 
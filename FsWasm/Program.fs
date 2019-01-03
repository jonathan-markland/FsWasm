
open System.IO
open WasmSerialiser
open WasmFileReader
open Wasm2ToSimpleReg32

// Main

[<EntryPoint>]
let main argv =

    let fileName = "program (4).wasm"
    let fileImage = File.ReadAllBytes fileName
    let r = new BinaryReader(fileImage)

    let thisModule = r |> Module
    let unitTestSerialisation = thisModule |> UnitTestSerialiser.ModuleToUnitTestString fileName

    let translatedToWasm2 = thisModule |> WasmToWasm2.TranslateWasmToWasm2
    let translatedToSimpleReg32 = translatedToWasm2 |> TranslateWasm2ToSimpleReg32
    

    printfn "Hello World from F#!"
    0 // return an integer exit code

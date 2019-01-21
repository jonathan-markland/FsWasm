
open System.IO
open WasmSerialiser
open WasmFileReader
open Wasm2ToSimpleReg32
open Wasm2ToSimpleReg32ConfigTypes

// Main

[<EntryPoint>]
let main argv =

    let paramFileName = "program (5).wasm"  // TODO: pass on command line

    try

        let fileName  = Path.GetFullPath(paramFileName)
        let fileImage = File.ReadAllBytes fileName
        let fileDate  = File.GetLastWriteTime(fileName).ToString()

        let r = new BinaryReader(fileImage)

        let thisModule = r |> Module
        // TODO:  Should this be an optional output?  let unitTestSerialisation = thisModule |> UnitTestSerialiser.ModuleToUnitTestString fileName

        let translatedToWasm2 = thisModule |> WasmToWasm2.TranslateWasmToWasm2

        let config = WriteOutFunctionConfig(WithBarriers, FullyOptimised)

        let headingText = (sprintf "%s (%d bytes) %s" fileName (fileImage.Length) fileDate)

        translatedToWasm2
            |> WriteOutWasm2AsJonathansAssemblerText config headingText

    with

        | _ as ex -> printf "%s: %s" paramFileName (ex.ToString())
    
    0 // return an integer exit code
 
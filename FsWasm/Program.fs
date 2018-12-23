
open System.IO
open WasmSerialiser
open WasmFileReader

// Main

[<EntryPoint>]
let main argv =

    let fileName = "program (4).wasm"
    let fileImage = File.ReadAllBytes fileName
    let r = new BinaryReader(fileImage)
    let thisModule = r |> Module

    // Obtain the array that TypeIdx values really index.

    let unitTestString = UnitTestSerialiser.ModuleToUnitTestString fileName thisModule

    printfn "Hello World from F#!"
    0 // return an integer exit code

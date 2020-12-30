
open System.IO
open WasmSerialiser
open WasmFileReader
open BetterWasmToJonathansAsm
open BWToCRMConfigurationTypes
open CompilationOutputting
open WasmToBetterWasm

// Main

[<EntryPoint>]
let main argv =

    let paramFileName = "program (2).wasm"  // TODO: pass on command line

    try

        let fileName  = Path.GetFullPath(paramFileName)
        let fileImage = File.ReadAllBytes fileName
        let fileDate  = File.GetLastWriteTime(fileName).ToString()

        let binaryReader = new BinaryReader(fileImage)

        let thisModule = binaryReader |> ReadWasmModule
        // TODO:  Should this be an optional output?  let unitTestSerialisation = thisModule |> UnitTestSerialiser.ModuleToUnitTestString fileName

        let betterWasm = thisModule |> ToBetterWasm

        let config = TranslationConfiguration(WithoutBarriers, FullyOptimised)  // TODO: Hard-code config!!

        let headingText = (sprintf "%s (%d bytes) %s" fileName (fileImage.Length) fileDate)

        let theProcess writeOutData writeOutCode writeOutVar =
            betterWasm
                |> WriteOutWasm2AsJonathansAssemblerText config headingText writeOutData writeOutCode writeOutVar

        theProcess |> OutputInFinalOrder  // or use OutputForDebug to see one-the-go emission ordering.

    with

        | _ as ex -> printf "%s: %s" paramFileName (ex.ToString())
    
    0 // return an integer exit code
 
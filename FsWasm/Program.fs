
open System.IO
open WasmSerialiser
open WasmFileReader
open BetterWasmToJonathansAsm
open BetterWasmToX86Asm
open BetterWasmToArm32Asm
open BWToCRMConfigurationTypes
open CompilationOutputting
open WasmToBetterWasm

// Main  -- TODO: This is just a spike main program, essentially.

[<EntryPoint>]
let main argv =

    let paramFileName = "program-7.wasm"  // TODO: pass on command line

    try

        let fileName  = Path.GetFullPath(paramFileName)
        let fileImage = File.ReadAllBytes fileName
        let fileDate  = File.GetLastWriteTime(fileName).ToString()

        let binaryReader = new BinaryReader(fileImage)

        let thisModule = binaryReader |> ReadWasmModule
        // TODO:  Should this be an optional output?  let unitTestSerialisation = thisModule |> UnitTestSerialiser.ModuleToUnitTestString fileName

        let betterWasm = thisModule |> ToBetterWasm

        let config = TranslationConfiguration(WithoutBarriers, FullyOptimised, (ForceEntryPoint "main"))
        // let config = TranslationConfiguration(WithBarriers, NoOptimisation, (ForceEntryPoint "main"))

        let headingText = (sprintf "%s (%d bytes) %s" fileName (fileImage.Length) fileDate)

        betterWasm
            // |> TranslateBetterWasmToX86AssemblerStdOut config headingText
            |> TranslateBetterWasmToArm32AssemblerStdOut config headingText

    with

        | _ as ex -> printf "%s: %s" paramFileName (ex.ToString())
    
    0 // return an integer exit code
 
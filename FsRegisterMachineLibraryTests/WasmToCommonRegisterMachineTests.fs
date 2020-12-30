module WasmToCommonRegisterMachineTests

open System
open System.IO
open WasmSerialiser
open WasmFileReader
open BetterWasmToCommonRegisterMachine
open BWToCRMConfigurationTypes
open WasmToBetterWasm

open Xunit


let WasmToCommonRegisterMachineText paramFileName =

    let outputInOrderOfGeneration f =
        let sb = new System.Text.StringBuilder()
        let append s1 s2 = sb.Append(sprintf "%s> %s\n" s1 s2) |> ignore
        let writeOutData s = append "DATA" s
        let writeOutVar  s = append "VAR " s
        let writeOutCode s = append "CODE" s
        f writeOutData writeOutCode writeOutVar
        sb.ToString()

    try

        let fileName  = Path.GetFullPath(paramFileName)
        let fileImage = File.ReadAllBytes fileName
        let fileDate  = File.GetLastWriteTime(fileName).ToString()

        let binaryReader = new BinaryReader(fileImage)

        let thisModule = binaryReader |> ReadWasmModule
        // TODO:  Should this be an optional output?  let unitTestSerialisation = thisModule |> UnitTestSerialiser.ModuleToUnitTestString fileName

        let betterWasm = thisModule |> ToBetterWasm

        let config = TranslationConfiguration(WithoutBarriers, FullyOptimised, FinalOutputOrder)  // TODO: Hard-code config!!

        let headingText = (sprintf "%s (%d bytes) %s" fileName (fileImage.Length) fileDate)

        let theProcess writeOutData writeOutCode writeOutVar =
            betterWasm
                |> WriteOutWasm2AsJonathansAssemblerText config headingText writeOutData writeOutCode writeOutVar

        outputInOrderOfGeneration theProcess

    with

        | _ as ex -> sprintf "Exception: %s" (ex.ToString())




let FilePassesTest n =
    let inputFile = (sprintf "program-%d.wasm" n)
    let expectationFile = (sprintf "program-%d-crm-asm.txt" n)
    let actual = WasmToCommonRegisterMachineText inputFile
    let expected = System.IO.File.ReadAllText expectationFile
    String.Compare(expected, actual, StringComparison.InvariantCulture) = 0




[<Fact>]
let ``Program 1 to CRM`` () =
    Assert.True(FilePassesTest 1)


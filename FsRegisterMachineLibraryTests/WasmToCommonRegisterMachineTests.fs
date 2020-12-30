module WasmToCommonRegisterMachineTests

open System
open System.IO
open WasmSerialiser
open WasmFileReader
open BetterWasmToCommonRegisterMachine
open BWToCRMConfigurationTypes
open WasmToBetterWasm

open Xunit


let WasmToCommonRegisterMachineText config paramFileName =

    let outputInOrderOfGeneration f =
        let resizeArray = new ResizeArray<string>();
        let append s1 s2 = resizeArray.Add(sprintf "%s> %s" s1 s2) |> ignore
        let writeOutData s = append "DATA" s
        let writeOutVar  s = append "VAR " s
        let writeOutCode s = append "CODE" s
        f writeOutData writeOutCode writeOutVar
        resizeArray.ToArray()

    try

        let fileName  = Path.GetFullPath(paramFileName)
        let fileImage = File.ReadAllBytes fileName
        let fileDate  = File.GetLastWriteTime(fileName).ToString()

        let binaryReader = new BinaryReader(fileImage)

        let thisModule = binaryReader |> ReadWasmModule
        // TODO:  Should this be an optional output?  let unitTestSerialisation = thisModule |> UnitTestSerialiser.ModuleToUnitTestString fileName

        let betterWasm = thisModule |> ToBetterWasm

        let headingText = (sprintf "%s (%d bytes) %s" fileName (fileImage.Length) fileDate)

        let theProcess writeOutData writeOutCode writeOutVar =
            betterWasm
                |> WriteOutWasm2AsJonathansAssemblerText config headingText writeOutData writeOutCode writeOutVar

        outputInOrderOfGeneration theProcess

    with

        | _ as ex -> 
            let message = sprintf "Exception: %s" (ex.Message)
            message.Split('\n')




let FilePassesTest n =
    let inputFile = (sprintf "program-%d.wasm" n)
    let expectationFile = (sprintf "program-%d-crm-asm.txt" n)
    let config = TranslationConfiguration(WithoutBarriers, FullyOptimised, FinalOutputOrder)  // TODO: Hard-code config!!
    let actual = WasmToCommonRegisterMachineText config inputFile
    let actualFileImage = actual |> String.concat "\r\n"
    let expected = System.IO.File.ReadAllLines expectationFile  // TODO: More detail on failed comparison.
    let b = (actual = expected)
    b




[<Fact>]
let ``Program 1 to CRM`` () =
    Assert.True(FilePassesTest 1)

[<Fact>]
let ``Program 2 to CRM`` () =
    Assert.True(FilePassesTest 2)

[<Fact>]
let ``Program 3 to CRM`` () =
    Assert.True(FilePassesTest 3)

[<Fact>]
let ``Program 4 to CRM`` () =
    Assert.True(FilePassesTest 4)

[<Fact>]
let ``Program 5 to CRM`` () =
    Assert.True(FilePassesTest 5)

[<Fact>]
let ``Program 6 to CRM`` () =
    Assert.True(FilePassesTest 6)


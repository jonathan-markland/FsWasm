module WasmToCommonRegisterMachineTests

open System.IO
open WasmSerialiser
open WasmFileReader
open BWToCRMConfigurationTypes
open WasmToBetterWasm
open Xunit



let WasmToCommonRegisterMachineText asAssemblyLanguage config paramFileName =

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
                |> asAssemblyLanguage config headingText writeOutData writeOutCode writeOutVar

        outputInOrderOfGeneration theProcess

    with

        | _ as ex -> 
            let message = sprintf "Exception: %s" (ex.Message)
            message.Split('\n')




type OptimisationCase = Optimised | Unoptimised



let FilePassesTestWhenTranslatedUsing cpuKind asAssemblyLanguage n optimisationCase entryPointConfig =
    
    let fileSubType = 
        match optimisationCase with
            | Optimised   -> "optimised"
            | Unoptimised -> "unoptimised"

    let config =
        match optimisationCase with
            | Optimised   -> TranslationConfiguration (WithoutBarriers, FullyOptimised, entryPointConfig)
            | Unoptimised -> TranslationConfiguration (WithBarriers,    NoOptimisation, entryPointConfig)

    let inputFile = (sprintf "program-%d.wasm" n)
    
    let actual = WasmToCommonRegisterMachineText asAssemblyLanguage config inputFile
    let actualFileImage = actual |> String.concat "\r\n"  // This is useful for developing a new test case.

    let expectationFile = (sprintf "program-%d-%s-%s-asm.txt" n fileSubType cpuKind)
    let expected = System.IO.File.ReadAllLines expectationFile  // TODO: More detail on failed comparison.

    let b = (actual = expected)
    b




let FilePassesTest = FilePassesTestWhenTranslatedUsing "crm" BetterWasmToJonathansAsm.WriteOutWasm2AsJonathansAssemblerText

[<Fact>] 
let ``Program 1 to CRM optimised to Jonathans Asm`` () = Assert.True(FilePassesTest 1 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 2 to CRM optimised to Jonathans Asm`` () = Assert.True(FilePassesTest 2 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 3 to CRM optimised to Jonathans Asm`` () = Assert.True(FilePassesTest 3 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 4 to CRM optimised to Jonathans Asm`` () = Assert.True(FilePassesTest 4 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 5 to CRM optimised to Jonathans Asm`` () = Assert.True(FilePassesTest 5 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 6 to CRM optimised to Jonathans Asm`` () = Assert.True(FilePassesTest 6 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 1 to CRM unoptimised to Jonathans Asm`` () = Assert.True(FilePassesTest 1 Unoptimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 2 to CRM unoptimised to Jonathans Asm`` () = Assert.True(FilePassesTest 2 Unoptimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 3 to CRM unoptimised to Jonathans Asm`` () = Assert.True(FilePassesTest 3 Unoptimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 4 to CRM unoptimised to Jonathans Asm`` () = Assert.True(FilePassesTest 4 Unoptimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 5 to CRM unoptimised to Jonathans Asm`` () = Assert.True(FilePassesTest 5 Unoptimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 6 to CRM unoptimised to Jonathans Asm`` () = Assert.True(FilePassesTest 6 Unoptimised WasmStartEntryPointIfPresent)





let FilePassesTestX86 = FilePassesTestWhenTranslatedUsing "x8632" BetterWasmToX86Asm.WriteOutWasm2AsX86AssemblerText

[<Fact>] 
let ``Program 1 to CRM optimised to X86 32`` () = Assert.True(FilePassesTestX86 1 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 2 to CRM optimised to X86 32`` () = Assert.True(FilePassesTestX86 2 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 3 to CRM optimised to X86 32`` () = Assert.True(FilePassesTestX86 3 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 4 to CRM optimised to X86 32`` () = Assert.True(FilePassesTestX86 4 Optimised (ForceEntryPoint "main"))
[<Fact>] 
let ``Program 5 to CRM optimised to X86 32`` () = Assert.True(FilePassesTestX86 5 Optimised (ForceEntryPoint "main"))
[<Fact>] 
let ``Program 6 to CRM optimised to X86 32`` () = Assert.True(FilePassesTestX86 6 Optimised WasmStartEntryPointIfPresent)





let FilePassesTestARM = FilePassesTestWhenTranslatedUsing "arm32" BetterWasmToArm32Asm.WriteOutWasm2AsArm32AssemblerText

[<Fact>] 
let ``Program 1 to CRM optimised to ARM 32`` () = Assert.True(FilePassesTestARM 1 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 2 to CRM optimised to ARM 32`` () = Assert.True(FilePassesTestARM 2 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 3 to CRM optimised to ARM 32`` () = Assert.True(FilePassesTestARM 3 Optimised WasmStartEntryPointIfPresent)
[<Fact>] 
let ``Program 4 to CRM optimised to ARM 32`` () = Assert.True(FilePassesTestARM 4 Optimised (ForceEntryPoint "main"))
[<Fact>] 
let ``Program 5 to CRM optimised to ARM 32`` () = Assert.True(FilePassesTestARM 5 Optimised (ForceEntryPoint "main"))
[<Fact>] 
let ``Program 6 to CRM optimised to ARM 32`` () = Assert.True(FilePassesTestARM 6 Optimised WasmStartEntryPointIfPresent)





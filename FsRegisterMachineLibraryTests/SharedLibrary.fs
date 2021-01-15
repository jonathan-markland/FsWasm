module SharedLibrary

open System.IO
open WasmSerialiser
open WasmFileReader
open BWToCRMConfigurationTypes
open WasmToBetterWasm


let private WasmToCommonRegisterMachineText asAssemblyLanguage config paramFileName =

    let outputInOrderOfGeneration f =
        let resizeArray = new ResizeArray<string>();
        let append s1 s2 = resizeArray.Add(sprintf "%s> %s" s1 s2) |> ignore
        let writeOutHead s = append "HEAD" s
        let writeOutData s = append "DATA" s
        let writeOutVar  s = append "VAR " s
        let writeOutCode s = append "CODE" s
        f writeOutHead writeOutData writeOutCode writeOutVar
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

        let theProcess writeOutHead writeOutData writeOutCode writeOutVar =
            betterWasm
                |> asAssemblyLanguage config headingText writeOutHead writeOutData writeOutCode writeOutVar

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
            | Optimised   -> TranslationConfiguration (WithBarriers, FullyOptimised, entryPointConfig)
            | Unoptimised -> TranslationConfiguration (WithBarriers, NoOptimisation, entryPointConfig)

    let inputFile = (sprintf "program-%d.wasm" n)
    
    let actual = WasmToCommonRegisterMachineText asAssemblyLanguage config inputFile
    let actualFileImage = actual |> String.concat "\r\n"  // This is useful for developing a new test case.

    let expectationFile = (sprintf "program-%d-%s-%s-asm.txt" n fileSubType cpuKind)
    let expected = System.IO.File.ReadAllLines expectationFile  // TODO: More detail on failed comparison.

    // Warning: Only uncomment when multi-updating test files:   
    File.WriteAllText(expectationFile, actualFileImage) |> ignore // Can be used hackishly to update all the expectation files, in the execution folder.  You could then copy those over the source code folder and check in.

    let b = (actual = expected)
    b







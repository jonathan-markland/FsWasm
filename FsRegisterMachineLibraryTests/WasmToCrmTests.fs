module WasmToCrmTests

open Xunit
open Library
open SharedLibrary
open CommonRegisterMachineTypes
open BWToCRMConfigurationTypes
open WasmBetterTypes
open WasmInstructionsToCRMInstructions
open AsmPrefixes



let WriteOutFunctionAndBranchTables writeOutCode _writeOutTables (funcIndex:int) (m:Module) translationState config (f:InternalFunctionRecord) =   // TODO: module only needed to query function metadata in TranslateInstructions

    let wasmToCrmTranslationConfig = 
        { ClearParametersAfterCall = true } 

    let crmInstructions, updatedTranslationState = 
        TranslateInstructionsAndApplyOptimisations
            f m.Funcs translationState wasmToCrmTranslationConfig config id

    let prettyFormatted instruction =

        let defaultFormat instruction = sprintf "        %A" instruction

        match instruction with 
            | Barrier                -> ["        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"]
            | Label (LabelName name) -> ["    " + name + ":"]
            | Goto _                 -> [instruction |> defaultFormat ; ""]
            | _                      -> [instruction |> defaultFormat]

    writeOutCode (sprintf "function %s%d:" AsmInternalFuncNamePrefix funcIndex)

    crmInstructions
        |> List.iter (prettyFormatted >> (List.iter writeOutCode))

    writeOutCode "end function"
    writeOutCode ""
    writeOutCode ""
    writeOutCode ""

    updatedTranslationState



let WriteOutBetterWasmAsCrmFsharpText config headingText writeFileHeader writeOutData writeOutCode _writeOutVar (m:Module) =   // TODO: rename because write out to text???

    ("Common Register Machine function serialisation from WASM module: " + headingText) |> writeFileHeader

    let mutable moduleTranslationState = ModuleTranslationState(0)  // TODO: hide ideally

    m.Funcs |> Array.iteri (fun i g ->  // TODO: Should this be in the library?
        match g with 

            | InternalFunction2 g -> 
                let newState =
                    g |> WriteOutFunctionAndBranchTables writeOutCode writeOutData i m moduleTranslationState config
                moduleTranslationState <- newState

            | ImportedFunction2 _ -> ()
        )



let FilePassesTest fileNumber optimiseMode entryPointConfig = 
    FilePassesTestWhenTranslatedUsing 
        "crm" WriteOutBetterWasmAsCrmFsharpText
        fileNumber optimiseMode entryPointConfig
            |> Assert.True


[<Fact>] 
let ``Program 5 to CRM optimised`` () = 
    FilePassesTest 5 Optimised (ForceEntryPoint "main")


[<Fact>] 
let ``Program 5 to CRM unoptimised`` () = 
    FilePassesTest 5 Unoptimised (ForceEntryPoint "main")


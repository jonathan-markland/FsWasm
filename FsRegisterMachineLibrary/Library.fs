module Library

open System.Text
open WasmFileTypes
open WasmBetterTypes
open BWToCRMConfigurationTypes
open CommonRegisterMachineTypes
open OptimiseCommonRegisterMachine
open WasmStaticExpressionEvaluator
open AsmPrefixes



let WriteOutHexDump (command:string) (byteSeparator:string) (hexPrefix:string) writeLine (byteArray:byte[]) =

    let sb = new StringBuilder(16 * (2 + byteSeparator.Length + hexPrefix.Length) + command.Length)

    byteArray |> Array.iteri (fun i byteVal -> 

        let c = i &&& 15
    
        sb.Append (
            match c with
                | 0 -> sprintf "%s %s%02X" command hexPrefix byteVal
                | _ -> sprintf "%s%s%02X" byteSeparator hexPrefix byteVal
            )
            |> ignore

        match c with
            | 15 -> 
                writeLine (sb.ToString())
                sb.Clear() |> ignore
            | _ -> ()
    )

    if sb.Length > 0 then writeLine (sb.ToString())



let WriteOutWasmStart writeOutBranchToEntryLabel writeOut toComment startOption moduleFuncsArray =
    match startOption with 
        | Some { StartFuncIdx = startFuncIdx } -> 
            writeOutBranchToEntryLabel writeOut startFuncIdx moduleFuncsArray
        | None -> 
            "No entry point in this translation" |> toComment |> writeOut



let private ReturnsSingleValue (ft:FuncType) =
    match ft.ReturnTypes.Length with
        | 0 -> false
        | 1 -> true
        | _ -> failwith "Cannot translate function that returns more than one value"



/// Iterate through all of the translated versions of the function's instructions.
let IterFunctionTranslated action translate thisFuncType config crmInstructions =

    let optimisationPhase1 = 
        match config with
            | TranslationConfiguration(_,FullyOptimised) -> crmInstructions |> Optimise
            | TranslationConfiguration(_,NoOptimisation) -> crmInstructions

    let optimisationPhase2 =
        match config with
            | TranslationConfiguration(WithBarriers,_)    -> optimisationPhase1
            | TranslationConfiguration(WithoutBarriers,_) -> optimisationPhase1 |> RemoveBarriers

    let finalInstructions = optimisationPhase2

    // Kick off the whole thing here:

    finalInstructions |> List.iter (fun crmInstruction -> translate crmInstruction |> List.iter action)

    // Handle the function's return (may need pop into A):

    let returnHandlingCode = 
        match thisFuncType |> ReturnsSingleValue with
            | true  -> translate (Pop A)  // TODO: not ideal construction of temporary
            | false -> []

    returnHandlingCode |> List.iter action



/// Iterate all of the branch tables in the given function's instructions.
let IterBranchTables branchTableStart branchTableItem crmInstructions =

    crmInstructions |> List.iter (fun instruction ->

        match instruction with
            
            | GotoIndex(LabelName tableLabel,_,_,codePointLabels) ->
                branchTableStart tableLabel
                codePointLabels |> Array.iter (fun (LabelName targetLabel) -> branchTableItem targetLabel)

            | _ -> ()
        )



/// Iterate wasm table heading and content.
let IterWasmTable wasmTableHeading wasmTableRow i (t:InternalTableRecord) =

    match t.InitData.Length with

        | 0 -> ()

        | 1 ->
            wasmTableHeading i
            t.InitData |> Array.iter (fun elem ->
                    let ofsExpr, funcIdxList = elem
                    let ofsValue = StaticEvaluate ofsExpr
                    if ofsValue <> 0 then failwith "Cannot translate module with TableSec table that has Elem with non-zero data initialisation offset"
                    funcIdxList |> Array.iter (fun funcIdx -> wasmTableRow (FuncIdxNameString funcIdx))
                )

        | _ -> failwith "Cannot translate module with more than one Elem in a TableSec table"




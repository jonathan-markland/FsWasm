module Library

open System.Text
open WasmFileTypes
open WasmBetterTypes
open BWToCRMConfigurationTypes
open CommonRegisterMachineTypes
open OptimiseCommonRegisterMachine
open WasmStaticExpressionEvaluator
open AsmPrefixes



let ForEachLineOfHexDumpDo (command:string) (byteSeparator:string) (hexPrefix:string) writeLine (byteArray:byte[]) =

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



let WithWasmStartDo writeOutBranchToEntryLabel writeOut toComment startOption moduleFuncsArray =
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
let ForTranslatedCrmInstructionsDo action translate thisFuncType config crmInstructions =

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
let ForAllBranchTablesDo branchTableStart branchTableItem crmInstructions =

    crmInstructions |> List.iter (fun instruction ->

        match instruction with
            
            | GotoIndex(LabelName tableLabel,_,_,codePointLabels) ->
                branchTableStart tableLabel
                codePointLabels |> Array.iter (fun (LabelName targetLabel) -> branchTableItem targetLabel)

            | _ -> ()
        )



/// Iterate wasm table heading and content.
let ForWasmTableDo wasmTableHeading wasmTableRow i (t:InternalTableRecord) =

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





/// Do actions with WASM memory and any initialisation data blocks.
let WithWasmMemDo wasmMemHeading wasmMemRow memIndex (thisMem:InternalMemoryRecord) =

    let WasmMemoryBlockMultiplier = 65536u

    let linearMemorySize = 

        match thisMem with 
            | { MemoryType={ MemoryLimits=lims } } -> 

            match lims with 

                | { LimitMin = U32 0u ; LimitMax = None }
                    -> failwith "Cannot translate module with Mem that is size 0"   

                | { LimitMin = U32 memSize ; LimitMax = None } 
                    -> memSize * WasmMemoryBlockMultiplier

                | { LimitMin = _ ; LimitMax = Some _ }
                    -> failwith "Cannot translate module with Mem that has max size limit"

    wasmMemHeading memIndex linearMemorySize
    thisMem.InitData |> Array.iteri (fun dataBlockIndex (_, byteArray) -> wasmMemRow memIndex dataBlockIndex byteArray)



/// Do the action for all WASM tables, raising exception if an 
/// imported table is seen, since these are not yet supported.
let ForAllWasmTablesDo action tables =

    tables |> Array.iteri (fun tableIndex t ->
        match t with
            | InternalTable2 tbl -> action tableIndex tbl
            | ImportedTable2 tbl -> failwith "Error:  Cannot support importing a WASM 'table'.  WASM module must be self-contained."
        )



/// Do the action for all WASM globals, raising exception if an 
/// imported global is seen, since these are not yet supported.
let ForAllWasmGlobalsDo action globals =

    globals |> Array.iteri (fun globalIndex g ->
        match g with
            | InternalGlobal2 glo -> 
                let initValue = StaticEvaluate glo.InitExpr
                let globalIdx = GlobalIdx(U32(uint32 globalIndex))   // TODO: not ideal construction of temporary
                let globalIdxNameString = (GlobalIdxNameString globalIdx)
                action globalIdxNameString initValue
            | ImportedGlobal2 glo -> failwith "Error:  Cannot support importing a WASM 'global'.  WASM module must be self-contained."
        )



/// Do the action for all WASM memories, raising exception if an 
/// imported memory is seen, since these are not yet supported.
let ForAllWasmMemsDo action mems =

    mems |> Array.iteri (fun memIndex me ->
        match me with
            | InternalMemory2 mem -> action memIndex mem
            | ImportedMemory2 mem -> failwith "Error:  Cannot support importing a WASM 'memory'.  WASM module must be self-contained."
        )



/// Generate the initialisation function that arranges the statically-initialised
/// data blocks in the memory space.
let ForTheDataInitialisationFunctionDo writeOutCopyBlockCode writeOutIns translate (mems:Memory[]) =

    let writeOutDataCopyCommand i (thisMem:InternalMemoryRecord) =
        if i<>0 then failwith "Cannot translate WASM module with more than one Linear Memory"
        thisMem.InitData |> Array.iteri (fun j elem ->
                let ofsExpr, byteArray = elem
                let ofsValue = StaticEvaluate ofsExpr
                writeOutCopyBlockCode i j ofsValue byteArray.Length
            )

    mems |> ForAllWasmMemsDo writeOutDataCopyCommand
    (translate ThunkIn) |> List.iter writeOutIns




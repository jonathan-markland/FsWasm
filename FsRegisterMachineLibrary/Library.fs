module Library

open System.Text
open WasmFileTypes
open WasmBetterTypes
open BWToCRMConfigurationTypes
open CommonRegisterMachineTypes
open OptimiseCommonRegisterMachine
open WasmStaticExpressionEvaluator
open AsmPrefixes
open TextFormatting
open WasmInstructionsToCRMInstructions



/// Returns true if any WASM memory object within the array has
/// any InitData blocks.
let HasAnyInitDataBlocks mems =
    
    let memHasInitData mem =
        match mem with
            | InternalMemory2 intMem -> intMem.InitData.Length > 0
            | ImportedMemory2 _ -> false

    mems |> Array.exists memHasInitData



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



let WithWasmStartDo writeOutBranchToEntryLabel writeOut toComment startOption moduleFuncsArray entryPointConfig =

    let useFunc func =
        let labelName = FuncLabelFor func
        writeOutBranchToEntryLabel writeOut labelName

    let takesParameters intFunc =
        intFunc.FuncType.ParameterTypes.Length > 0

    match entryPointConfig with
    
        | WasmStartEntryPointIfPresent ->
            match startOption with 
                | Some func -> useFunc func
                | None -> "No WASM entry point (start record) in this translation" |> toComment |> writeOut

        | ForceEntryPoint exportFunctionName ->
            let entryFunctionOpt = moduleFuncsArray |> Array.tryPick (fun f ->
                match f with
                    | InternalFunction2 intFunc ->
                        match intFunc.Export with
                            | Some { ExportName=name } -> 
                                if name = exportFunctionName then Some (f,intFunc) else None
                            | None ->
                                None
                    | ImportedFunction2 _impFunc ->
                        None // TODO: Do we need to check if the name occurs here?  If we allow other modules, the other module should generate the entry link perhaps?
                )
            match entryFunctionOpt with
                | Some (func,intFunc) -> 
                    if intFunc |> takesParameters then
                        failwith (sprintf "Cannot use function '%s' as the entry point because it requires parameters." exportFunctionName)
                    useFunc func
                | None -> failwith (sprintf "Cannot find entry point function '%s'" exportFunctionName)





let private ReturnsSingleValue (ft:FuncType) =
    match ft.ReturnTypes.Length with
        | 0 -> false
        | 1 -> true
        | _ -> failwith "Cannot translate function that returns more than one value"



let TranslateInstructionsAndApplyOptimisations 
    (f:InternalFunctionRecord) (moduleFuncsArray:Function[]) translationState wasmToCrmTranslationConfig outputConfig =

    let crmInstructions, updatedTranslationState = 
        TranslateInstructions moduleFuncsArray translationState wasmToCrmTranslationConfig f.Body

    let optimisationPhase1 = 
        match outputConfig with
            | TranslationConfiguration(_,FullyOptimised,_) -> crmInstructions |> Optimise
            | TranslationConfiguration(_,NoOptimisation,_) -> crmInstructions

    let optimisationPhase2 =
        match outputConfig with
            | TranslationConfiguration(WithBarriers,_,_)    -> optimisationPhase1
            | TranslationConfiguration(WithoutBarriers,_,_) -> optimisationPhase1 |> RemoveBarriers

    let optimisedCrmInstructions = optimisationPhase2

    optimisedCrmInstructions, updatedTranslationState



/// Iterate through all of the translated versions of the function's instructions.
let ForTranslatedCrmInstructionsDo action translate thisFunc crmInstructions =

    // Kick off the whole thing here:

    crmInstructions |> List.iter (fun crmInstruction -> translate thisFunc crmInstruction |> List.iter action)

    // Handle the function's return (may need pop into A):

    let returnHandlingCode = 
        match thisFunc.FuncType |> ReturnsSingleValue with
            | true  -> translate thisFunc (Pop A)  // TODO: not ideal construction of temporary
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
let ForWasmTableDo writeOutData wasmTableHeading wasmTableRow i (t:InternalTableRecord) =

    match t.InitData.Length with

        | 0 -> ()

        | 1 ->
            wasmTableHeading i
                |> List.iter writeOutData

            t.InitData 
                |> Array.iter (fun elem ->
                    let ofsExpr, funcIdxList = elem
                    let ofsValue = StaticEvaluate ofsExpr
                    if ofsValue <> 0 then failwith "Cannot translate module with TableSec table that has Elem with non-zero data initialisation offset"
                    funcIdxList |> Array.iter (fun funcIdx -> 
                        writeOutData (wasmTableRow (FuncIdxNameString funcIdx))))

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
let ForTheDataInitialisationFunctionDo writeOutCopyBlockCode (mems:Memory[]) =

    let writeOutDataCopyCommand i (thisMem:InternalMemoryRecord) =
        if i<>0 then failwith "Cannot translate WASM module with more than one Linear Memory"
        thisMem.InitData |> Array.iteri (fun j elem ->
                let ofsExpr, byteArray = elem
                let ofsValue = StaticEvaluate ofsExpr
                writeOutCopyBlockCode i j ofsValue byteArray.Length
            )

    mems |> ForAllWasmMemsDo writeOutDataCopyCommand



/// Output the "thunk in" sequence to load the base register with the address
/// of the WASM memory data block.
let WriteThunkIn writeOutIns thisFunc translate =  // TODO: unfortunate amount of parameters.
    (translate thisFunc ThunkIn) |> List.iter writeOutIns



let ValTypeTranslationOf = function
    | I32Type -> "int"
    | I64Type -> failwith "Cannot translate I64 type with this simple translator"
    | F32Type -> failwith "Cannot translate F32 type with this simple translator"
    | F64Type -> failwith "Cannot translate F64 type with this simple translator"



/// Return the given function type's parameters and returns lists
/// in a format suitable for use in a comment.  The comment character
/// is not included here.
let FunctionSignatureAsComment (funcType:FuncType) =

    let translatedParameters = 
        funcType.ParameterTypes
            |> Array.mapi (fun i paramType ->
                sprintf "%s%d: %s" AsmLocalNamePrefix i (ValTypeTranslationOf paramType) )
            |> String.concat ", "

    let translatedReturns = 
        funcType.ReturnTypes
            |> Array.map ValTypeTranslationOf
            |> String.concat ", "

    (Bracketed translatedParameters) + (ColonPrefixed translatedReturns)



/// Returns a list of function local variables in a form suitable
/// for inclusion in a comment. The comment character
/// is not included here.
let FunctionLocalsAsComment (f:InternalFunctionRecord) =

    let funcType = f.FuncType
    let funcLocals = f.Locals
    let indexOfFirstLocal = funcType.ParameterTypes.Length
    funcLocals 
        |> Array.mapi (fun arrayIndex v ->
            let indexOfVariable = indexOfFirstLocal + arrayIndex
            sprintf "%s%d:%s" AsmLocalNamePrefix indexOfVariable (ValTypeTranslationOf v))
        |> String.concat ", "



/// Returns true if the function type has any parameters.
let HasParams funcType =
    funcType.ParameterTypes.Length > 0

/// Returns true if the function type has any parameters, or any locals.
let HasParametersOrLocals (func:InternalFunctionRecord) =
    func.Locals.Length > 0  ||  func.FuncType |> HasParams

/// Returns true if the function has local variables.
let HasLocals (func:InternalFunctionRecord) =
    func.Locals.Length > 0

/// Returns the number of local variables the function has.
let LocalsCount (func:InternalFunctionRecord) =
    uint32 (func.Locals.Length)



/// Returns true if the CRM instruction sequence calls out to anything, false 
/// if it makes no calls (is a leaf function).
let CrmInstructionsListMakesCallsOut crmInstructions =
    
    crmInstructions |> List.exists (fun crmInstruction ->
        match crmInstruction with
            | CallFunc _
            | CallTableIndirect _ -> true
            | _ -> false
    )

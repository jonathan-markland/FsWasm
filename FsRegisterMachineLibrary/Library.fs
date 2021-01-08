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



// TODO:  Array.toList  is used a lot, can we store lists primarily?



/// Returns true if any WASM memory object within the array has
/// any InitData blocks.
let HasAnyInitDataBlocks mems =
    
    let memHasInitData mem =
        match mem with
            | InternalMemory2 intMem -> intMem.InitData.Length > 0
            | ImportedMemory2 _ -> false

    mems |> Array.exists memHasInitData



let HexDumpList (command:string) (byteSeparator:string) (hexPrefix:string) (byteArray:byte[]) =

    let mutable accumulator = []

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
                accumulator <- (sb.ToString())::accumulator
                sb.Clear() |> ignore
            | _ -> ()
    )

    if sb.Length > 0 then 
        accumulator <- (sb.ToString())::accumulator

    accumulator |> List.rev



let WasmStartCode (writeOutBranchToEntryLabel:LABELNAME -> string list) toComment startOption moduleFuncsArray entryPointConfig : string list =

    let useFunc func =
        let labelName = FuncLabelFor func
        writeOutBranchToEntryLabel labelName

    let takesParameters intFunc =
        intFunc.FuncType.ParameterTypes.Length > 0

    match entryPointConfig with
    
        | WasmStartEntryPointIfPresent ->
            match startOption with 
                | Some func -> useFunc func
                | None -> [ "No WASM entry point (start record) in this translation" |> toComment ]

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

    let crmInstructions = 
        match f.FuncType |> ReturnsSingleValue with
            | true  -> 
                crmInstructions @ [Pop A]
            | false -> crmInstructions

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
let MapTranslatedCrmInstructions translate thisFunc crmInstructions : string list =
    crmInstructions 
        |> List.map (fun crmInstruction -> translate thisFunc crmInstruction)
        |> List.concat




/// Iterate all of the branch tables in the given function's instructions.
let MapBranchTablesList branchTableStart branchTableItem crmInstructions : string list =

    crmInstructions |> List.map (fun instruction ->

        match instruction with
            
            | GotoIndex(LabelName tableLabel,_,_,codePointLabels) ->
                let labels = 
                    codePointLabels 
                        |> Array.toList
                        |> List.map (fun (LabelName targetLabel) -> branchTableItem targetLabel)
                (branchTableStart tableLabel) @ labels

            | _ -> []
        )

        |> List.concat



/// Iterate wasm table heading and content.
let MapWasmTable wasmTableHeading wasmTableRow i (t:InternalTableRecord) : string list =

    match t.InitData.Length with

        | 0 -> []

        | 1 ->
            let tableHeading : string list =
                wasmTableHeading i

            let tableBody : string list =
                t.InitData 
                    |> Array.toList
                    |> List.map (fun elem ->
                        let ofsExpr, funcIdxList = elem
                        let ofsValue = StaticEvaluate ofsExpr
                        if ofsValue <> 0 then failwith "Cannot translate module with TableSec table that has Elem with non-zero data initialisation offset"
                        funcIdxList 
                            |> Array.toList
                            |> List.map (fun funcIdx -> wasmTableRow (FuncIdxNameString funcIdx)))
                    |> List.concat

            tableHeading @ tableBody

        | _ -> failwith "Cannot translate module with more than one Elem in a TableSec table"





/// Do actions with WASM memory and any initialisation data blocks.
let MapWasmMem1 wasmMemVar memIndex (thisMem:InternalMemoryRecord) =  // TODO: rename

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

    wasmMemVar memIndex linearMemorySize



/// Do actions with WASM memory and any initialisation data blocks.
let MapWasmMem2 wasmMemDataHeading wasmMemRow memIndex (thisMem:InternalMemoryRecord) =  // TODO: rename

    if thisMem.InitData.Length > 0 then
        let heading =
            wasmMemDataHeading memIndex
        let content = 
            thisMem.InitData 
                |> Array.toList
                |> List.mapi (fun dataBlockIndex (_, byteArray) -> 
                    wasmMemRow memIndex dataBlockIndex byteArray)
                |> List.concat
        heading @ content
    else
        []



/// Do the action for all WASM tables, raising exception if an 
/// imported table is seen, since these are not yet supported.
let MapAllWasmTables mapFunc tables =

    tables 
        |> Array.toList
        |> List.mapi (fun tableIndex t ->
            match t with
                | InternalTable2 tbl -> mapFunc tableIndex tbl
                | ImportedTable2 _   -> failwith "Error:  Cannot support importing a WASM 'table'.  WASM module must be self-contained."
            )



/// Do the action for all WASM globals, raising exception if an 
/// imported global is seen, since these are not yet supported.
let MapAllWasmGlobals mapFunc globals =

    globals 
        |> Array.toList
        |> List.mapi (fun globalIndex g ->
        match g with
            | InternalGlobal2 glo -> 
                let initValue = StaticEvaluate glo.InitExpr
                let globalIdx = GlobalIdx(U32(uint32 globalIndex))   // TODO: not ideal construction of temporary
                let globalIdxNameString = (GlobalIdxNameString globalIdx)
                mapFunc globalIdxNameString initValue
            | ImportedGlobal2 _ -> 
                failwith "Error:  Cannot support importing a WASM 'global'.  WASM module must be self-contained."
        )



/// Do the action for all WASM memories, raising exception if an 
/// imported memory is seen, since these are not yet supported.
let MapAllWasmMems mapFunc mems =

    mems 
        |> Array.toList
        |> List.mapi (fun memIndex me ->
        match me with
            | InternalMemory2 mem -> mapFunc memIndex mem
            | ImportedMemory2 _   -> failwith "Error:  Cannot support importing a WASM 'memory'.  WASM module must be self-contained."
        )


/// Generate the initialisation function that arranges the statically-initialised
/// data blocks in the memory space.
let DataInitialisationFunctionUsing copyBlockCode (mems:Memory[]) : string list =

    let dataCopyCode i (thisMem:InternalMemoryRecord) =
        if i<>0 then failwith "Cannot translate WASM module with more than one Linear Memory"
        thisMem.InitData 
            |> Array.toList
            |> List.mapi (fun j elem ->
                let ofsExpr, byteArray = elem
                let ofsValue = StaticEvaluate ofsExpr
                copyBlockCode i j ofsValue byteArray.Length
            )
            |> List.concat

    mems 
        |> MapAllWasmMems dataCopyCode
        |> List.concat



/// Output the "thunk in" sequence to load the base register with the address
/// of the WASM memory data block.
let WriteThunkIn thisFunc translate : string list =  // TODO: unfortunate amount of parameters.  Is this really worth it?
    translate thisFunc ThunkIn



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

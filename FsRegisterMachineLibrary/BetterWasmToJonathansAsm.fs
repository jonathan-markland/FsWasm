module BetterWasmToJonathansAsm

open WasmFileTypes
open WasmBetterTypes
open CommonRegisterMachineTypes
open AsmPrefixes
open WasmInstructionsToCRMInstructions
open Library
open TextFormatting
open BWToCRMConfigurationTypes
open OptimiseCommonRegisterMachine



let LabelCommand labelNameString = 
    labelNameString + ":"



let JonathansConditionCodeFor crmCondition = 
    match crmCondition with
        | CrmCondEq  -> "z"
        | CrmCondNe  -> "nz"
        | CrmCondLts -> "<"
        | CrmCondLtu -> "<<"
        | CrmCondGts -> ">"
        | CrmCondGtu -> ">>"
        | CrmCondLes -> "<="
        | CrmCondLeu -> "<<="
        | CrmCondGes -> ">="
        | CrmCondGeu -> ">>="



let TranslateInstructionToAsmSequence _thisFunc instruction =

    // TODO:  These translations can assume a 32-bit target for now.

    let regNameOf = function
        | A -> "A"
        | B -> "B"
        | C -> "C"
        | Y -> "Y"

    let offsetIfNeeded = function
        | U32 0u -> ""                   // indexed addressing not needed with zero offset
        | U32 n  -> "+" + n.ToString()   // indexed addressing needed

    let storeType = function
        | Stored8  -> "byte"
        | Stored16 -> "ushort"
        | Stored32 -> "uint"

    let fetchType = function
        | SignExt8  -> "sbyte"
        | ZeroExt8  -> "byte"
        | SignExt16 -> "short"
        | ZeroExt16 -> "ushort"
        | SignExt32 -> "uint" // TODO: Doesn't matter for now, but this stops diffs with previous code gen.

    let storeType = function
        | Stored8  -> "byte"
        | Stored16 -> "ushort"
        | Stored32 -> "uint"

    let translateREGU32 s1 r u s2 = 
        [ sprintf "%s%s%s%s" s1 (regNameOf r) (offsetIfNeeded u) s2 ]

    let translateREGU32I32 s1 r u s2 n = 
        [ sprintf "%s%s%s%s%d" s1 (regNameOf r) (offsetIfNeeded u) s2 n ]

    let translateStore t r ofs = 
        [ sprintf "let %s[%s%s]=A" (t |> storeType) (regNameOf r) (offsetIfNeeded ofs) ]

    let translateFetch t r ofs =
        [ sprintf "let A=%s[%s%s]" (t |> fetchType) (regNameOf r) (offsetIfNeeded ofs) ]

    let translateStoreConst t r u n = 
        [ sprintf "let %s[%s%s]=%d" (t |> storeType) (regNameOf r) (offsetIfNeeded u) n ]


    let translateGotoIndex (LabelName tableLabel) numMax (LabelName defaultLabel) =
        [
            // A is already the index to branch to
            sprintf "cmp A,%d:if >>= goto %s" numMax defaultLabel
            "shl A,logptr"
            sprintf "goto [A+%s]" tableLabel
        ]

    let translateCallTableIndirect () =
        // TODO: We really need to emit some code to validate the signatures.
        // A is already the index to call.
        // TODO: We need to validate index A lies within wasm table [0]
        [
            "shl A,logptr"
            sprintf "goto [A+%s0]" AsmTableNamePrefix  // WASM 1.0 always looks in table #0
        ]

    let translateSecondaryCmpBranch condition (LabelName targetLabel) =
        let compareInstruction = sprintf "cmp B,A:if %s goto " (JonathansConditionCodeFor condition)
        [ compareInstruction + targetLabel ]

    let toMathMnemonic = function
        | AddRN -> "add"
        | SubRN -> "sub"
        | AndRN -> "and"
        | OrRN  -> "or"
        | XorRN -> "xor"

    let calcInstruction = function
        | AddRbRa -> "add"
        | SubRbRa -> "sub"
        | MulRbRa -> "mul"
        | AndRbRa -> "and"
        | OrRbRa  -> "or"
        | XorRbRa -> "xor"
        | DivsRbRa
        | DivuRbRa
        | RemsRbRa
        | RemuRbRa -> failwith "Division or remainder instructions not yet translated"

    let shiftInstruction = function
        | Shl    -> "shl"
        | Shrs   -> "sar"
        | Shru   -> "shr"
        | Rotl 
        | Rotr   -> failwith "Rotate instructions are not yet translated"

    let toZNZMnemonic = function
        | BZero     -> "z"
        | BNonZero  -> "nz"

    match instruction with
        | Barrier                     -> [ "// ~~~ register barrier ~~~" ]
        | Breakpoint                  -> [ "break" ]
        | Drop(U32 numSlots)          -> [ sprintf "add SP,%d" (numSlots * 4u) ]   // TODO: Assumes 32-bit target
        | Label(LabelName l)          -> [ "label " + l ]   // TODO: sort out using the local label references in Jonathan's ASM
        | Const(r,Const32(n))         -> [ sprintf "let %s=%d" (regNameOf r) n ]
        | Goto(LabelName l)           -> [ "goto " + l ]
        | CallFunc(LabelName l)       -> [ "call " + l ]
        | CallTableIndirect           -> translateCallTableIndirect ()
        | BranchRegZNZ(A,c,LabelName l) -> [ sprintf "cmp A,0:if %s goto %s" (c |> toZNZMnemonic) l ]
        | BranchRegZNZ _              -> failwith "Cannot translate branch"
        | GotoIndex(t,n,d,_)          -> translateGotoIndex t n d   // The ignored parameter is the lookup table, which we separately output.
        | Push(r)                     -> [ sprintf "push %s" (regNameOf r) ]
        | Pop(r)                      -> [ sprintf "pop %s" (regNameOf r) ]
        | PeekA                       -> [ "let A=int [SP]" ]  // TODO: Assumes 32-bit target
        | Let(r1,r2)                  -> [ sprintf "let %s=%s" (regNameOf r1) (regNameOf r2) ]
        | CalcWithConst(ins,A,I32(n)) -> [ sprintf "%s A,%d" (ins |> toMathMnemonic) n ]
        | CalcWithConst _             -> failwith "Cannot translate calculation with constant"
        | CalcRegReg(ins,r1,r2)       -> [ sprintf "%s %s,%s" (ins |> calcInstruction) (regNameOf r1) (regNameOf r2) ]
        | ShiftRot ins                -> [ sprintf "%s B,C" (ins |> shiftInstruction) ]
        | CmpBA crmCond               -> [ sprintf "cmp B,A:set %s A" (JonathansConditionCodeFor crmCond) ]
        | CmpAZ                       -> [ "cmp A,0:set z A" ]
        | FetchLoc(r,i)               -> [ sprintf "let %s=int[@%s]" (regNameOf r) (LocalIdxNameString i) ]  // TODO: Assumes 32-bit target
        | StoreLoc(r,i)               -> [ sprintf "let int[@%s]=%s" (LocalIdxNameString i) (regNameOf r) ]  // TODO: Assumes 32-bit target
        | FetchGlo(r,i)               -> [ sprintf "let %s=int[%s]" (regNameOf r) (GlobalIdxNameString i) ]  // TODO: Eventually use the type rather than "int"
        | StoreGlo(r,i)               -> [ sprintf "let int[%s]=%s" (GlobalIdxNameString i) (regNameOf r) ]  // TODO: Eventually use the type rather than "int"
        | StoreConst(t,r,ofs,I32 v)   -> translateStoreConst t r ofs v
        | Store(A,t,r,ofs)            -> translateStore t r ofs
        | Store _                     -> failwith "Cannot translate store instruction"
        | Fetch(A,t,r,ofs)            -> translateFetch t r ofs
        | Fetch _                     -> failwith "Cannot translate fetch instruction"
        | ThunkIn              -> 
            // The translated code requires the Y register to
            // point to the base of the linear memory region.
            // Note: There is only *one* linear memory supported in WASM 1.0  (mem #0)
            [ sprintf "let Y=%s%d" AsmMemPrefix 0 ]

        | SecondaryCmpBranch (condition, targetLabel) -> 
            translateSecondaryCmpBranch condition targetLabel
        
        | X8632Specific _ -> failwith "Unexpected usage of X86/32 optimisation!"



let ValTypeTranslationOf = function
    | I32Type -> "int"
    | I64Type -> failwith "Cannot translate I64 type with this simple translator"
    | F32Type -> failwith "Cannot translate F32 type with this simple translator"
    | F64Type -> failwith "Cannot translate F64 type with this simple translator"


let FunctionLocals (funcType:FuncType) funcLocals : string list =

    let indexOfFirstLocal = funcType.ParameterTypes.Length

    funcLocals 
        |> Array.toList
        |> List.mapi (fun arrayIndex v ->
            let indexOfVariable = indexOfFirstLocal + arrayIndex
            let prefixStr = 
                match arrayIndex with 
                    | 0 -> "var "
                    | _ -> "  , "
            sprintf "%s@%s%d:%s" prefixStr AsmLocalNamePrefix indexOfVariable (ValTypeTranslationOf v))



let WriteOutFunctionAndBranchTables writeOutCode writeOutTables funcIndex (m:Module) translationState config (f:InternalFunctionRecord) =   // TODO: module only needed to query function metadata in TranslateInstructions

    let wasmToCrmTranslationConfig = 
        { ClearParametersAfterCall = false } 

    let crmInstructions, updatedTranslationState = 
        TranslateInstructionsAndApplyOptimisations
            f m.Funcs translationState wasmToCrmTranslationConfig config id

    let asmSignatureOf (funcType:FuncType) =

        let atParamDecls (ps:ValType[]) =
            String.concat ", " (ps |> Array.mapi (fun i t -> (sprintf "@%s%d" AsmLocalNamePrefix i)))

        let atParamsString = 
            atParamDecls funcType.ParameterTypes

        (Bracketed atParamsString) + "  // " + (FunctionSignatureAsComment funcType)

    let procedureCommand = 
        sprintf "procedure %s%d%s" AsmInternalFuncNamePrefix funcIndex (asmSignatureOf f.FuncType)

    let writeInstruction instructionText = 
        writeOutCode ("    " + instructionText)

    let branchTableStart tableLabel =
        [
            "align ptr"
            sprintf "data %s" tableLabel
        ]

    let branchTableItem targetLabel =
        sprintf "    ptr %s" targetLabel

    let returnCommandFor (funcType:FuncType) (funcLocals:ValType[]) =
        match (funcType.ParameterTypes.Length, funcLocals.Length) with
            | (0,0) -> "ret"
            | (_,_) -> "endproc"

    try
        procedureCommand
            |> writeOutCode
        FunctionLocals f.FuncType f.Locals
            |> List.iter writeOutCode
        crmInstructions 
            |> MapTranslatedCrmInstructions TranslateInstructionToAsmSequence f
            |> List.iter writeInstruction
        returnCommandFor f.FuncType f.Locals
            |> writeOutCode
        crmInstructions
            |> MapBranchTablesList branchTableStart branchTableItem
            |> List.iter writeOutTables
    with
        | _ as ex -> failwith (sprintf "Error in %s:  %s" procedureCommand (ex.ToString()))

    updatedTranslationState



let branchToEntryLabel mems (LabelName labelName) =
    ["procedure " + AsmEntryPointLabel]
    @ if mems |> HasAnyInitDataBlocks then [sprintf "call %s" AsmInitMemoriesFuncName] else []
    @ WriteThunkIn TheInitialisationFunctionMetadata TranslateInstructionToAsmSequence
    @ [sprintf "goto %s" labelName]



let JonathansAsmDataInitialisation mems =

    let copyBlockCode i j ofsValue byteArrayLength =
        [
            sprintf "    let Y=%s%d+%d" AsmMemPrefix i ofsValue
            sprintf "    let X=%s%d_%d" AsmMemoryNamePrefix i j
            sprintf "    let C=%d" byteArrayLength
            "    cld rep movsb"
        ]

    if mems |> HasAnyInitDataBlocks then
        [ "procedure " + AsmInitMemoriesFuncName ]
        @ (mems |> DataInitialisationFunctionUsing copyBlockCode)
        @ [ "ret" ]
    else
        []



let WriteOutBetterWasmAsJonathansAssemblerText config headingText _writeOutHead writeOutData writeOutCode writeOutVar (m:Module) =   // TODO: rename because write out to text???

    // Start outputting ASM language text:

    let toComment commentText = 
        ("// " + commentText)

    let wasmTableHeading tableIndex =
        [
            "align ptr"
            sprintf "data %s%d" AsmTableNamePrefix tableIndex
        ]

    let wasmTableRow nameString =
        sprintf "    ptr %s" nameString

    let wasmMemVar memIndex linearMemorySize =
        [
            "global"
            "    align ptr"
            sprintf "    %s%d: %d" AsmMemPrefix memIndex linearMemorySize
        ]

    let wasmMemDataHeading memIndex =
        [ sprintf "// Data for WASM mem %s%d" AsmMemoryNamePrefix memIndex ]

    let wasmMemRow memIndex dataBlockIndex (byteArray:byte[]) : string list =
        [ sprintf "data %s%d_%d" AsmMemoryNamePrefix memIndex dataBlockIndex ]
        @ (HexDumpList "    byte" "," "0x" byteArray)

    let wasmGlobal globalIdxNameString initValue =
        // TODO: We do nothing with the immutability information.  Could we avoid a store and hoist the constant into the code?
        [ sprintf "data %s int %d" globalIdxNameString initValue ]

    // --- Start ---

    ("Translation of WASM module: " + headingText) |> toComment |> writeOutData
    writeOutData ""

    m.Tables
        |> MapAllWasmTables (MapWasmTable wasmTableHeading wasmTableRow)
        |> List.concat
        |> List.iter writeOutData
    
    m.Globals 
        |> MapAllWasmGlobals wasmGlobal
        |> List.concat
        |> List.iter writeOutData

    m.Mems    
        |> MapAllWasmMems (MapWasmMem1 wasmMemVar)
        |> List.concat
        |> List.iter writeOutVar

    m.Mems    
        |> MapAllWasmMems (MapWasmMem2 wasmMemDataHeading wasmMemRow)
        |> List.concat
        |> List.iter writeOutData

    m.Mems 
        |> JonathansAsmDataInitialisation 
        |> List.iter writeOutCode

    let mutable moduleTranslationState = ModuleTranslationState(0)  // TODO: hide ideally

    m.Funcs |> Array.iteri (fun i g ->
        match g with 

            | InternalFunction2(g) -> 
                moduleTranslationState <- 
                    g |> WriteOutFunctionAndBranchTables writeOutCode writeOutData i m moduleTranslationState config

            | ImportedFunction2({Import={ImportModuleName=m; ImportName=n}}) ->
                (sprintf "WASM Import: %s.%s" m n) |> toComment |> writeOutCode 
        )

    let (TranslationConfiguration (_,_,entryPointConfig)) = config
    
    WasmStartCode (branchToEntryLabel m.Mems) toComment m.Start m.Funcs entryPointConfig
        |> List.iter writeOutCode

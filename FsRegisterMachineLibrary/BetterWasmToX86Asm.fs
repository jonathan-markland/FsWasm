module BetterWasmToX86Asm

open System.Text
open WasmFileTypes
open WasmBetterTypes
open CommonRegisterMachineTypes
open AsmPrefixes
open WasmInstructionsToCRMInstructions
open Library
open BWToCRMConfigurationTypes
open OptimiseCommonRegisterMachine



let StackSlotSizeU = 4u
let LabelCommand labelNameString = labelNameString + ":"
let CodeAlign = "align 16"



let X86ConditionCodeFor crmCondition = 
    match crmCondition with
        | CrmCondEq  -> "z "   // (We can probably get away with the spaces)
        | CrmCondNe  -> "nz"
        | CrmCondLts -> "l "
        | CrmCondLtu -> "b "
        | CrmCondGts -> "g "
        | CrmCondGtu -> "a "
        | CrmCondLes -> "le"
        | CrmCondLeu -> "be"
        | CrmCondGes -> "ge"
        | CrmCondGeu -> "ae"


let X86CalcInstruction ins =
    match ins with
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


let X86ShiftInstruction ins =
    match ins with
        | Shl    -> "shl"
        | Shrs   -> "sar"
        | Shru   -> "shr"
        | Rotl 
        | Rotr   -> failwith "Rotate instructions are not yet translated"



let TranslateInstructionToAsmSequence thisFunc instruction =

    // TODO:  These translations can assume a 32-bit target for now.

    let regNameOf = function
        | A -> "EAX"
        | B -> "EBX"
        | C -> "ECX"
        | Y -> "EDI"

    let offsetIfNeeded = function
        | U32 0u -> ""                   // indexed addressing not needed with zero offset
        | U32 n  -> "+" + n.ToString()   // indexed addressing needed

    let storeType = function
        | Stored8  -> "byte"
        | Stored16 -> "word"
        | Stored32 -> "dword"

    let toSXZX = function
        | SignExt8  -> "sx"
        | ZeroExt8  -> "zx"
        | SignExt16 -> "sx"
        | ZeroExt16 -> "zx"
        | SignExt32 -> ""

    let fetchType = function
        | SignExt8  -> "byte"
        | ZeroExt8  -> "byte"
        | SignExt16 -> "word"
        | ZeroExt16 -> "word"
        | SignExt32 -> "dword"

    let accumulatorByStoreType = function
        | Stored8  -> "AL"
        | Stored16 -> "AX"
        | Stored32 -> "EAX"

    let translateStore t r ofs = 
        [ sprintf "mov [%s%s],%s" (regNameOf r) (offsetIfNeeded ofs) (t |> accumulatorByStoreType) ]

    let translateFetch t r ofs =
        [ sprintf "mov%s EAX, %s [%s%s]" (t |> toSXZX) (t |> fetchType) (regNameOf r) (offsetIfNeeded ofs) ]

    let translateREGU32 s1 r u s2 =  // TODO: re-do this horrible named function 
        [ sprintf "%s%s%s%s" s1 (regNameOf r) (offsetIfNeeded u) s2 ]

    let translateStoreConst t r u n = 
        [ sprintf "mov %s [%s%s],%d" (t |> storeType) (regNameOf r) (offsetIfNeeded u) n ]

    let translateGotoIndex (LabelName tableLabel) numMax (LabelName defaultLabel) =
        [
            // A is already the index to branch to
            sprintf "cmp EAX,%d" numMax
            sprintf "jae %s" defaultLabel
            sprintf "jmp [%s+EAX*4]" tableLabel
        ]

    let translateCallTableIndirect () =
        // TODO: We really need to emit some code to validate the signatures.
        // A is already the index to call.
        // TODO: We need to validate index A lies within wasm table [0]
        [
            sprintf "jmp [%s0+EAX*4]" AsmTableNamePrefix  // WASM 1.0 always looks in table #0
        ]

    /// Return the X86 EBP offset for the given local variable 
    /// slot number,which includes the function's parameters.
    let frameOffsetForLoc (LocalIdx(U32(locNumber))) =

        // Loc0    |               [EBP+16]
        // Loc1    | Params        [EBP+12]
        // Loc2    |               [EBP+8]
        // Return Address          [EBP+4]
        // Parent EBP         <--- [EBP]
        // Loc3    | Locals        [EBP-4]
        // Loc4    |               [EBP-8]

        let paramCount = uint32 thisFunc.FuncType.ParameterTypes.Length
        if locNumber < paramCount then
            "+" + ((paramCount - locNumber) * StackSlotSizeU + StackSlotSizeU).ToString()
        else
            "-" + ((locNumber - paramCount) * StackSlotSizeU + StackSlotSizeU).ToString()

    let translateSecondaryCmpBranch condition (LabelName targetLabel) =
        let branchInstruction = sprintf "j%s " (X86ConditionCodeFor condition)
        [ "cmp EBX,EAX" ; (branchInstruction + targetLabel) ]

    let toMathMnemonic = function
        | AddRN -> "add"
        | SubRN -> "sub"
        | AndRN -> "and"
        | OrRN  -> "or"
        | XorRN -> "xor"

    let toZNZMnemonic = function
        | BZero     -> "z"
        | BNonZero  -> "nz"


    match instruction with
        | Barrier                    -> [ "; ~~~ register barrier ~~~" ]
        | Breakpoint                 -> [ "int 3" ]
        | Drop(U32 numSlots)         -> [ sprintf "add ESP,%d" (numSlots * StackSlotSizeU) ]
        | Label(LabelName l)         -> [ LabelCommand l ]
        | Const(r,Const32(n))        -> [ sprintf "mov %s,%d" (regNameOf r) n ]
        | Goto(LabelName l)          -> [ ("jmp " + l) ; CodeAlign ]
        | CallFunc(LabelName l)      -> [ "call " + l ]
        | CallTableIndirect          -> translateCallTableIndirect ()
        | BranchRegZNZ(A,c,LabelName l) -> [ "cmp EAX,0" ; (sprintf "j%s %s" (c |> toZNZMnemonic) l) ]
        | BranchRegZNZ _                -> failwith "Cannot translate branch"
        | GotoIndex(t,n,d,_)         -> translateGotoIndex t n d   // The ignored parameter is the lookup table, which we separately output.
        | Push r                     -> [ sprintf "push %s" (regNameOf r) ]
        | Pop r                      -> [ sprintf "pop %s" (regNameOf r) ]
        | PeekA                      -> [ "mov EAX,[ESP]" ]
        | Let(r1,r2)                 -> [ sprintf "mov %s,%s" (regNameOf r1) (regNameOf r2) ]
        | CalcWithConst(ins,A,I32 n) -> [ sprintf "%s EAX,%d" (ins |> toMathMnemonic) n ]
        | CalcWithConst _            -> failwith "Cannot translate calculation with constant"
        | CalcRegReg(ins,r1,r2)      -> [ sprintf "%s %s,%s" (ins |> X86CalcInstruction) (regNameOf r1) (regNameOf r2) ]
        | ShiftRot ins               -> [ sprintf "%s EBX,CL" (ins |> X86ShiftInstruction) ]
        | CmpBA crmCond              -> [ "cmp EBX,EAX" ; (sprintf "set%s AL" (X86ConditionCodeFor crmCond)) ; "movzx EAX,AL" ]
        | CmpAZ                      -> [ "cmp EAX,0"   ; "setz AL"  ; "movzx EAX,AL" ]
        | FetchLoc(r,i)              -> [ sprintf "mov %s,[EBP%s]  ; @%s" (regNameOf r) (frameOffsetForLoc i) (LocalIdxNameString i) ]  // TODO: Assumes 32-bit target
        | StoreLoc(r,i)              -> [ sprintf "mov [EBP%s],%s  ; @%s" (frameOffsetForLoc i) (regNameOf r) (LocalIdxNameString i) ]  // TODO: Assumes 32-bit target
        | FetchGlo(r,i)              -> [ sprintf "mov %s,[%s]" (regNameOf r) (GlobalIdxNameString i) ]  // TODO: Eventually use the type rather than "int"
        | StoreGlo(r,i)              -> [ sprintf "mov [%s],%s" (GlobalIdxNameString i) (regNameOf r) ]  // TODO: Eventually use the type rather than "int"
        | StoreConst(t,r,ofs,I32 v)  -> translateStoreConst t r ofs v
        | Store(A,t,r,ofs)           -> translateStore t r ofs
        | Store _                    -> failwith "Cannot translate store instruction"
        | Fetch(A,t,r,ofs)           -> translateFetch t r ofs
        | Fetch _                    -> failwith "Cannot translate fetch instruction"
        | ThunkIn                    -> 
            // The translated code requires the Y register to
            // point to the base of the linear memory region.
            // Note: There is only *one* linear memory supported in WASM 1.0  (mem #0)
            [ sprintf "mov EDI,%s%d" AsmMemPrefix 0 ]

        | SecondaryCmpBranch (condition, targetLabel) -> 
            translateSecondaryCmpBranch condition targetLabel

        | X8632Specific instruction ->
            match instruction with
                
                | X8632PushConstant (Const32 value) ->
                    [ sprintf "push %d" value ] 

                | X8632StoreAatEBXplusEDIplusOffset (ofs, regName) ->
                    translateREGU32 "mov [EDI+" B ofs ("]," + regName)

                | X8632OperateOnLocal32 (opcode, locIdx, I32 value) ->
                    [ sprintf "%s dword [EBP%s],%d  ; @%s" opcode (frameOffsetForLoc locIdx) value (LocalIdxNameString locIdx) ] 


let WriteOutFunctionAndBranchTables writeOutCode writeOutTables funcIndex (m:Module) translationState config (f:InternalFunctionRecord) =   // TODO: module only needed to query function metadata in TranslateInstructions

    let wasmToCrmTranslationConfig = 
        { ClearParametersAfterCall = true } 

    let crmInstructions, updatedTranslationState = 
        TranslateInstructionsAndApplyOptimisations
            f m.Funcs translationState wasmToCrmTranslationConfig config OptimiseX8632

    let functionUsesStackPointerAtAddress4 =
        crmInstructions |> CrmInstructionsUsesShadowStackAtAddress4  // TODO: Ideal to collect this up front but we don't have all the translated function bodies to hand when the file header is written out.

    let functionExportNameIfPresent = 
        match f.Export with
            | Some export -> "export " + export.ExportName
            | None -> "~no export name~"

    let procedureCommand =
        sprintf "%s%d:  ; %s%s" AsmInternalFuncNamePrefix funcIndex functionExportNameIfPresent (FunctionSignatureAsComment f.FuncType)

    let labelAndPrologueCode f =
        ["" ; CodeAlign ; procedureCommand]
        @
        if f |> HasParametersOrLocals then
            [ "push EBP" ;  "mov EBP,ESP" ]
            @
            if f |> HasLocals then
                [sprintf "sub ESP,%d  ; %s" ((f |> LocalsCount) * StackSlotSizeU) (f |> FunctionLocalsAsComment)]
            else
                []
        else
            []

    let epilogueCode f =
        if f |> HasParametersOrLocals then
            if f |> HasLocals then ["mov ESP,EBP"] else []
            @ ["pop EBP"]
        else
            []

    let writeInstruction instructionText = 
        writeOutCode ("    " + instructionText)

    let branchTableStart tableLabel =
        [
            sprintf "align %d" StackSlotSizeU
            LabelCommand tableLabel
        ]

    let branchTableItem targetLabel =
        sprintf "    dd %s" targetLabel

    try
        labelAndPrologueCode f 
            |> List.iter writeOutCode
        crmInstructions 
            |> MapTranslatedCrmInstructions TranslateInstructionToAsmSequence f
            |> List.iter writeInstruction
        epilogueCode f 
            |> List.iter writeOutCode
        "ret"
            |> writeOutCode
        crmInstructions 
            |> MapBranchTablesList branchTableStart branchTableItem
            |> List.iter writeOutTables
    with
        | _ as ex -> failwith (sprintf "Error in %s:  %s" procedureCommand (ex.ToString()))

    (updatedTranslationState , functionUsesStackPointerAtAddress4)



let branchToEntryLabel mems (LabelName labelName) =
    [
        CodeAlign
        LabelCommand AsmEntryPointLabel
        "pushad"// TODO: In lieu of finding out about the caller's convention.
    ]
    @
    if mems |> HasAnyInitDataBlocks then [sprintf "call %s" AsmInitMemoriesFuncName] else []
    @
    WriteThunkIn TheInitialisationFunctionMetadata TranslateInstructionToAsmSequence
    @
    [
        sprintf "call %s" labelName
        "popad"
        "ret"
    ]



let FilePrologue =
    [
        // TODO: temporary scaffold:
        "format binary"
        "use32"
        "org 0x40000000"
        "db 'F','#','F','X'    ; Indicates Jonathan's F# Web Assembly project executable file  (Fixed address executable)"
        "db 'I','A','3','2'    ; Indicates this is for X86/32"
        "dq 1                  ; File Version"
        "dq 0x40000000         ; Origin address for this fixed executable."
        "dq TotalSize          ; Total size needed for this fixed flat image"
        "dq wasm_entry         ; Entry point address"  // TODO: If using WasmStartEntryPointIfPresent this will fail to resolve since the entry is optional.
        sprintf "dq %s%d          ; Address of WASM linear memory" AsmMemPrefix 0
    ]

let FileEpilogue moduleUsesStackPointerAtAddress4 =
    [
        sprintf "dd %s                   ; 1=uses stack pointer at WASM linear memory address 4"  (if moduleUsesStackPointerAtAddress4 then "1" else "0")
    ]



let X86DataInitialisation mems =

    let x8632CopyBlockCode i j ofsValue byteArrayLength =
        [
            sprintf "    mov EDI,(%s%d+%d)" AsmMemPrefix i ofsValue
            sprintf "    mov ESI,%s%d_%d" AsmMemoryNamePrefix i j
            sprintf "    mov ECX,%d" byteArrayLength
            "    cld"
            "    rep movsb"
        ]

    if mems |> HasAnyInitDataBlocks then
        [
            CodeAlign
            LabelCommand AsmInitMemoriesFuncName
        ]
        @ (mems |> DataInitialisationFunctionUsing x8632CopyBlockCode)
        @ 
        [ 
            "ret" 
        ]
    else
        []



let WriteOutBetterWasmAsX86AssemblerText config headingText writeFileHeader writeOutData writeOutCode writeOutVar (m:Module) =   // TODO: rename because write out to text???

    // Start outputting ASM language text:

    let toComment commentText = 
        ("; " + commentText)

    let wasmTableHeading tableIndex =
        [
            sprintf "align %d" StackSlotSizeU
            LabelCommand (sprintf "%s%d" AsmTableNamePrefix tableIndex)
        ]

    let wasmTableRow nameString =
        sprintf "    dd %s" nameString

    let wasmMemVar memIndex linearMemorySize =
        [
            sprintf "align %d" StackSlotSizeU
            sprintf "%s%d: rb %d ; WASM linear memory reservation" AsmMemPrefix memIndex linearMemorySize
        ]

    let wasmMemDataHeading memIndex =
        [ sprintf "; Data for WASM mem %s%d" AsmMemoryNamePrefix memIndex ]

    let wasmMemRow memIndex dataBlockIndex byteArray =
        [ LabelCommand (sprintf "%s%d_%d" AsmMemoryNamePrefix memIndex dataBlockIndex) ]
        @ (HexDumpList "    db" "," "0x" byteArray)

    let wasmGlobal globalIdxNameString initValue =
        [
            LabelCommand globalIdxNameString
            sprintf "dd %d" initValue
        ]

    // --- Start ---

    ("Translation of WASM module: " + headingText) |> toComment |> writeFileHeader

    FilePrologue
        |> List.iter writeFileHeader

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
    
    LabelCommand "TotalSize"
        |> writeOutVar

    m.Mems 
        |> X86DataInitialisation 
        |> List.iter writeOutCode

    let mutable moduleTranslationState = ModuleTranslationState(0)  // TODO: hide ideally
    let mutable moduleUsesStackPointerAtAddress4 = false

    m.Funcs |> Array.iteri (fun i g ->  // TODO: Should this be in the library?
        match g with 

            | InternalFunction2(g) -> 
                let newState, usesStackPointerAtAddress4 =
                    g |> WriteOutFunctionAndBranchTables writeOutCode writeOutData i m moduleTranslationState config
                moduleTranslationState <- newState
                moduleUsesStackPointerAtAddress4 <- moduleUsesStackPointerAtAddress4 || usesStackPointerAtAddress4

            | ImportedFunction2({Import={ImportModuleName=m; ImportName=n}}) ->
                (sprintf "WASM Import: %s.%s" m n) |> toComment |> writeOutCode  // TODO: Do we really support this at this time?
        )

    let (TranslationConfiguration (_,_,entryPointConfig)) = config
    
    WasmStartCode (branchToEntryLabel m.Mems) toComment m.Start m.Funcs entryPointConfig
        |> List.iter writeOutCode

    FileEpilogue moduleUsesStackPointerAtAddress4
        |> List.iter writeFileHeader




let TranslateBetterWasmToX86AssemblerStdOut config headingText (m:Module) =

    // TODO: I didn't really like having this front-end on, but it is
    //       better to have this in the translation module than outside.

    let headerStringBuilder = new StringBuilder ()
    let dataStringBuilder = new StringBuilder ()
    let varStringBuilder  = new StringBuilder ()
    let codeStringBuilder = new StringBuilder ()

    let writeFileHeader s = headerStringBuilder.AppendLine(s) |> ignore
    let writeOutData s = dataStringBuilder.AppendLine(s) |> ignore
    let writeOutVar  s = varStringBuilder.AppendLine(s)  |> ignore
    let writeOutCode s = codeStringBuilder.AppendLine(s) |> ignore

    WriteOutBetterWasmAsX86AssemblerText config headingText writeFileHeader writeOutData writeOutCode writeOutVar m

    printf "%s" (headerStringBuilder.ToString())
    printf "%s" (dataStringBuilder.ToString())
    printf "%s" (codeStringBuilder.ToString())
    printf "%s" (varStringBuilder.ToString())



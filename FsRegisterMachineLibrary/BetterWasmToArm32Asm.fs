module BetterWasmToArm32Asm

open System.Text
open WasmFileTypes
open WasmBetterTypes
open CommonRegisterMachineTypes
open AsmPrefixes
open WasmInstructionsToCRMInstructions
open Library
open ArmSupportLibrary
open BWToCRMConfigurationTypes
open OptimiseCommonRegisterMachine



let StackSlotSizeU = 4u
let armTempRegister = "R8"
let offsetTempRegister = "R10"
let LabelCommand labelNameString = labelNameString + ":"
let CodeAlign = "align 4"
let AsmArmBlockCopyLabel = "wasm_arm_block_copy"



let TranslateInstructionToAsmSequence thisFunctionCallsOut thisFunc instruction =

    // TODO:  These translations can assume an ArmV7 target for now.

    let regNameOf = function
        | A -> "R0"
        | B -> "R1"
        | C -> "R2"
        | Y -> "R9"  // Conventionally the "Static base" register

    let loadConstant r value =
        LoadConstantInto (regNameOf r) value

    let loadStoreRegOffset fetchStoreType loadStoreInstruction addressReg offsetDesired =
        LoadStoreRegOffset fetchStoreType loadStoreInstruction (regNameOf addressReg) offsetDesired offsetTempRegister

    let storeConstant fetchStoreType armStoreInstruction addressReg offsetDesired value = 
        StoreConstant fetchStoreType armStoreInstruction (regNameOf addressReg) offsetDesired value offsetTempRegister armTempRegister

    let translateGotoIndex (LabelName tableLabel) numMax (LabelName defaultLabel) =
        // A is already the index to branch to
        (MathsWithConstant "cmp" "R0" numMax armTempRegister) @  // ie: cmp R0,numMax
        [
            sprintf "bhs %s" defaultLabel
            sprintf "jmp [%s+R0*4]" tableLabel
        ]

    let translateCallTableIndirect () =
        // TODO: We really need to emit some code to validate the signatures.
        // A is already the index to call.
        // TODO: We need to validate index A lies within wasm table [0]
        [ sprintf "jmp [%s0+R0*4]" AsmTableNamePrefix ] // WASM 1.0 always looks in table #0 

    (* TODO: consider:
        ldr r0, [pc, #xx] ; My constant
        add r0, r0, r0 ; try to use r0 straight away, incurring a 3 cycle wait on use of r0    
    *)

    /// Return the R11 offset for the given local variable 
    /// slot number,which includes the function's parameters.
    let frameOffsetForLoc (LocalIdx(U32(locNumber))) =

        // When thisFunctionCallsOut:

            // Loc0    |               [R11+16]
            // Loc1    | Params        [R11+12]
            // Loc2    |               [R11+8]
            // Return Address          [R11+4]    <--- This is where we preserve the link register R14
            // Parent R11         <--- [R11]
            // Loc3    | Locals        [R11-4]
            // Loc4    |               [R11-8]

        // When NOT thisFunctionCallsOut:    (No link register preservation needed for leaf function).

            // Loc0    |               [R11+12]
            // Loc1    | Params        [R11+8]
            // Loc2    |               [R11+4]
            // Parent R11         <--- [R11]
            // Loc3    | Locals        [R11-4]
            // Loc4    |               [R11-8]

        let linkRegisterSlotSize =
            if thisFunctionCallsOut then StackSlotSizeU else 0u

        let paramCount = uint32 thisFunc.FuncType.ParameterTypes.Length
        if locNumber < paramCount then
            "+" + ((paramCount - locNumber) * StackSlotSizeU + linkRegisterSlotSize).ToString()
        else
            "-" + ((locNumber - paramCount) * StackSlotSizeU + StackSlotSizeU).ToString()


    let translateSecondaryCmpBranch condInstruction (LabelName targetLabel) =
        let branchInstruction =
            match condInstruction with
            | CmpEqBA           -> "beq "
            | CmpNeBA           -> "bne "
            | CmpLtsBA          -> "blt "
            | CmpLtuBA          -> "blo "
            | CmpGtsBA          -> "bgt "
            | CmpGtuBA          -> "bhi "
            | CmpLesBA          -> "ble "
            | CmpLeuBA          -> "bls "
            | CmpGesBA          -> "bge "
            | CmpGeuBA          -> "bhs "
            | _ -> failwith "Expected a compare instruction for compare-and-branch."
        [ "cmp R1,R0" ; (branchInstruction + targetLabel) ]


    match instruction with
        | Barrier               -> [ "; ~~~ register barrier ~~~" ]
        | Breakpoint            -> [ "bkpt #0" ]
        | Drop(U32 numSlots)    -> [ sprintf "add R13,R13,#%d" (numSlots * StackSlotSizeU) ]
        | Label(LabelName l)    -> [ LabelCommand l ]
        | Const(r,Const32(n))   -> loadConstant r (uint32 n)
        | Goto(LabelName l)     -> [ "b " + l ]
        | CallFunc(LabelName l) -> [ "bl " + l ]
        | CallTableIndirect     -> translateCallTableIndirect ()
        | BranchAZ(LabelName l) -> [ "cmp R0,#0" ; "beq " + l ]
        | BranchANZ(LabelName l)-> [ "cmp R0,#0" ; "bne " + l ]
        | GotoIndex(t,n,d,_)    -> translateGotoIndex t (uint32 n) d   // The ignored parameter is the lookup table, which we separately output.
        | Push(r)               -> [ sprintf "push {%s}" (regNameOf r) ]
        | Pop(r)                -> [ sprintf "pop {%s}" (regNameOf r) ]
        | PeekA                 -> [ "ldr R0,[R13]" ]
        | Let(r1,r2)            -> [ sprintf "mov %s,%s" (regNameOf r1) (regNameOf r2) ]
        | AddAN(I32(n))         -> MathsWithConstant "add" "R0" (uint32 n) armTempRegister
        | SubAN(I32(n))         -> MathsWithConstant "sub" "R0" (uint32 n) armTempRegister
        | AndAN(I32(n))         -> MathsWithConstant "and" "R0" (uint32 n) armTempRegister
        | OrAN(I32(n))          -> MathsWithConstant "orr" "R0" (uint32 n) armTempRegister
        | XorAN(I32(n))         -> MathsWithConstant "eor" "R0" (uint32 n) armTempRegister
        | Add(r1,r2)            -> [ sprintf "add %s,%s,%s" (regNameOf r1) (regNameOf r1) (regNameOf r2) ]
        | SubBA                 -> [ "sub R1,R1,R0" ]  // TODO: ARM could put result in R0
        | MulAB                 -> [ "mul R0,R0,R1" ]
        | DivsBA | DivuBA | RemsBA | RemuBA -> failwith "Assembler does not have division or remainder instructions"
        | AndAB                 -> [ "and R0,R0,R1" ]
        | OrAB                  -> [ "orr R0,R0,R1" ]
        | XorAB                 -> [ "eor R0,R0,R1" ]
        | ShlBC                 -> [ "lsl R1,R1,R2" ] // TODO: ARM is more flexible than X86, result could go into R0
        | ShrsBC                -> [ "asr R1,R1,R2" ] // TODO: ARM is more flexible than X86, result could go into R0
        | ShruBC                -> [ "lsr R1,R1,R2" ] // TODO: ARM is more flexible than X86, result could go into R0
        | RotlBC | RotrBC       -> failwith "Assembler does not have a rotate instruction"
        | CmpEqBA               -> [ "cmp R1,R0" ; "mov R0,#0" ; "moveq R0,#1" ]
        | CmpNeBA               -> [ "cmp R1,R0" ; "mov R0,#0" ; "movne R0,#1" ]
        | CmpLtsBA              -> [ "cmp R1,R0" ; "mov R0,#0" ; "movlt R0,#1" ]
        | CmpLtuBA              -> [ "cmp R1,R0" ; "mov R0,#0" ; "movlo R0,#1" ]
        | CmpGtsBA              -> [ "cmp R1,R0" ; "mov R0,#0" ; "movgt R0,#1" ]
        | CmpGtuBA              -> [ "cmp R1,R0" ; "mov R0,#0" ; "movhi R0,#1" ]
        | CmpLesBA              -> [ "cmp R1,R0" ; "mov R0,#0" ; "movle R0,#1" ]
        | CmpLeuBA              -> [ "cmp R1,R0" ; "mov R0,#0" ; "movls R0,#1" ]
        | CmpGesBA              -> [ "cmp R1,R0" ; "mov R0,#0" ; "movge R0,#1" ]
        | CmpGeuBA              -> [ "cmp R1,R0" ; "mov R0,#0" ; "movhs R0,#1" ]
        | CmpAZ                 -> [ "cmp R0,0"  ; "mov R0,#0" ; "moveq R0,#1" ]
        | FetchLoc(r,i)         -> [ sprintf "ldr %s,[R11, #%s]" (regNameOf r) (frameOffsetForLoc i) ]  // TODO: We only support WASM 32-bit integer type for now.
        | StoreLoc(r,i)         -> [ sprintf "str %s,[R11, #%s]" (regNameOf r) (frameOffsetForLoc i) ]  // TODO: We only support WASM 32-bit integer type for now.
        | FetchGlo(r,i)         -> [ sprintf "ldr %s,[%s]" (regNameOf r) (GlobalIdxNameString i) ]  // TODO: Eventually use the type rather than "int"
        | StoreGlo(r,i)         -> [ sprintf "str %s,[%s]" (regNameOf r) (GlobalIdxNameString i) ]  // TODO: Eventually use the type rather than "int"
        | StoreConst8 (r,U32 ofs,I32 v) -> storeConstant ArmByte     "strb"  r ofs (uint32 v)
        | StoreConst16(r,U32 ofs,I32 v) -> storeConstant ArmHalfword "strh"  r ofs (uint32 v)
        | StoreConst32(r,U32 ofs,I32 v) -> storeConstant ArmWord     "str"   r ofs (uint32 v)
        | Store8A (r,U32 ofs)   -> loadStoreRegOffset ArmByte     "strb"  r ofs
        | Store16A(r,U32 ofs)   -> loadStoreRegOffset ArmHalfword "strh"  r ofs
        | Store32A(r,U32 ofs)   -> loadStoreRegOffset ArmWord     "str"   r ofs
        | Fetch8s (r,U32 ofs)   -> loadStoreRegOffset ArmByte     "ldrsb" r ofs
        | Fetch8u (r,U32 ofs)   -> loadStoreRegOffset ArmByte     "ldrb"  r ofs
        | Fetch16s(r,U32 ofs)   -> loadStoreRegOffset ArmHalfword "ldrsh" r ofs
        | Fetch16u(r,U32 ofs)   -> loadStoreRegOffset ArmHalfword "ldrh"  r ofs
        | Fetch32 (r,U32 ofs)   -> loadStoreRegOffset ArmWord     "ldr"   r ofs
        | ThunkIn -> 
            // The translated code requires the Y register to
            // point to the base of the linear memory region.
            // Note: There is only *one* linear memory supported in WASM 1.0  (mem #0)
            [ 
                sprintf "movw R9,(%s%d and 0xFFFF)"  AsmMemPrefix 0 
                sprintf "movt R9,(%s%d shr 16)"      AsmMemPrefix 0 
            ]

        | SecondaryCmpBranch (condInstruction, targetLabel) -> 
            translateSecondaryCmpBranch condInstruction targetLabel

        | X8632Specific _ -> failwith "Unexpected usage of X86/32 optimisation!"


let ValTypeTranslationOf = function
    | I32Type -> "int"
    | I64Type -> failwith "Cannot translate I64 type with this simple translator"
    | F32Type -> failwith "Cannot translate F32 type with this simple translator"
    | F64Type -> failwith "Cannot translate F64 type with this simple translator"



let WriteOutFunctionAndBranchTables writeOutCode writeOutTables funcIndex (m:Module) translationState config (f:InternalFunctionRecord) =   // TODO: module only needed to query function metadata in TranslateInstructions

    let wasmToCrmTranslationConfig = 
        { ClearParametersAfterCall = true } 

    let crmInstructions, updatedTranslationState = 
        TranslateInstructionsAndApplyOptimisations 
            f m.Funcs translationState wasmToCrmTranslationConfig config id

    let thisFunctionCallsOut = 
        crmInstructions |> CrmInstructionsListMakesCallsOut

    let functionUsesStackPointerAtAddress4 =
        crmInstructions |> CrmInstructionsUsesShadowStackAtAddress4  // TODO: Ideal to collect this up front but we don't have all the translated function bodies to hand when the file header is written out.

    let procedureCommand = 
        sprintf "%s%d:  ; %s" AsmInternalFuncNamePrefix funcIndex (FunctionSignatureAsComment f.FuncType)

    let labelAndPrologueCode f =
        ["" ; procedureCommand]
        @
        if thisFunctionCallsOut then ["push {R14}"] else []
        @
        if f |> HasParametersOrLocals then
            ["push {R11}" ; "mov R11,R13"]
            @
            if f |> HasLocals then
                MathsWithConstant "sub" "R13" ((f |> LocalsCount) * StackSlotSizeU) armTempRegister    // TODO: show this as a comment?   (f |> FunctionLocalsAsComment))
            else 
                []
        else
            []

    let epilogueCode f =
        if f |> HasParametersOrLocals then
            if f |> HasLocals then ["mov R13,R11"] else []
            @ ["pop {R11}"]
        else []
        @
        if thisFunctionCallsOut then ["pop {R14}"] else []

    let writeInstruction instructionText = 
        writeOutCode ("    " + instructionText)

    let branchTableStart tableLabel =
        [
            sprintf "align %d" StackSlotSizeU
            LabelCommand tableLabel
        ]

    let branchTableItem targetLabel =
        sprintf "    dw %s" targetLabel

    let returnCommandFor (funcType:FuncType) (funcLocals:ValType[]) =
        "bx lr"

    try
        labelAndPrologueCode f 
            |> List.iter writeOutCode
        crmInstructions 
            |> MapTranslatedCrmInstructions (TranslateInstructionToAsmSequence thisFunctionCallsOut) f
            |> List.iter writeInstruction
        epilogueCode f 
            |> List.iter writeOutCode
        returnCommandFor f.FuncType f.Locals
            |> writeOutCode
        crmInstructions 
            |> MapBranchTablesList branchTableStart branchTableItem
            |> List.iter writeOutTables
    with
        | _ as ex -> failwith (sprintf "Error in %s:  %s" procedureCommand (ex.ToString()))

    (updatedTranslationState , functionUsesStackPointerAtAddress4)



let branchToEntryLabel mems (LabelName labelName) =
    [
        LabelCommand AsmEntryPointLabel
        "push {r0-r12,r14}" // TODO: In lieu of finding out about the caller's convention.
    ]
    @
    if mems |> HasAnyInitDataBlocks then [sprintf "bl %s" AsmInitMemoriesFuncName] else []
    @
    WriteThunkIn TheInitialisationFunctionMetadata (TranslateInstructionToAsmSequence false)
    @
    [
        sprintf "bl %s" labelName
        "pop {r0-r12,r14}"
        "bx lr"
    ]



let FilePrologue =
    [
        // TODO: temporary scaffold:
        "format binary"
        "org 0x40000000"
        "db 'F','#','F','X'    ; Indicates Jonathan's F# Web Assembly project executable file  (Fixed address executable)"
        "db 'A','R','v','7'    ; Indicates this is for ARMv7 32-bit"
        "dd 1                  ; File Version"
        "dd 0x40000000         ; Origin address for this fixed executable."
        "dd TotalSize          ; Total size needed for this fixed flat image"
        "dd wasm_entry         ; Entry point address"  // TODO: If using WasmStartEntryPointIfPresent this will fail to resolve since the entry is optional.
        sprintf "dd %s%d          ; Address of WASM linear memory" AsmMemPrefix 0
    ]

let FileEpilogue moduleUsesStackPointerAtAddress4 =
    [
        sprintf "dw %s                  ; 1=uses stack pointer at WASM linear memory address 4"  (if moduleUsesStackPointerAtAddress4 then "1" else "0")
    ]



let ArmDataInitialisation mems =

    let armCopyBlockCode i j ofsValue (byteArrayLength:int) =
        [
            sprintf "    ldr R0,%s%d_%d" AsmMemoryNamePrefix i j
            sprintf "    ldr R1,(%s%d+%d)" AsmMemPrefix i ofsValue
        ]
        @ LoadConstantInto "R2" (uint32 byteArrayLength)
        @
        [
            sprintf "    bl %s" AsmArmBlockCopyLabel
        ]

    if mems |> HasAnyInitDataBlocks then
        [
            CodeAlign
            LabelCommand AsmArmBlockCopyLabel
            "    ldrb R3,[R0], #1"
            "    strb R3,[R1], #1"
            "    subs R2, R2, #1"
            "    bne " + AsmArmBlockCopyLabel
            "    bx lr"
            CodeAlign
            LabelCommand AsmInitMemoriesFuncName
        ]
        @ (mems |> DataInitialisationFunctionUsing armCopyBlockCode)
        @ ["bx lr"]
    else
        []




let WriteOutBetterWasmAsArm32AssemblerText config headingText writeFileHeader writeOutData writeOutCode writeOutVar (m:Module) =   // TODO: rename because write out to text???

    // Start outputting ASM language text:

    let toComment commentText = 
        ("; " + commentText)

    let wasmTableHeading tableIndex =
        [
            "align 4"
            sprintf "data %s%d" AsmTableNamePrefix tableIndex
        ]

    let wasmTableRow nameString =
        sprintf "    dw %s" nameString

    let wasmMemVar memIndex linearMemorySize =
        [
            sprintf "align %d" StackSlotSizeU
            sprintf "%s%d: rb %d ; WASM linear memory reservation" AsmMemPrefix memIndex linearMemorySize
        ]
    
    let wasmMemDataHeading memIndex =
        [ sprintf "; Data for WASM mem %s%d" AsmMemoryNamePrefix memIndex ]

    let wasmMemRow memIndex dataBlockIndex byteArray =
        [ sprintf "%s%d_%d:" AsmMemoryNamePrefix memIndex dataBlockIndex ]
        @ (HexDumpList "    db" "," "0x" byteArray)

    let wasmGlobal globalIdxNameString initValue =
        [
            LabelCommand globalIdxNameString
            sprintf "dw %d" initValue
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
        |> ArmDataInitialisation 
        |> List.iter writeOutCode

    let mutable moduleTranslationState = ModuleTranslationState(0)  // TODO: hide ideally
    let mutable moduleUsesStackPointerAtAddress4 = false

    m.Funcs |> Array.iteri (fun i g ->
        match g with 

            | InternalFunction2(g) -> 
                let newState, usesStackPointerAtAddress4 =
                    g |> WriteOutFunctionAndBranchTables writeOutCode writeOutData i m moduleTranslationState config
                moduleTranslationState <- newState
                moduleUsesStackPointerAtAddress4 <- moduleUsesStackPointerAtAddress4 || usesStackPointerAtAddress4

            | ImportedFunction2({Import={ImportModuleName=m; ImportName=n}}) ->
                (sprintf "WASM Import: %s.%s" m n) |> toComment |> writeOutCode 
        )

    let (TranslationConfiguration (_,_,entryPointConfig)) = config
    
    WasmStartCode (branchToEntryLabel m.Mems) toComment m.Start m.Funcs entryPointConfig
        |> List.iter writeOutCode

    FileEpilogue moduleUsesStackPointerAtAddress4
        |> List.iter writeFileHeader




let TranslateBetterWasmToArm32AssemblerStdOut config headingText (m:Module) =

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

    WriteOutBetterWasmAsArm32AssemblerText config headingText writeFileHeader writeOutData writeOutCode writeOutVar m

    printf "%s" (headerStringBuilder.ToString())
    printf "%s" (dataStringBuilder.ToString())
    printf "%s" (codeStringBuilder.ToString())
    printf "%s" (varStringBuilder.ToString())


﻿module BetterWasmToArm32Asm

open System.Text
open WasmFileTypes
open WasmBetterTypes
open CommonRegisterMachineTypes
open AsmPrefixes
open WasmInstructionsToCRMInstructions
open Library
open TextFormatting
open ArmSupportLibrary



let TranslateInstructionToAsmSequence instruction =

    // TODO:  These translations can assume an ArmV7 target for now.

    let regNameOf = function
        | A -> "R0"
        | B -> "R1"
        | C -> "R2"
        | Y -> "R9"  // Conventionally the "Static base" register

    let armTempRegister = "R8"
    let offsetTempRegister = "R10"

    let loadConstant r value =
        LoadConstantInto (regNameOf r) value

    let loadStoreRegOffset fetchStoreType loadStoreInstruction addressReg offsetDesired = 
        let offsetStrategy = 
            ArmOffsetHandlingStrategyFor fetchStoreType offsetDesired offsetTempRegister 
        (OffsetLoadInstructionFor offsetStrategy)
            @ [ sprintf "%s R0,[%s%s]" loadStoreInstruction (regNameOf addressReg) (ArmOffset offsetStrategy) ]

    let storeConstant fetchStoreType armStoreInstruction addressReg offsetDesired value = 
        let offsetStrategy = 
            ArmOffsetHandlingStrategyFor fetchStoreType offsetDesired offsetTempRegister 
        (LoadConstantInto armTempRegister value) @
        (OffsetLoadInstructionFor offsetStrategy) @
        [ sprintf "%s %s,[%s%s]" armStoreInstruction armTempRegister (regNameOf addressReg) (ArmOffset offsetStrategy) ] 

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

    match instruction with
        | Barrier               -> [ "; ~~~ register barrier ~~~" ]
        | Breakpoint            -> [ "bkpt #0" ]
        | Drop                  -> [ "add R13,R13,#4" ]
        | Label(LabelName l)    -> [ "." + l ]
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
        | FetchLoc(r,i)         -> [ sprintf "ldr %s,[@%s]" (regNameOf r) (LocalIdxNameString i) ]  // TODO: Assumes 32-bit target
        | StoreLoc(r,i)         -> [ sprintf "str %s,[@%s]" (regNameOf r) (LocalIdxNameString i) ]  // TODO: Assumes 32-bit target
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
            [ sprintf "mov R9,%s%d" AsmMemPrefix 0 ]



let ValTypeTranslationOf = function
    | I32Type -> "int"
    | I64Type -> failwith "Cannot translate I64 type with this simple translator"
    | F32Type -> failwith "Cannot translate F32 type with this simple translator"
    | F64Type -> failwith "Cannot translate F64 type with this simple translator"



let WriteOutFunctionLocals writeOut (funcType:FuncType) funcLocals =

    let indexOfFirstLocal = funcType.ParameterTypes.Length

    funcLocals |> Array.iteri (fun arrayIndex v ->
        let indexOfVariable = indexOfFirstLocal + arrayIndex
        let prefixStr = 
            match arrayIndex with 
                | 0 -> "var "
                | _ -> "  , "
        writeOut (sprintf "%s@%s%d:%s" prefixStr AsmLocalNamePrefix indexOfVariable (ValTypeTranslationOf v)))



let WriteOutFunctionAndBranchTables writeOutCode writeOutTables funcIndex (m:Module) translationState config (f:InternalFunctionRecord) =   // TODO: module only needed to query function metadata in TranslateInstructions

    let crmInstructions, updatedTranslationState = 
        f.Body |> TranslateInstructions m.Funcs translationState

    let paramListOf (ps:ValType[]) =
        String.concat ", " (ps |> Array.map ValTypeTranslationOf)

    let textSignatureOf (funcType:FuncType) =
        let translatedParameters = paramListOf funcType.ParameterTypes
        let translatedReturns    = paramListOf funcType.ReturnTypes
        (Bracketed translatedParameters) + (ColonPrefixed translatedReturns)

    let asmSignatureOf (funcType:FuncType) =

        let atParamDecls (ps:ValType[]) =
            String.concat ", " (ps |> Array.mapi (fun i t -> (sprintf "@%s%d" AsmLocalNamePrefix i)))

        let atParamsString = 
            atParamDecls funcType.ParameterTypes

        (Bracketed atParamsString) + "  ; " + (textSignatureOf funcType)

    let procedureCommand = 
        sprintf ".%s%d%s" AsmInternalFuncNamePrefix funcIndex (asmSignatureOf f.FuncType)

    let writeInstruction instructionText = 
        writeOutCode ("    " + instructionText)

    let branchTableStart tableLabel =
        writeOutTables "align ptr"
        writeOutTables (sprintf "data %s" tableLabel)

    let branchTableItem targetLabel =
        writeOutTables (sprintf "    ptr %s" targetLabel)

    let returnCommandFor (funcType:FuncType) (funcLocals:ValType[]) =
        match (funcType.ParameterTypes.Length, funcLocals.Length) with
            | (0,0) -> "ret"
            | (_,_) -> "endproc"

    try
        writeOutCode procedureCommand
        WriteOutFunctionLocals writeOutCode f.FuncType f.Locals
        crmInstructions |> ForTranslatedCrmInstructionsDo writeInstruction TranslateInstructionToAsmSequence f.FuncType config
        writeOutCode (returnCommandFor f.FuncType f.Locals)
        crmInstructions |> ForAllBranchTablesDo branchTableStart branchTableItem
    with
        | _ as ex -> failwith (sprintf "Error in %s:  %s" procedureCommand (ex.ToString()))

    updatedTranslationState



let WriteOutBranchToEntryLabel writeOut startFuncIdx moduleFuncsArray =

    writeOut ("." + AsmEntryPointLabel)
    let (LabelName labelName) = FuncLabelFor startFuncIdx moduleFuncsArray
    writeOut (sprintf "jmp %s" labelName)



let WriteOutWasm2AsArm32AssemblerText config headingText writeOutData writeOutCode writeOutVar (m:Module) =   // TODO: rename because write out to text???

    // Start outputting ASM language text:

    let toComment commentText = 
        ("; " + commentText)

    let wasmTableHeading tableIndex =
        writeOutData "align ptr"
        writeOutData (sprintf "data %s%d" AsmTableNamePrefix tableIndex)

    let wasmTableRow nameString =
        writeOutData (sprintf "    ptr %s" nameString)

    let wasmMemHeading memIndex linearMemorySize =
        writeOutVar "global"
        writeOutVar "    align ptr"
        writeOutVar (sprintf "    %s%d: %d" AsmMemPrefix memIndex linearMemorySize)
        writeOutData (sprintf "; Data for WASM mem %s%d" AsmMemoryNamePrefix memIndex) // TODO: If there is none, omit this.

    let wasmMemRow memIndex dataBlockIndex byteArray =
        let writeIns s = writeOutData ("    " + s)
        writeOutData (sprintf "data %s%d_%d" AsmMemoryNamePrefix memIndex dataBlockIndex)
        ForEachLineOfHexDumpDo "db" "," "0x" writeIns byteArray

    let writeOutCopyBlockCode i j ofsValue byteArrayLength =
        writeOutCode (sprintf "    mov R9,(%s%d+%d)" AsmMemPrefix i ofsValue)
        writeOutCode (sprintf "    mov ESI,%s%d_%d" AsmMemoryNamePrefix i j)
        writeOutCode (sprintf "    mov ECX,%d" byteArrayLength)
        writeOutCode          "    cld"
        writeOutCode          "    rep movsb"

    let writeOutIns s = 
        writeOutCode ("    " + s)

    let writeOutWasmGlobal globalIdxNameString initValue =
        writeOutData (sprintf ".%s" globalIdxNameString)
        writeOutData (sprintf "dd %d" initValue)

    ("Translation of WASM module: " + headingText) |> toComment |> writeOutData
    writeOutData ""

    m.Tables  |> ForAllWasmTablesDo  (ForWasmTableDo wasmTableHeading wasmTableRow)
    m.Globals |> ForAllWasmGlobalsDo writeOutWasmGlobal
    m.Mems    |> ForAllWasmMemsDo    (WithWasmMemDo wasmMemHeading wasmMemRow)

    writeOutCode (sprintf ".init_%s" AsmMemPrefix)
    m.Mems |> ForTheDataInitialisationFunctionDo writeOutCopyBlockCode writeOutIns TranslateInstructionToAsmSequence
    writeOutCode "ret"

    let mutable moduleTranslationState = ModuleTranslationState(0)  // TODO: hide ideally

    m.Funcs |> Array.iteri (fun i g ->
        match g with 

            | InternalFunction2(g) -> 
                moduleTranslationState <- 
                    g |> WriteOutFunctionAndBranchTables writeOutCode writeOutData i m moduleTranslationState config

            | ImportedFunction2({Import={ImportModuleName=m; ImportName=n}}) ->
                (sprintf "WASM Import: %s.%s" m n) |> toComment |> writeOutCode 
        )

    WithWasmStartDo WriteOutBranchToEntryLabel writeOutCode toComment m.Start m.Funcs
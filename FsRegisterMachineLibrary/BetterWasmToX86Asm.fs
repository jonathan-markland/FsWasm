﻿module BetterWasmToX86Asm

open System.Text
open WasmFileTypes
open WasmBetterTypes
open CommonRegisterMachineTypes
open AsmPrefixes
open WasmInstructionsToCRMInstructions
open Library
open TextFormatting



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

    let translateREGU32 s1 r u s2 = 
        [ sprintf "%s%s%s%s" s1 (regNameOf r) (offsetIfNeeded u) s2 ]

    let translateREGU32I32 s1 r u s2 n = 
        [ sprintf "%s%s%s%s%d" s1 (regNameOf r) (offsetIfNeeded u) s2 n ]

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
            "+" + ((paramCount - locNumber) * 4u + 4u).ToString()
        else
            "-" + ((locNumber - paramCount) * 4u + 4u).ToString()


    match instruction with
        | Barrier               -> [ "; ~~~ register barrier ~~~" ]
        | Breakpoint            -> [ "int 3" ]
        | Drop                  -> [ "add ESP,4" ]
        | Label(LabelName l)    -> [ "." + l ]
        | Const(r,Const32(n))   -> [ sprintf "mov %s,%d" (regNameOf r) n ]
        | Goto(LabelName l)     -> [ "jmp " + l ]
        | CallFunc(LabelName l) -> [ "call " + l ]
        | CallTableIndirect     -> translateCallTableIndirect ()
        | BranchAZ(LabelName l) -> [ "cmp EAX,0" ; "jz " + l ]
        | BranchANZ(LabelName l)-> [ "cmp EAX,0" ; "jnz " + l ]
        | GotoIndex(t,n,d,_)    -> translateGotoIndex t n d   // The ignored parameter is the lookup table, which we separately output.
        | Push(r)               -> [ sprintf "push %s" (regNameOf r) ]
        | Pop(r)                -> [ sprintf "pop %s" (regNameOf r) ]
        | PeekA                 -> [ "mov EAX,dword ptr [ESP]" ]
        | Let(r1,r2)            -> [ sprintf "mov %s,%s" (regNameOf r1) (regNameOf r2) ]
        | AddAN(I32(n))         -> [ sprintf "add EAX,%d" n ]
        | SubAN(I32(n))         -> [ sprintf "sub EAX,%d" n ]
        | AndAN(I32(n))         -> [ sprintf "and EAX,%d" n ]
        | OrAN(I32(n))          -> [ sprintf "or EAX,%d"  n ]
        | XorAN(I32(n))         -> [ sprintf "xor EAX,%d" n ]
        | Add(r1,r2)            -> [ sprintf "add %s,%s" (regNameOf r1) (regNameOf r2) ]  // commutative
        | SubBA                 -> [ "sub EBX,EAX" ]
        | MulAB                 -> [ "mul EAX,EBX" ]  // commutative
        | DivsBA | DivuBA | RemsBA | RemuBA -> failwith "Assembler does not have division or remainder instructions"
        | AndAB                 -> [ "and EAX,EBX" ]  // commutative
        | OrAB                  -> [ "or EAX,EBX" ]   // commutative
        | XorAB                 -> [ "xor EAX,EBX" ]  // commutative
        | ShlBC                 -> [ "shl EBX,CL" ]
        | ShrsBC                -> [ "sar EBX,CL" ]
        | ShruBC                -> [ "shr EBX,CL" ]
        | RotlBC | RotrBC       -> failwith "Assembler does not have a rotate instruction"
        | CmpEqBA               -> [ "cmp EBX,EAX" ; "setz AL"  ; "movzx EAX,AL" ]
        | CmpNeBA               -> [ "cmp EBX,EAX" ; "setnz AL" ; "movzx EAX,AL" ]
        | CmpLtsBA              -> [ "cmp EBX,EAX" ; "setl AL"  ; "movzx EAX,AL" ]
        | CmpLtuBA              -> [ "cmp EBX,EAX" ; "setb AL"  ; "movzx EAX,AL" ]
        | CmpGtsBA              -> [ "cmp EBX,EAX" ; "setg AL"  ; "movzx EAX,AL" ]
        | CmpGtuBA              -> [ "cmp EBX,EAX" ; "seta AL"  ; "movzx EAX,AL" ]
        | CmpLesBA              -> [ "cmp EBX,EAX" ; "setle AL" ; "movzx EAX,AL" ]
        | CmpLeuBA              -> [ "cmp EBX,EAX" ; "setbe AL" ; "movzx EAX,AL" ]
        | CmpGesBA              -> [ "cmp EBX,EAX" ; "setge AL" ; "movzx EAX,AL" ]
        | CmpGeuBA              -> [ "cmp EBX,EAX" ; "setae AL" ; "movzx EAX,AL" ]
        | CmpAZ                 -> [ "cmp EAX,0"   ; "setz AL"  ; "movzx EAX,AL" ]
        | FetchLoc(r,i)         -> [ sprintf "mov %s,[EBP%s]  ; @%s" (regNameOf r) (frameOffsetForLoc i) (LocalIdxNameString i) ]  // TODO: Assumes 32-bit target
        | StoreLoc(r,i)         -> [ sprintf "mov [EBP%s],%s  ; @%s" (frameOffsetForLoc i) (regNameOf r) (LocalIdxNameString i) ]  // TODO: Assumes 32-bit target
        | FetchGlo(r,i)         -> [ sprintf "mov %s,[%s]" (regNameOf r) (GlobalIdxNameString i) ]  // TODO: Eventually use the type rather than "int"
        | StoreGlo(r,i)         -> [ sprintf "mov [%s],%s" (GlobalIdxNameString i) (regNameOf r) ]  // TODO: Eventually use the type rather than "int"
        | StoreConst8(r,ofs,I32(v))   -> translateREGU32I32 "mov byte ptr [" r ofs "]," v
        | StoreConst16(r,ofs,I32(v))  -> translateREGU32I32 "mov word ptr [" r ofs "]," v
        | StoreConst32(r,ofs,I32(v))  -> translateREGU32I32 "mov dword ptr [" r ofs "]," v  
        | Store8A(r,ofs)       -> translateREGU32 "mov byte ptr [" r ofs "],EAX"  
        | Store16A(r,ofs)      -> translateREGU32 "mov word ptr [" r ofs "],EAX"
        | Store32A(r,ofs)      -> translateREGU32 "mov dword ptr [" r ofs "],EAX"  
        | Fetch8s(r,ofs)       -> translateREGU32 "movsx EAX, byte ptr [" r ofs "]" 
        | Fetch8u(r,ofs)       -> translateREGU32 "movzx EAX, byte ptr [" r ofs "]"  
        | Fetch16s(r,ofs)      -> translateREGU32 "movsx EAX, word ptr [" r ofs "]" 
        | Fetch16u(r,ofs)      -> translateREGU32 "movzx EAX, word ptr [" r ofs "]"
        | Fetch32(r,ofs)       -> translateREGU32 "mov EAX,[" r ofs "]"  
        | ThunkIn              -> 
            // The translated code requires the Y register to
            // point to the base of the linear memory region.
            // Note: There is only *one* linear memory supported in WASM 1.0  (mem #0)
            [ sprintf "mov EDI,%s%d" AsmMemPrefix 0 ]



let WriteOutFunctionAndBranchTables writeOutCode writeOutTables funcIndex (m:Module) translationState config (f:InternalFunctionRecord) =   // TODO: module only needed to query function metadata in TranslateInstructions

    let crmInstructions, updatedTranslationState = 
        f.Body |> TranslateInstructions m.Funcs translationState

    let procedureCommand =
        sprintf ".%s%d  ; %s" AsmInternalFuncNamePrefix funcIndex (FunctionSignatureAsComment f.FuncType)

    let writeLabelAndPrologueCode f =
        writeOutCode procedureCommand
        if f |> HasParamsReturnsOrLocals then
            writeOutCode "push EBP"
            writeOutCode "mov EBP,ESP"
            if f |> HasLocals then
                writeOutCode (sprintf "sub ESP,%d  ; %s" ((f |> LocalsCount) * 4u) (f |> FunctionLocalsAsComment))

    let writeEpilogueCode f =
        if f |> HasParamsReturnsOrLocals then
            if f |> HasLocals then writeOutCode "mov ESP,EBP"
            writeOutCode "pop EBP"

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
        writeLabelAndPrologueCode f
        crmInstructions |> ForTranslatedCrmInstructionsDo writeInstruction TranslateInstructionToAsmSequence f config
        writeEpilogueCode f
        writeOutCode (returnCommandFor f.FuncType f.Locals)
        crmInstructions |> ForAllBranchTablesDo branchTableStart branchTableItem
    with
        | _ as ex -> failwith (sprintf "Error in %s:  %s" procedureCommand (ex.ToString()))

    updatedTranslationState



let WriteOutBranchToEntryLabel writeOut startFuncIdx moduleFuncsArray =

    writeOut ("." + AsmEntryPointLabel)
    let (LabelName labelName) = FuncLabelFor startFuncIdx moduleFuncsArray
    writeOut (sprintf "jmp %s" labelName)



let WriteOutWasm2AsX86AssemblerText config headingText writeOutData writeOutCode writeOutVar (m:Module) =   // TODO: rename because write out to text???

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
        writeOutCode (sprintf "    mov EDI,(%s%d+%d)" AsmMemPrefix i ofsValue)
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
    m.Mems |> ForTheDataInitialisationFunctionDo writeOutCopyBlockCode writeOutIns TheInitialisationFunctionMetadata TranslateInstructionToAsmSequence
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

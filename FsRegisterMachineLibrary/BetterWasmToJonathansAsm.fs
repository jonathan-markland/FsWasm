module BetterWasmToJonathansAsm


open System.Text
open WasmFileTypes
open WasmBetterTypes
open CommonRegisterMachineTypes
open AsmPrefixes
open WasmStaticExpressionEvaluator
open BWToCRMConfigurationTypes
open OptimiseCommonRegisterMachine
open WasmInstructionsToCRMInstructions
open Library



let TranslateInstructionToAsmSequence instruction =

    // TODO:  These translations can assume a 32-bit target for now.

    let regNameOf = function
        | A -> "A"
        | B -> "B"
        | C -> "C"
        | Y -> "Y"

    let offsetIfNeeded u = 
        match u with
            | U32 0u -> ""                   // indexed addressing not needed with zero offset
            | U32 n  -> "+" + n.ToString()   // indexed addressing needed

    let translateREGU32 s1 r u s2 = 
        [ sprintf "%s%s%s%s" s1 (regNameOf r) (offsetIfNeeded u) s2 ]

    let translateREGU32I32 s1 r u s2 n = 
        [ sprintf "%s%s%s%s%d" s1 (regNameOf r) (offsetIfNeeded u) s2 n ]

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


    match instruction with
        | Barrier               -> [ "// ~~~ register barrier ~~~" ]
        | Breakpoint            -> [ "break" ]
        | Drop                  -> [ "add SP,4" ]  // TODO: Assumes 32-bit target
        | Label(LabelName l)    -> [ "label " + l ]   // TODO: sort out using the local label references in Jonathan's ASM
        | Const(r,Const32(n))   -> [ sprintf "let %s=%d" (regNameOf r) n ]
        | Goto(LabelName l)     -> [ "goto " + l ]
        | CallFunc(LabelName l) -> [ "call " + l ]
        | CallTableIndirect     -> translateCallTableIndirect ()
        | BranchAZ(LabelName l) -> [ "cmp A,0:if z goto " + l ]
        | BranchANZ(LabelName l)-> [ "cmp A,0:if nz goto " + l ]
        | GotoIndex(t,n,d,_)    -> translateGotoIndex t n d   // The ignored parameter is the lookup table, which we separately output.
        | Push(r)               -> [ sprintf "push %s" (regNameOf r) ]
        | Pop(r)                -> [ sprintf "pop %s" (regNameOf r) ]
        | PeekA                 -> [ "let A=int [SP]" ]  // TODO: Assumes 32-bit target
        | Let(r1,r2)            -> [ sprintf "let %s=%s" (regNameOf r1) (regNameOf r2) ]
        | AddAN(I32(n))         -> [ sprintf "add A,%d" n ]
        | SubAN(I32(n))         -> [ sprintf "sub A,%d" n ]
        | AndAN(I32(n))         -> [ sprintf "and A,%d" n ]
        | OrAN(I32(n))          -> [ sprintf "or A,%d"  n ]
        | XorAN(I32(n))         -> [ sprintf "xor A,%d" n ]
        | Add(r1,r2)            -> [ sprintf "add %s,%s" (regNameOf r1) (regNameOf r2) ]  // commutative
        | SubBA                 -> [ "sub B,A" ]
        | MulAB                 -> [ "mul A,B" ]  // commutative
        | DivsBA | DivuBA | RemsBA | RemuBA -> failwith "Assembler does not have division or remainder instructions"
        | AndAB                 -> [ "and A,B" ]  // commutative
        | OrAB                  -> [ "or A,B" ]   // commutative
        | XorAB                 -> [ "xor A,B" ]  // commutative
        | ShlBC                 -> [ "shl B,C" ]
        | ShrsBC                -> [ "sar B,C" ]
        | ShruBC                -> [ "shr B,C" ]
        | RotlBC | RotrBC       -> failwith "Assembler does not have a rotate instruction"
        | CmpEqBA               -> [ "cmp B,A:set z A" ]
        | CmpNeBA               -> [ "cmp B,A:set nz A" ]
        | CmpLtsBA              -> [ "cmp B,A:set < A" ]
        | CmpLtuBA              -> [ "cmp B,A:set << A" ]
        | CmpGtsBA              -> [ "cmp B,A:set > A" ]
        | CmpGtuBA              -> [ "cmp B,A:set >> A" ]
        | CmpLesBA              -> [ "cmp B,A:set <= A" ]
        | CmpLeuBA              -> [ "cmp B,A:set <<= A" ]
        | CmpGesBA              -> [ "cmp B,A:set >= A" ]
        | CmpGeuBA              -> [ "cmp B,A:set >>= A" ]
        | CmpAZ                 -> [ "cmp A,0:set z A" ]
        | FetchLoc(r,i)         -> [ sprintf "let %s=int[@%s]" (regNameOf r) (LocalIdxNameString i) ]  // TODO: Assumes 32-bit target
        | StoreLoc(r,i)         -> [ sprintf "let int[@%s]=%s" (LocalIdxNameString i) (regNameOf r) ]  // TODO: Assumes 32-bit target
        | FetchGlo(r,i)         -> [ sprintf "let %s=int[%s]" (regNameOf r) (GlobalIdxNameString i) ]  // TODO: Eventually use the type rather than "int"
        | StoreGlo(r,i)         -> [ sprintf "let int[%s]=%s" (GlobalIdxNameString i) (regNameOf r) ]  // TODO: Eventually use the type rather than "int"
        | StoreConst8(r,ofs,I32(v))   -> translateREGU32I32 "let byte[" r ofs "]=" v
        | StoreConst16(r,ofs,I32(v))  -> translateREGU32I32 "let ushort[" r ofs "]=" v
        | StoreConst32(r,ofs,I32(v))  -> translateREGU32I32 "let uint[" r ofs "]=" v  
        | Store8A(r,ofs)       -> translateREGU32 "let byte[" r ofs "]=A"  
        | Store16A(r,ofs)      -> translateREGU32 "let ushort[" r ofs "]=A"
        | Store32A(r,ofs)      -> translateREGU32 "let uint[" r ofs "]=A"  
        | Fetch8s(r,ofs)       -> translateREGU32 "let A=sbyte[" r ofs "]" 
        | Fetch8u(r,ofs)       -> translateREGU32 "let A=byte[" r ofs "]"  
        | Fetch16s(r,ofs)      -> translateREGU32 "let A=short[" r ofs "]" 
        | Fetch16u(r,ofs)      -> translateREGU32 "let A=ushort[" r ofs "]"
        | Fetch32(r,ofs)       -> translateREGU32 "let A=uint[" r ofs "]"  
        | ThunkIn              -> 
            // The translated code requires the Y register to
            // point to the base of the linear memory region.
            // Note: There is only *one* linear memory supported in WASM 1.0  (mem #0)
            [ sprintf "let Y=%s%d" AsmMemPrefix 0 ]
















let Surrounded (before:string) (after:string) (content:string) =
    match content.Length with
        | 0 -> ""
        | _ -> before + content + after


let Bracketed s =           s |> Surrounded "(" ")"
let Prefixed thePrefix s =  s |> Surrounded thePrefix ""
let ColonPrefixed s =       s |> Prefixed ": "



let ValTypeTranslationOf =
    function
        | I32Type -> "int"
        | I64Type -> failwith "Cannot translate I64 type with this simple translator"
        | F32Type -> failwith "Cannot translate F32 type with this simple translator"
        | F64Type -> failwith "Cannot translate F64 type with this simple translator"


let ParamListOf (ps:ValType[]) =
    String.concat ", " (ps |> Array.map ValTypeTranslationOf)


let TextSignatureOf (funcType:FuncType) =
    let translatedParameters = ParamListOf funcType.ParameterTypes
    let translatedReturns    = ParamListOf funcType.ReturnTypes
    (Bracketed translatedParameters) + (ColonPrefixed translatedReturns)


let AsmSignatureOf (funcType:FuncType) =

    let atParamDecls (ps:ValType[]) =
        String.concat ", " (ps |> Array.mapi (fun i t -> (sprintf "@%s%d" AsmLocalNamePrefix i)))

    let atParamsString = 
        atParamDecls funcType.ParameterTypes

    (Bracketed atParamsString) + "  // " + (TextSignatureOf funcType)




let WriteOutFunctionLocals writeOut (funcType:FuncType) funcLocals =

    let indexOfFirstLocal = funcType.ParameterTypes.Length

    funcLocals |> Array.iteri (fun arrayIndex v ->
        let indexOfVariable = indexOfFirstLocal + arrayIndex
        let prefixStr = 
            match arrayIndex with 
                | 0 -> "var "
                | _ -> "  , "
        writeOut (sprintf "%s@%s%d:%s" prefixStr AsmLocalNamePrefix indexOfVariable (ValTypeTranslationOf v)))















let WriteOutWasmGlobal writeOut (m:Module) i (g:InternalGlobalRecord) =

    // TODO: We do nothing with the immutability information.  Could we avoid a store and hoist the constant into the code?

    let initValue = StaticEvaluate g.InitExpr
    let globalIdx = GlobalIdx(U32(uint32 i))   // TODO: not ideal construction of temporary

    writeOut (sprintf "data %s int %d" (GlobalIdxNameString globalIdx) initValue)








let WriteOutFunctionAndBranchTables writeOutCode writeOutTables funcIndex (m:Module) translationState config (f:InternalFunctionRecord) =   // TODO: module only needed to query function metadata in TranslateInstructions

    let crmInstructions, updatedTranslationState = 
        f.Body |> TranslateInstructions m.Funcs translationState

    let procedureCommand = 
        sprintf "procedure %s%d%s" AsmInternalFuncNamePrefix funcIndex (AsmSignatureOf f.FuncType)

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

    writeOut ("procedure " + AsmEntryPointLabel)
    let (LabelName labelName) = FuncLabelFor startFuncIdx moduleFuncsArray
    writeOut (sprintf "goto %s" labelName)





let WriteOutWasm2AsJonathansAssemblerText config headingText writeOutData writeOutCode writeOutVar (m:Module) =   // TODO: rename because write out to text???

    // Start outputting ASM language text:

    let toComment commentText = 
        ("// " + commentText)

    let wasmTableHeading tableIndex =
        writeOutData "align ptr"
        writeOutData (sprintf "data %s%d" AsmTableNamePrefix tableIndex)

    let wasmTableRow nameString =
        writeOutData (sprintf "    ptr %s" nameString)

    let wasmMemHeading memIndex linearMemorySize =
        writeOutVar "global"
        writeOutVar "    align ptr"
        writeOutVar (sprintf "    %s%d: %d" AsmMemPrefix memIndex linearMemorySize)
        writeOutData (sprintf "// Data for WASM mem %s%d" AsmMemoryNamePrefix memIndex) // TODO: If there is none, omit this.

    let wasmMemRow memIndex dataBlockIndex byteArray =
        let writeIns s = writeOutData ("    " + s)
        writeOutData (sprintf "data %s%d_%d" AsmMemoryNamePrefix memIndex dataBlockIndex)
        ForEachLineOfHexDumpDo "byte" "," "0x" writeIns byteArray

    let writeOutCopyBlockCode i j ofsValue byteArrayLength =
        writeOutCode (sprintf "    let Y=%s%d+%d" AsmMemPrefix i ofsValue)
        writeOutCode (sprintf "    let X=%s%d_%d" AsmMemoryNamePrefix i j)
        writeOutCode (sprintf "    let C=%d" byteArrayLength)
        writeOutCode          "    cld rep movsb"

    let writeOutIns s = 
        writeOutCode ("    " + s)

    ("Translation of WASM module: " + headingText) |> toComment |> writeOutData
    writeOutData ""

    m.Tables  |> ForAllWasmTablesDo  (ForWasmTableDo wasmTableHeading wasmTableRow)
    m.Globals |> ForAllWasmGlobalsDo (WriteOutWasmGlobal writeOutData m)
    m.Mems    |> ForAllWasmMemsDo    (WithWasmMemDo wasmMemHeading wasmMemRow)

    writeOutCode (sprintf "procedure init_%s" AsmMemPrefix)
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



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



let TranslateGotoIndex (LabelName tableLabel) numMax (LabelName defaultLabel) =
    [
        // A is already the index to branch to
        sprintf "cmp A,%d:if >>= goto %s" numMax defaultLabel
        "shl A,logptr"
        sprintf "goto [A+%s]" tableLabel
    ]


let TranslateCallTableIndirect () =
    // TODO: We really need to emit some code to validate the signatures.
    // A is already the index to call.
    // TODO: We need to validate index A lies within wasm table [0]
    [
        "shl A,logptr"
        sprintf "goto [A+%s0]" AsmTableNamePrefix  // WASM 1.0 always looks in table #0
    ]



let OfsIfNeeded u = 
    match u with
        | U32 0u -> ""                   // indexed addressing not needed with zero offset
        | U32 n  -> "+" + n.ToString()   // indexed addressing needed


let RegNameOf = function
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | Y -> "Y"


let TranslateREGU32 s1 r u s2 = 
    [ sprintf "%s%s%s%s" s1 (RegNameOf r) (OfsIfNeeded u) s2 ]


let TranslateREGU32I32 s1 r u s2 n = 
    [ sprintf "%s%s%s%s%d" s1 (RegNameOf r) (OfsIfNeeded u) s2 n ]



let TranslateInstructionToAsmSequence instruction =

    // TODO:  These translations can assume a 32-bit target for now.

    match instruction with
        | Barrier               -> [ "// ~~~ register barrier ~~~" ]
        | Breakpoint            -> [ "break" ]
        | Drop                  -> [ "add SP,4" ]  // TODO: Assumes 32-bit target
        | Label(LabelName l)    -> [ "label " + l ]   // TODO: sort out using the local label references in Jonathan's ASM
        | Const(r,Const32(n))   -> [ sprintf "let %s=%d" (RegNameOf r) n ]
        | Goto(LabelName l)     -> [ "goto " + l ]
        | CallFunc(LabelName l) -> [ "call " + l ]
        | CallTableIndirect     -> TranslateCallTableIndirect ()
        | BranchAZ(LabelName l) -> [ "cmp A,0:if z goto " + l ]
        | BranchANZ(LabelName l)-> [ "cmp A,0:if nz goto " + l ]
        | GotoIndex(t,n,d,_)    -> TranslateGotoIndex t n d   // The ignored parameter is the lookup table, which we separately output.
        | Push(r)               -> [ sprintf "push %s" (RegNameOf r) ]
        | Pop(r)                -> [ sprintf "pop %s" (RegNameOf r) ]
        | PeekA                 -> [ "let A=int [SP]" ]  // TODO: Assumes 32-bit target
        | Let(r1,r2)            -> [ sprintf "let %s=%s" (RegNameOf r1) (RegNameOf r2) ]
        | AddAN(I32(n))         -> [ sprintf "add A,%d" n ]
        | SubAN(I32(n))         -> [ sprintf "sub A,%d" n ]
        | AndAN(I32(n))         -> [ sprintf "and A,%d" n ]
        | OrAN(I32(n))          -> [ sprintf "or A,%d"  n ]
        | XorAN(I32(n))         -> [ sprintf "xor A,%d" n ]
        | Add(r1,r2)            -> [ sprintf "add %s,%s" (RegNameOf r1) (RegNameOf r2) ]  // commutative
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
        | FetchLoc(r,i)         -> [ sprintf "let %s=int[@%s]" (RegNameOf r) (LocalIdxNameString i) ]  // TODO: Assumes 32-bit target
        | StoreLoc(r,i)         -> [ sprintf "let int[@%s]=%s" (LocalIdxNameString i) (RegNameOf r) ]  // TODO: Assumes 32-bit target
        | FetchGlo(r,i)         -> [ sprintf "let %s=int[%s]" (RegNameOf r) (GlobalIdxNameString i) ]  // TODO: Eventually use the type rather than "int"
        | StoreGlo(r,i)         -> [ sprintf "let int[%s]=%s" (GlobalIdxNameString i) (RegNameOf r) ]  // TODO: Eventually use the type rather than "int"
        | StoreConst8(r,ofs,I32(v))   -> TranslateREGU32I32 "let byte[" r ofs "]=" v
        | StoreConst16(r,ofs,I32(v))  -> TranslateREGU32I32 "let ushort[" r ofs "]=" v
        | StoreConst32(r,ofs,I32(v))  -> TranslateREGU32I32 "let uint[" r ofs "]=" v  
        | Store8A(r,ofs)       -> TranslateREGU32 "let byte[" r ofs "]=A"  
        | Store16A(r,ofs)      -> TranslateREGU32 "let ushort[" r ofs "]=A"
        | Store32A(r,ofs)      -> TranslateREGU32 "let uint[" r ofs "]=A"  
        | Fetch8s(r,ofs)       -> TranslateREGU32 "let A=sbyte[" r ofs "]" 
        | Fetch8u(r,ofs)       -> TranslateREGU32 "let A=byte[" r ofs "]"  
        | Fetch16s(r,ofs)      -> TranslateREGU32 "let A=short[" r ofs "]" 
        | Fetch16u(r,ofs)      -> TranslateREGU32 "let A=ushort[" r ofs "]"
        | Fetch32(r,ofs)       -> TranslateREGU32 "let A=uint[" r ofs "]"  
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



let ReturnsSingleValue (ft:FuncType) =
    match ft.ReturnTypes.Length with
        | 0 -> false
        | 1 -> true
        | _ -> failwith "Cannot translate function that returns more than one value"



let ReturnCommandFor (funcType:FuncType) (funcLocals:ValType[]) =
    match (funcType.ParameterTypes.Length, funcLocals.Length) with
        | (0,0) -> "ret"
        | (_,_) -> "endproc"






let WriteOutWasmTable writeOut i (m:Module) (t:InternalTableRecord) =

    match t.InitData.Length with
        | 0 -> ()
        | 1 ->

            let writeIns s = writeOut ("    " + s)  // TODO: repetition throughout routines!

            writeOut "align ptr"
            writeOut (sprintf "data %s%d" AsmTableNamePrefix i)

            t.InitData |> Array.iter (fun elem ->
                    let ofsExpr, funcIdxList = elem
                    let ofsValue = StaticEvaluate ofsExpr
                    if ofsValue <> 0 then failwith "Cannot translate module with TableSec table that has Elem with non-zero data initialisation offset"
                    funcIdxList |> Array.iter (fun funcIdx -> 
                        writeIns (sprintf "ptr %s" (FuncIdxNameString funcIdx)))
                )

        | _ -> failwith "Cannot translate module with more than one Elem in a TableSec table"



let WasmMemoryBlockMultiplier = 65536u







let WriteOutAllDataInitialisationFunction  writeOutCode (mems:Memory[]) =

    writeOutCode (sprintf "procedure init_%s" AsmMemPrefix)

    // NO!!  The following is absolutely wrong, as this translator
    //       should not pretend to know who its input generator is:
    // writeOutCode (sprintf "    // Caller must pass stack pointer (relative to %s%d) in A" AsmMemPrefix 0)
    // writeOutCode (sprintf "    let uint [%s%d+4]=A // Initialise WasmFiddle stack pointer at address offset 4" AsmMemPrefix 0)

    let writeOutDataCopyCommand i (thisMem:InternalMemoryRecord) =
        if i<>0 then failwith "Cannot translate WASM module with more than one Linear Memory"
        thisMem.InitData |> Array.iteri (fun j elem ->
                let ofsExpr, byteArray = elem
                let ofsValue = StaticEvaluate ofsExpr
                writeOutCode (sprintf "    let Y=%s%d+%d" AsmMemPrefix i ofsValue)
                writeOutCode (sprintf "    let X=%s%d_%d" AsmMemoryNamePrefix i j)
                writeOutCode (sprintf "    let C=%d" byteArray.Length)
                writeOutCode          "    cld rep movsb"
            )

    mems |> Array.iteri (fun i me ->
        match me with
            | InternalMemory2(mem) -> mem |> writeOutDataCopyCommand i 
            | ImportedMemory2(mem) -> failwith "Error:  Cannot support importing a 'memory' region.  WASM module must be expect self-contained."
        )

    let writeOutIns s = writeOutCode ("    " + s)

    (TranslateInstructionToAsmSequence ThunkIn) |> List.iter writeOutIns
    writeOutCode "ret"



let WriteOutWasmMem writeOutData writeOutVar i (thisMem:InternalMemoryRecord) =

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

    writeOutVar "global"
    writeOutVar "    align ptr"
    writeOutVar (sprintf "    %s%d: %d" AsmMemPrefix i linearMemorySize)

    let writeIns s = writeOutData ("    " + s)

    writeOutData (sprintf "// Data for WASM mem %s%d" AsmMemoryNamePrefix i) // TODO: If there is none, omit this.

    thisMem.InitData |> Array.iteri (fun j (_, byteArray) ->
        writeOutData (sprintf "data %s%d_%d" AsmMemoryNamePrefix i j)
        WriteOutHexDump "byte" "," "0x" writeIns byteArray
    )




let WriteOutWasmGlobal writeOut i (m:Module) (g:InternalGlobalRecord) =

    // TODO: We do nothing with the immutability information.  Could we avoid a store and hoist the constant into the code?

    let initValue = StaticEvaluate g.InitExpr
    let globalIdx = GlobalIdx(U32(uint32 i))   // TODO: not ideal construction of temporary

    writeOut (sprintf "data %s int %d" (GlobalIdxNameString globalIdx) initValue)





let WriteOutInstructionsToText writeOut instructionsList thisFuncType =

    let writeIns s = writeOut ("    " + s)

    // Kick off the whole thing here:

    instructionsList |> List.iter (fun i -> TranslateInstructionToAsmSequence i |> List.iter writeIns)

    // Handle the function's return (may need pop into A):

    let returnHandlingCode = 
        match thisFuncType |> ReturnsSingleValue with
            | true  -> TranslateInstructionToAsmSequence (Pop A)  // TODO: not ideal construction of temporary
            | false -> []

    returnHandlingCode |> List.iter writeIns




let WriteOutBranchTables writeOut funcInstructions =

    let writeIns s = writeOut ("    " + s)

    funcInstructions |> List.iter (fun ins ->
        match ins with
            | GotoIndex(LabelName tableLabel,_,_,codePointLabels) ->
                writeOut "align ptr"
                writeOut (sprintf "data %s" tableLabel)
                codePointLabels |> Array.iter (fun (LabelName lbl) -> writeIns (sprintf "ptr %s" lbl))
            | _ -> ()
        )



let WriteOutFunction writeOut thisFuncType thisFuncLocals funcInstructions config =   // TODO:  Can we reduce f to inner components ?

    let phase1 = 
        match config with
            | TranslationConfiguration(_,FullyOptimised) -> funcInstructions |> Optimise
            | TranslationConfiguration(_,NoOptimisation) -> funcInstructions

    let phase2 =
        match config with
            | TranslationConfiguration(WithBarriers,_)    -> phase1
            | TranslationConfiguration(WithoutBarriers,_) -> phase1 |> RemoveBarriers

    let desiredInstructions = phase2

    WriteOutInstructionsToText writeOut desiredInstructions thisFuncType
    writeOut (ReturnCommandFor thisFuncType thisFuncLocals)



let WriteOutFunctionAndBranchTables writeOut writeOutTables funcIndex (m:Module) translationState config (f:InternalFunctionRecord) =   // TODO: module only needed to query function metadata in TranslateInstructions

    let funcInstructions, updatedTranslationState = 
        f.Body |> TranslateInstructions m.Funcs translationState

    let procedureCommand = 
        sprintf "procedure %s%d%s" AsmInternalFuncNamePrefix funcIndex (AsmSignatureOf f.FuncType)

    try
        writeOut procedureCommand
        WriteOutFunctionLocals writeOut f.FuncType f.Locals
        WriteOutFunction writeOut f.FuncType f.Locals funcInstructions config
        WriteOutBranchTables writeOutTables funcInstructions
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

    ("Translation of WASM module: " + headingText) |> toComment |> writeOutData
    writeOutData ""

    m.Tables |> Array.iteri (fun i t ->
        match t with
            | InternalTable2(tbl) -> tbl |> WriteOutWasmTable writeOutData i m 
            | ImportedTable2(tbl) -> failwith "Error:  Cannot support importing a 'table'.  WASM module must be self-contained."
        )

    m.Globals |> Array.iteri (fun i g ->
        match g with
            | InternalGlobal2(glo) -> glo |> WriteOutWasmGlobal writeOutData i m 
            | ImportedGlobal2(glo) -> failwith "Error:  Cannot support importing a 'global'.  WASM module must be self-contained."
        )

    m.Mems |> Array.iteri (fun i me ->
        match me with
            | InternalMemory2(mem) -> mem |> WriteOutWasmMem writeOutData writeOutVar i 
            | ImportedMemory2(mem) -> failwith "Error:  Cannot support importing a 'memory'.  WASM module must be self-contained."
        )

    m.Mems |> WriteOutAllDataInitialisationFunction writeOutCode

    let mutable moduleTranslationState = ModuleTranslationState(0)  // TODO: hide ideally

    m.Funcs |> Array.iteri (fun i g ->
        match g with 

            | InternalFunction2(g) -> 
                moduleTranslationState <- 
                    g |> WriteOutFunctionAndBranchTables writeOutCode writeOutData i m moduleTranslationState config

            | ImportedFunction2({Import={ImportModuleName=m; ImportName=n}}) ->
                (sprintf "WASM Import: %s.%s" m n) |> toComment |> writeOutCode 
        )

    WriteOutWasmStart WriteOutBranchToEntryLabel writeOutCode toComment m.Start m.Funcs



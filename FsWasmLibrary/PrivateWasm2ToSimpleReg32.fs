module PrivateWasm2ToSimpleReg32

open Wasm
open Wasm2
open Wasm2ToSimpleReg32ConfigTypes
open SimpleReg32
open SimpleReg32Constants
open OptimiseSimpleReg32
open System.Text



let (-+-) a b =
    U32(match a with U32(a2) -> match b with I32(b2) -> a2 + uint32 b2)



let FuncLabelFor fidx (moduleFuncsArray:Function2[]) =
    match fidx with
        | FuncIdx(U32(i)) -> 
            match moduleFuncsArray.[int i] with
                | ImportedFunction2({Import={ImportModuleName=m; ImportName=n}}) -> 
                    LabelName(AsmInternalFuncNamePrefix + m + "_" + n)
                | InternalFunction2(f) ->
                    LabelName(AsmInternalFuncNamePrefix + i.ToString()) 



type ModuleTranslationState =
    | ModuleTranslationState of labelCount:int



let TranslateInstructions (moduleFuncsArray:Function2[]) translationState (ws:Wasm.Instr list) =

    let mutable labelCount = match translationState with ModuleTranslationState(count) -> count
    let mutable labelStack = new ResizeArray<LABELNAME>()

    let newLabel () =
        labelCount <- labelCount + 1
        LabelName(sprintf "%s%d" AsmCodeLabelPrefix labelCount)

    let pushNewLabel () =
        let l = newLabel ()
        labelStack.Add l
        l

    let popLabel () =
        labelStack.RemoveAt(labelStack.Count - 1)

    let labelFor d =
        labelStack.[labelStack.Count - (1 + match d with LabelIdx(U32(i)) -> (int i))]

    let returnLabel = newLabel ()

    let getFuncType (fidx:FuncIdx) =
        match fidx with
            | FuncIdx(U32(i)) -> 
                match moduleFuncsArray.[int i] with
                    | ImportedFunction2(f) -> f.FuncType
                    | InternalFunction2(f) -> f.FuncType

    let thunkInIfNeeded fidx = 
        match fidx with
            | FuncIdx(U32(i)) -> 
                match moduleFuncsArray.[int i] with
                    | ImportedFunction2(_) -> [ ThunkIn ]   // Since we called an imported function, we reload Y on return.
                    | InternalFunction2(_) -> []
 
    let pushAnyReturn fidx =
        let ft = (getFuncType fidx)
        match ft.ReturnTypes.Length with
            | 0 -> []
            | 1 -> [ PushA ]
            | _ -> failwith "Cannot translate functions which return more than 1 result"

    let rec TranslateInstrList ws =

        List.concat (ws |> List.map TranslateInstr)

    and TranslateInstr w =

        let binaryCommutativeOp lhs rhs op = 
            (TranslateInstr lhs) @ 
            (TranslateInstr rhs) @ 
            [
                PopA; // RHS operand
                PopB; // LHS operand
                op;   // Result in A
                PushA; 
                Barrier 
            ]

        let binaryOpWithConst lhs getOp =
            (TranslateInstr lhs) @ 
            [
                PopA;       // LHS operand
                getOp ();   // Result in A
                PushA; 
                Barrier 
            ]

        let compareOp lhs rhs op = 
            (TranslateInstr lhs) @ 
            (TranslateInstr rhs) @ 
            [
                PopA; // RHS operand
                PopB; // LHS operand
                op;   // Compare B (LHS) with A (RHS) and set boolean into A
                PushA; 
                Barrier 
            ]

        let binaryNonCommutativeOp lhs rhs op = 
            (TranslateInstr lhs) @ (TranslateInstr rhs)  @ 
            [
                PopA; // RHS operand
                PopB; // LHS operand
                op;   // Result in B
                LetAB;
                PushA; 
                Barrier 
            ]

        let shiftOp lhs rhs op = 
            (TranslateInstr lhs) @ (TranslateInstr rhs)  @ 
            [
                PopA;  // RHS operand
                PopB;  // LHS operand
                LetCA;
                op;    // Result in B
                LetAB;
                PushA; 
                Barrier 
            ]

        let translateConstruct sourceBody putInOrder =
            let constructLabel = pushNewLabel ()
            let translatedBody = TranslateInstrList sourceBody
            popLabel ()
            (putInOrder translatedBody [ Label(constructLabel) ]) @ [ Barrier ]

        match w with

            | Unreachable -> [ Breakpoint; Barrier ]
            | Nop -> []

            // The following translations must all end with Barrier

            | Instr.Drop(ins) -> TranslateInstr(ins) @ [ Drop; Barrier ]
        
            | Select(a,b,c) -> 
                let l1 = newLabel ()
                let l2 = newLabel ()
                TranslateInstr(a)  @ 
                TranslateInstr(b)  @ 
                TranslateInstr(c)  @ 
                [
                    PopA; 
                    PopB; // val2  (desired if A == 0)
                    PopC; // val1  (desired if A <> 0)
                    BranchAZ(l1);
                    LetAC;
                    Goto(l2);
                    Label(l1);
                    LetAB;
                    Label(l2);
                    PushA;
                    Barrier;
                ]
   
            | Block(_, body) -> translateConstruct body (List.append)
            | Loop(_, body)  -> translateConstruct body (fun a b -> List.append b a)

            | If(_, body) -> 
                let skipLabel = pushNewLabel ()
                let translatedBody = TranslateInstrList body
                popLabel ()
                [ PopA; BranchAZ(skipLabel); ] 
                    @ translatedBody 
                    @ [ Label(skipLabel); Barrier ]
                    
            | IfElse(_, ifbody, elsebody) ->

                let skipIfLabel = pushNewLabel ()
                let translatedIfBody = TranslateInstrList ifbody
                popLabel ()

                let skipElseLabel = pushNewLabel ()
                let translatedElseBody = TranslateInstrList elsebody
                popLabel ()

                [ PopA; BranchAZ(skipIfLabel); ] 
                    @ translatedIfBody 
                    @ [ Goto(skipElseLabel); Label(skipIfLabel); Barrier; ] 
                    @ translatedElseBody 
                    @ [ Label(skipElseLabel); Barrier ]

            | Br(target) -> [ Goto(labelFor target); Barrier ]

            | BrIf(cond,target) -> TranslateInstr(cond) @ [ PopA; BranchANZ(labelFor target); Barrier ]

            | BrTable(indexExpression, labelArray, defaultLabel) -> 
                let tableLabel = newLabel ()
                (TranslateInstr indexExpression)  @ 
                [
                    PopA; 
                    GotoIndex(tableLabel, labelArray.Length, labelFor defaultLabel, Array.map labelFor labelArray);
                    Barrier
                ]

            | Return -> [ Goto(returnLabel); Barrier ]
            
            | Call(funcIdx, argsList) -> 
                let codeToPushArguments = argsList |> TranslateInstrList
                codeToPushArguments
                    @ [ CallFunc(FuncLabelFor funcIdx moduleFuncsArray) ]
                    @ (thunkInIfNeeded funcIdx) 
                    @ (pushAnyReturn funcIdx) 
                    @ [ Barrier ]

            | CallIndirect(funcType, argsList, indexExpr) ->
                // TODO: runtime validation of funcType, in order to be safe per-spec
                let codeToPushArguments = argsList |> TranslateInstrList
                let translatedIndex     = indexExpr |> TranslateInstr   // TODO: We could see if this would statically evaluate, and reduce this to a regular call, although the compiler would probably optimise that.
                codeToPushArguments 
                    @ translatedIndex 
                    @ [ PopA; CallTableIndirect; Barrier ]

            | I32Const(I32(C)) -> [ ConstA(Const32(C)); PushA; Barrier ]

            | GetLocal(L)   -> [ FetchLocA(L); PushA; Barrier ]
            | SetLocal(L,V) -> (TranslateInstr V) @ [ PopA;  StoreALoc(L); Barrier ]
            | TeeLocal(L,V) -> (TranslateInstr V) @ [ PeekA; StoreALoc(L); Barrier ]
        
            | GetGlobal(G)   -> [ FetchGloA(G); PushA; Barrier ]
            | SetGlobal(G,V) -> (TranslateInstr V) @ [ PopA; StoreAGlo(G);  Barrier ]

                    // TODO: runtime restriction of addressing to the Linear Memory extent.
                    // (Wouldn't fit my application anyway, since it will not be possible
                    // to guarantee contiguous extension of the Linear Memory).

            | I32Store8(  {Align=_;       Offset=O}, I32Const(O2), I32Const(v)) -> [ StoreConst8toY( O -+- O2,v); Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32(1u); Offset=O}, I32Const(O2), I32Const(v)) -> [ StoreConst16toY(O -+- O2,v); Barrier ]
            | I32Store(   {Align=U32(2u); Offset=O}, I32Const(O2), I32Const(v)) -> [ StoreConst32toY(O -+- O2,v); Barrier ]

            | I32Store8(  {Align=_;       Offset=O},          lhs, I32Const(v)) -> (TranslateInstr lhs) @ [ PopA; AddAY; StoreConst8toA(O,v);  Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32(1u); Offset=O},          lhs, I32Const(v)) -> (TranslateInstr lhs) @ [ PopA; AddAY; StoreConst16toA(O,v); Barrier ]
            | I32Store(   {Align=U32(2u); Offset=O},          lhs, I32Const(v)) -> (TranslateInstr lhs) @ [ PopA; AddAY; StoreConst32toA(O,v); Barrier ]

            | I32Store8(  {Align=_;       Offset=O}, I32Const(O2),         rhs) -> (TranslateInstr rhs) @ [ PopA; Store8AtoY( O -+- O2); Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32(1u); Offset=O}, I32Const(O2),         rhs) -> (TranslateInstr rhs) @ [ PopA; Store16AtoY(O -+- O2); Barrier ]
            | I32Store(   {Align=U32(2u); Offset=O}, I32Const(O2),         rhs) -> (TranslateInstr rhs) @ [ PopA; Store32AtoY(O -+- O2); Barrier ]

            | I32Store8(  {Align=_;       Offset=O},          lhs,         rhs) -> (TranslateInstr lhs) @ (TranslateInstr rhs) @ [ PopA; PopB; AddBY; Store8AtoB(O);  Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32(1u); Offset=O},          lhs,         rhs) -> (TranslateInstr lhs) @ (TranslateInstr rhs) @ [ PopA; PopB; AddBY; Store16AtoB(O); Barrier ]
            | I32Store(   {Align=U32(2u); Offset=O},          lhs,         rhs) -> (TranslateInstr lhs) @ (TranslateInstr rhs) @ [ PopA; PopB; AddBY; Store32AtoB(O); Barrier ]

            | I32Store16( {Align=U32(_);  Offset=_},   _,   _) -> failwith "Cannot translate 16-bit store unless alignment is 2 bytes"
            | I32Store(   {Align=U32(_);  Offset=_},   _,   _) -> failwith "Cannot translate 32-bit store unless alignment is 4 bytes"

            | I32Load8s(  {Align=_;       Offset=O}, I32Const(O2)) -> [ Fetch8sFromY( O -+- O2); PushA; Barrier ]
            | I32Load8u(  {Align=_;       Offset=O}, I32Const(O2)) -> [ Fetch8uFromY( O -+- O2); PushA; Barrier ]
            | I32Load16s( {Align=U32(1u); Offset=O}, I32Const(O2)) -> [ Fetch16sFromY(O -+- O2); PushA; Barrier ]
            | I32Load16u( {Align=U32(1u); Offset=O}, I32Const(O2)) -> [ Fetch16uFromY(O -+- O2); PushA; Barrier ]
            | I32Load(    {Align=U32(2u); Offset=O}, I32Const(O2)) -> [ Fetch32FromY( O -+- O2); PushA; Barrier ]

            | I32Load8s(  {Align=_;       Offset=O}, operand) -> (TranslateInstr operand) @ [ PopA; AddAY; Fetch8sFromA(O);  PushA; Barrier ]
            | I32Load8u(  {Align=_;       Offset=O}, operand) -> (TranslateInstr operand) @ [ PopA; AddAY; Fetch8uFromA(O);  PushA; Barrier ]
            | I32Load16s( {Align=U32(1u); Offset=O}, operand) -> (TranslateInstr operand) @ [ PopA; AddAY; Fetch16sFromA(O); PushA; Barrier ]
            | I32Load16u( {Align=U32(1u); Offset=O}, operand) -> (TranslateInstr operand) @ [ PopA; AddAY; Fetch16uFromA(O); PushA; Barrier ]
            | I32Load(    {Align=U32(2u); Offset=O}, operand) -> (TranslateInstr operand) @ [ PopA; AddAY; Fetch32FromA(O);  PushA; Barrier ]
            
            | I32Load16s( {Align=U32(A);  Offset=_}, _) -> failwith "Cannot translate 16-bit sign-extended load unless alignment is 2 bytes"
            | I32Load16u( {Align=U32(A);  Offset=_}, _) -> failwith "Cannot translate 16-bit unsigned load unless alignment is 2 bytes"
            | I32Load(    {Align=U32(A);  Offset=_}, _) -> failwith "Cannot translate 32-bit load unless alignment is 4 bytes"

            | I32Eqz(operand) -> (TranslateInstr operand) @ [ PopA; CmpAZ; PushA; ] 

            | I32Eq(a,b)   -> compareOp a b CmpEqBA 
            | I32Ne(a,b)   -> compareOp a b CmpNeBA

            | I32Lts(a,b)  -> compareOp a b CmpLtsBA
            | I32Ltu(a,b)  -> compareOp a b CmpLtuBA
            | I32Gts(a,b)  -> compareOp a b CmpGtsBA
            | I32Gtu(a,b)  -> compareOp a b CmpGtuBA
            | I32Les(a,b)  -> compareOp a b CmpLesBA
            | I32Leu(a,b)  -> compareOp a b CmpLeuBA
            | I32Ges(a,b)  -> compareOp a b CmpGesBA
            | I32Geu(a,b)  -> compareOp a b CmpGeuBA

            | I32Add (a,I32Const(n)) -> binaryOpWithConst a (fun () -> AddAN(n))
            | I32Sub (a,I32Const(n)) -> binaryOpWithConst a (fun () -> SubAN(n))
            | I32And (a,I32Const(n)) -> binaryOpWithConst a (fun () -> AndAN(n))
            | I32Or  (a,I32Const(n)) -> binaryOpWithConst a (fun () -> OrAN (n))
            | I32Xor (a,I32Const(n)) -> binaryOpWithConst a (fun () -> XorAN(n))

            | I32Add (a,b) -> binaryCommutativeOp     a b AddAB
            | I32Sub (a,b) -> binaryNonCommutativeOp  a b SubBA
            | I32Mul (a,b) -> binaryCommutativeOp     a b MulAB
            | I32Divs(a,b) -> binaryNonCommutativeOp  a b DivsBA
            | I32Divu(a,b) -> binaryNonCommutativeOp  a b DivuBA
            | I32Rems(a,b) -> binaryNonCommutativeOp  a b RemsBA
            | I32Remu(a,b) -> binaryNonCommutativeOp  a b RemuBA
            | I32And (a,b) -> binaryCommutativeOp     a b AndAB
            | I32Or  (a,b) -> binaryCommutativeOp     a b OrAB
            | I32Xor (a,b) -> binaryCommutativeOp     a b XorAB
            | I32Shl (a,b) -> shiftOp a b ShlBC
            | I32Shrs(a,b) -> shiftOp a b ShrsBC
            | I32Shru(a,b) -> shiftOp a b ShruBC
            | I32Rotl(a,b) -> shiftOp a b RotlBC
            | I32Rotr(a,b) -> shiftOp a b RotrBC

            | _ -> failwith "Cannot translate this instruction to simple 32-bit machine."

    // Do the translation with the above nested functions:

    let finalTranslation = (TranslateInstrList ws) @ [ Label(returnLabel) ] 
    let updatedTranslationState = ModuleTranslationState(labelCount)

    (finalTranslation, updatedTranslationState)










let StaticEvaluate (instrs:Instr list) : int =

    match instrs.Length with // TODO: proper list matching
        | 0 -> failwith "Cannot statically evaluate empty sequence of instructions"
        | 1 -> ()
        | _ -> failwith "Cannot statically evaluate instruction sequence because we only support a single instruction, and there is more than one"

    match instrs.[0] with
        | I32Const(I32(n)) -> n
        | _ -> failwith "Cannot statically evaluate instruction sequence -- unsupported single instruction"  // TODO: clarify







let Surrounded (before:string) (after:string) (content:string) =
    match content.Length with
        | 0 -> ""
        | _ -> before + content + after


let Bracketed s =           s |> Surrounded "(" ")"
let Prefixed thePrefix s =  s |> Surrounded thePrefix ""
let ColonPrefixed s =       s |> Prefixed ": "




let GlobalIdxAsUint32 i = match i with GlobalIdx(U32(i)) -> i
let LocalIdxAsUint32  i = match i with LocalIdx(U32(i)) -> i
let FuncIdxAsUint32   i = match i with FuncIdx(U32(i)) -> i

let LabelTextOf l = match l with LabelName(n) -> n
let FuncNameOf  l = match l with LabelName(n) -> n   // TODO: type usage isn't right: no distinction

let GlobalIdxNameString globalIdx = 
    sprintf "%s%d" AsmGlobalNamePrefix (GlobalIdxAsUint32 globalIdx)

let FuncIdxNameString funcIdx = 
    sprintf "%s%d" AsmInternalFuncNamePrefix (FuncIdxAsUint32 funcIdx)





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




let WriteOutFunctionLocals writeOut (funcType:FuncType) valTypes =

    let localBaseIndex = funcType.ParameterTypes.Length

    valTypes |> Array.iteri (fun arrayIndex v ->
        let indexOfVariable = localBaseIndex + arrayIndex
        let prefixStr = if arrayIndex = 0 then "var " else "  , "
        writeOut (sprintf "%s@%s%d:%s" prefixStr AsmLocalNamePrefix indexOfVariable (ValTypeTranslationOf v)))



let ReturnsSingleValue (ft:FuncType) =
    match ft.ReturnTypes.Length with
        | 0 -> false
        | 1 -> true
        | _ -> failwith "Cannot translate function that returns more than one value"



let ReturnCommandFor (ft:FuncType) =
    match ft.ParameterTypes.Length with
        | 0 -> "ret"
        | _ -> "endproc"    // TODO: Not sure if this can't just be endproc everywhere now in the ASM ?






let WriteOutWasmTable writeOut i (m:Module2) (t:InternalTable2Record) =

    let writeIns s = writeOut ("    " + s)  // TODO: repetition throughout routines!

    writeOut (sprintf "data %s%d" AsmTableNamePrefix i)

    if t.InitData.Length > 1 then failwith "Cannot translate module with more than one Elem in a TableSec table"

    t.InitData |> Array.iter (fun elem ->
            let ofsExpr, funcIdxList = elem
            let ofsValue = StaticEvaluate ofsExpr
            if ofsValue <> 0 then failwith "Cannot translate module with TableSec table that has Elem with non-zero data initialisation offset"
            funcIdxList |> Array.iter (fun funcIdx -> writeIns (FuncIdxNameString funcIdx))
        )



let WasmMemoryBlockMultiplier = 65536u



let WriteOutLoadLinearMemoryRegister writeOutCode =
    // The translated code requires the Y register to
    // point to the base of the linear memory region.
    // Note: There is only *one* linear memory supported in WASM 1.0  (mem #0)
    writeOutCode (sprintf "let Y=%s%d" AsmMemPrefix 0)
    



let WriteOutAllDataInitialisationFunction  writeOutCode (mems:Memory2[]) =

    writeOutCode (sprintf "procedure init_%s" AsmMemPrefix)
    writeOutCode (sprintf "    // Caller must pass stack pointer (relative to %s%d) in A" AsmMemPrefix 0)
    writeOutCode (sprintf "    let uint [%s%d+4]=A // Initialise WasmFiddle stack pointer at address offset 4" AsmMemPrefix 0)

    let writeOutDataCopyCommand i (thisMem:InternalMemory2Record) =
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

    WriteOutLoadLinearMemoryRegister writeOutCode
    writeOutCode "ret"



let WriteOutHexDump writeLine byteArray =
    
    let sb = new StringBuilder()
    
    byteArray |> Array.iteri (fun i byteVal -> 

        let c = i &&& 15
        
        sb.Append (
            match c with
                | 0 -> sprintf "byte 0x%02X" byteVal
                | _ -> sprintf ",0x%02X" byteVal
            )
            |> ignore

        match c with
            | 15 -> 
                writeLine (sb.ToString())
                sb.Clear() |> ignore
            | _ -> ()
    )

    if sb.Length > 0 then writeLine (sb.ToString())



let WriteOutWasmMem writeOutData writeOutVar i (thisMem:InternalMemory2Record) =

    let linearMemorySize = 
        match thisMem with 
            | { MemoryType={ MemoryLimits=lims } } -> 
            match lims with 
                | { LimitMin=U32(0u); LimitMax=None; }      -> failwith "Cannot translate module with Mem that is size 0"   // At least we expect to initalise GCC's stack pointer.
                | { LimitMin=U32(memSize); LimitMax=None; } -> memSize * WasmMemoryBlockMultiplier
                | { LimitMin=_; LimitMax=Some(_); }         -> failwith "Cannot translate module with Mem that has max size limit"

    writeOutVar (sprintf "var %s%d: %d" AsmMemPrefix i linearMemorySize)

    let writeIns s = writeOutData ("    " + s)

    writeOutData (sprintf "// Data for WASM mem %s%d" AsmMemoryNamePrefix i)

    thisMem.InitData |> Array.iteri (fun j elem ->
            let _, byteArray = elem
            writeOutData (sprintf "data %s%d_%d" AsmMemoryNamePrefix i j)
            WriteOutHexDump writeIns byteArray
        )




let WriteOutWasmGlobal writeOut i (m:Module2) (g:InternalGlobal2Record) =

    // TODO: We do nothing with the immutability information.  Could we avoid a store and hoist the constant into the code?

    let initValue = StaticEvaluate g.InitExpr
    let globalIdx = GlobalIdx(U32(uint32 i))   // TODO: not ideal construction of temporary

    writeOut (sprintf "data %s int %d" (GlobalIdxNameString globalIdx) initValue)




let WriteOutInstructionsToText writeOut instrs thisFuncType =

    let writeIns s = writeOut ("    " + s)

    let writeOfs u = 
        match u with
            | U32(0u) -> ""   // indexed addressing not needed
            | U32(n)  -> ("+" + n.ToString())   // indexed addressing

    let writeU32    s1 u s2   = writeIns (sprintf "%s%s%s"   s1 (writeOfs u) s2)
    let writeU32I32 s1 u s2 n = writeIns (sprintf "%s%s%s%d" s1 (writeOfs u) s2 n)
    let writeLoc    s1 i s2   = writeIns (sprintf "%s%d%s"   s1 (LocalIdxAsUint32 i) s2)

    let writeGotoIndex tableLabel numMax defaultLabel =
        // A is already the index to branch to
        writeIns (sprintf "cmp A,%d:if >>= goto %s" numMax (LabelTextOf defaultLabel))
        writeIns "shl A,2"  // TODO: assumes 32-bit target
        writeIns (sprintf "goto [A+%s]" (LabelTextOf tableLabel))

    let writeCallTableIndirect () =
        // TODO: We really need to emit some code to validate the signatures.
        // A is already the index to call.
        // TODO: We need to validate index A lies within wasm table [0]
        writeIns "shl A,2"  // TODO: assumes 32-bit target
        writeIns (sprintf "goto [A+%s0]" AsmTableNamePrefix)  // WASM 1.0 always looks in table #0

    let instructionToText ins =  // These translations can assume a 32-bit target for now.

        match ins with
            | Barrier               -> writeIns "// ~~~ register barrier ~~~"
            | Breakpoint            -> writeIns "break"
            | Drop                  -> writeIns "add SP,4"  // TODO: Assumes 32-bit target
            | Label(l)              -> writeOut ("label " + LabelTextOf l)   // TODO: sort out ASM local label references
            | ConstA(Const32(n))    -> writeIns (sprintf "let A=%d" n)
            | Goto(l)               -> writeIns ("goto " + LabelTextOf l)
            | CallFunc(l)           -> writeIns ("call " + FuncNameOf l)
            | CallTableIndirect     -> writeCallTableIndirect ()
            | BranchAZ(l)           -> writeIns ("cmp A,0:if z goto " + LabelTextOf l)
            | BranchANZ(l)          -> writeIns ("cmp A,0:if nz goto " + LabelTextOf l)
            | GotoIndex(t,n,d,_)    -> writeGotoIndex t n d   // The ignored parameter is the lookup table, which we separately output.
            | PushA                 -> writeIns "push A"
            | PeekA                 -> writeIns "let A=int [SP]"  // TODO: Assumes 32-bit target
            | PopA                  -> writeIns "pop A"
            | PopB                  -> writeIns "pop B"
            | PopC                  -> writeIns "pop C"
            | LetAB                 -> writeIns "let A=B"
            | LetAC                 -> writeIns "let A=C"
            | LetCA                 -> writeIns "let C=A"
            | AddAY                 -> writeIns "add A,Y"  // used for address space relocation
            | AddBY                 -> writeIns "add B,Y"  // used for address space relocation
            | AddAN(I32(n))         -> writeIns (sprintf "add A,%d" n)
            | SubAN(I32(n))         -> writeIns (sprintf "sub A,%d" n)
            | AndAN(I32(n))         -> writeIns (sprintf "and A,%d" n)
            | OrAN(I32(n))          -> writeIns (sprintf "or A,%d"  n)
            | XorAN(I32(n))         -> writeIns (sprintf "xor A,%d" n)
            | AddAB                 -> writeIns "add A,B"  // commutative
            | SubBA                 -> writeIns "sub B,A"
            | MulAB                 -> writeIns "mul A,B"  // commutative
            | DivsBA | DivuBA | RemsBA | RemuBA -> failwith "Assembler does not have a division instruction"
            | AndAB                 -> writeIns "and A,B"  // commutative
            | OrAB                  -> writeIns "or A,B"   // commutative
            | XorAB                 -> writeIns "xor A,B"  // commutative
            | ShlBC                 -> writeIns "shl B,C"
            | ShrsBC                -> writeIns "sar B,C"
            | ShruBC                -> writeIns "shr B,C"
            | RotlBC | RotrBC       -> failwith "Assembler does not have a rotate instruction"
            | CmpEqBA               -> writeIns "cmp B,A:set z A"
            | CmpNeBA               -> writeIns "cmp B,A:set nz A"
            | CmpLtsBA              -> writeIns "cmp B,A:set < A"
            | CmpLtuBA              -> writeIns "cmp B,A:set << A"
            | CmpGtsBA              -> writeIns "cmp B,A:set > A"
            | CmpGtuBA              -> writeIns "cmp B,A:set >> A"
            | CmpLesBA              -> writeIns "cmp B,A:set <= A"
            | CmpLeuBA              -> writeIns "cmp B,A:set <<= A"
            | CmpGesBA              -> writeIns "cmp B,A:set >= A"
            | CmpGeuBA              -> writeIns "cmp B,A:set >>= A"
            | CmpAZ                 -> writeIns "cmp B,A:set z A"
            | FetchLocA(i)          -> writeLoc ("let A=int[@" + AsmLocalNamePrefix) i "]"  // TODO: Assumes 32-bit target
            | StoreALoc(i)          -> writeLoc ("let int[@" + AsmLocalNamePrefix) i "]=A"  // TODO: Assumes 32-bit target
            | FetchGloA(i)          -> writeIns (sprintf "let A=int[%s]" (GlobalIdxNameString i))  // TODO: Eventually use the type rather than "int"
            | StoreAGlo(i)          -> writeIns (sprintf "let int[%s]=A" (GlobalIdxNameString i))  // TODO: Eventually use the type rather than "int"
            | StoreConst8toA(ofs,I32(v))   -> writeU32I32 "let byte[A" ofs "]=" v  
            | StoreConst16toA(ofs,I32(v))  -> writeU32I32 "let ushort[A" ofs "]=" v
            | StoreConst32toA(ofs,I32(v))  -> writeU32I32 "let uint[A" ofs "]=" v  
            | StoreConst8toY(ofs,I32(v))   -> writeU32I32 "let byte[Y" ofs "]=" v
            | StoreConst16toY(ofs,I32(v))  -> writeU32I32 "let ushort[Y" ofs "]=" v
            | StoreConst32toY(ofs,I32(v))  -> writeU32I32 "let uint[Y" ofs "]=" v
            | Store8AtoB(ofs)         -> writeU32 "let byte[B" ofs "]=A"
            | Store16AtoB(ofs)        -> writeU32 "let ushort[B" ofs "]=A"
            | Store32AtoB(ofs)        -> writeU32 "let uint[B" ofs "]=A"
            | Store8AtoY(ofs)         -> writeU32 "let byte[Y" ofs "]=A"
            | Store16AtoY(ofs)        -> writeU32 "let ushort[Y" ofs "]=A"
            | Store32AtoY(ofs)        -> writeU32 "let uint[Y" ofs "]=A"
            | Fetch8sFromA(ofs)       -> writeU32 "let A=sbyte[A" ofs "]"
            | Fetch8uFromA(ofs)       -> writeU32 "let A=byte[A" ofs "]"
            | Fetch16sFromA(ofs)      -> writeU32 "let A=short[A" ofs "]"
            | Fetch16uFromA(ofs)      -> writeU32 "let A=ushort[A" ofs "]"
            | Fetch32FromA(ofs)       -> writeU32 "let A=uint[A" ofs "]"
            | Fetch8sFromY(ofs)       -> writeU32 "let A=sbyte[Y" ofs "]"
            | Fetch8uFromY(ofs)       -> writeU32 "let A=byte[Y" ofs "]"
            | Fetch16sFromY(ofs)      -> writeU32 "let A=short[Y" ofs "]"
            | Fetch16uFromY(ofs)      -> writeU32 "let A=ushort[Y" ofs "]"
            | Fetch32FromY(ofs)       -> writeU32 "let A=uint[Y" ofs "]"
            | ThunkIn                 -> WriteOutLoadLinearMemoryRegister writeIns

    // Kick off the whole thing here:

    instrs |> List.iter instructionToText

    // Handle the function's return (may need pop into A):

    match thisFuncType |> ReturnsSingleValue with
        | true  -> writeIns "pop A"
        | false -> ()

    


let WriteOutBranchTables writeOut funcInstructions =

    let writeIns s = writeOut ("    " + s)

    funcInstructions |> List.iter (fun ins ->
        match ins with
            | GotoIndex(tableLabel,_,_,codePointLabels) ->
                writeOut (sprintf "data %s" (LabelTextOf tableLabel))
                codePointLabels |> Array.iter (fun lbl -> writeIns (sprintf "int %s" (LabelTextOf lbl)))
                writeOut ""
            | _ -> ()
        )



let WriteOutFunction writeOut thisFuncType funcInstructions config =   // TODO:  Can we reduce f to inner components ?

    let phase1 = 
        match config with
            | WriteOutFunctionConfig(_,FullyOptimised) -> funcInstructions |> Optimise
            | WriteOutFunctionConfig(_,NoOptimisation) -> funcInstructions
    
    let phase2 =
        match config with
            | WriteOutFunctionConfig(WithBarriers,_)    -> phase1
            | WriteOutFunctionConfig(WithoutBarriers,_) -> phase1 |> RemoveBarriers

    let desiredInstructions = phase2

    WriteOutInstructionsToText writeOut desiredInstructions thisFuncType
    writeOut (ReturnCommandFor thisFuncType)



let WriteOutFunctionAndBranchTables writeOut writeOutTables funcIndex (m:Module2) translationState config (f:InternalFunction2Record) =   // TODO: module only needed to query function metadata in TranslateInstructions
    
    let funcInstructions, updatedTranslationState = 
        f.Body |> TranslateInstructions m.Funcs translationState

    let procedureCommand = (sprintf "procedure %s%d%s" AsmInternalFuncNamePrefix funcIndex (AsmSignatureOf f.FuncType))

    try
        writeOut procedureCommand
        WriteOutFunctionLocals writeOut f.FuncType f.Locals
        WriteOutFunction writeOut f.FuncType funcInstructions config
        WriteOutBranchTables writeOutTables funcInstructions
    with
        | _ as ex -> failwith (sprintf "Error in %s:  %s" procedureCommand (ex.ToString()))

    updatedTranslationState



let WriteOutBranchToEntryLabel writeOut startFuncIdx moduleFuncsArray =
    writeOut ("procedure " + AsmEntryPointLabel)
    writeOut (sprintf "goto %s" (match (FuncLabelFor startFuncIdx moduleFuncsArray) with LabelName(s) -> s))



let WriteOutWasmStart writeOut startOption moduleFuncsArray =
    match startOption with 
        | Some({StartFuncIdx=startFuncIdx}) -> 
            WriteOutBranchToEntryLabel writeOut startFuncIdx moduleFuncsArray
        | None -> writeOut "// No entry point in this translation"



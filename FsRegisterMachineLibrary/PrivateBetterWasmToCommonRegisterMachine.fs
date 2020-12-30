module PrivateBetterWasmToCommonRegisterMachine

open WasmFileTypes
open WasmBetterTypes
open BWToCRMConfigurationTypes
open CommonRegisterMachineTypes
open AsmPrefixes
open OptimiseCommonRegisterMachine
open System.Text


let RegNameOf = function
        | A -> "A"
        | B -> "B"
        | C -> "C"
        | Y -> "Y"



let (-+-) a b =
    U32(match a with U32(a2) -> match b with I32(b2) -> a2 + uint32 b2)



let FuncLabelFor fidx (moduleFuncsArray:Function[]) =
    match fidx with
        | FuncIdx(U32(i)) -> 
            match moduleFuncsArray.[int i] with
                | ImportedFunction2({Import={ImportModuleName=m; ImportName=n}}) -> 
                    LabelName(AsmInternalFuncNamePrefix + m + "_" + n)
                | InternalFunction2(f) ->
                    LabelName(AsmInternalFuncNamePrefix + i.ToString()) 



type ModuleTranslationState =
    | ModuleTranslationState of labelCount:int



let TranslateInstructions (moduleFuncsArray:Function[]) translationState (ws:WasmFileTypes.Instr list) =

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
            | 1 -> [ Push(A) ]
            | _ -> failwith "Cannot translate functions which return more than 1 result"

    let rec translateInstrList ws =

        List.concat (ws |> List.map translateInstr)

    and translateInstr w =

        let binaryCommutativeOp lhs rhs op = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop(A); // RHS operand
                Pop(B); // LHS operand
                op;   // Result in A
                Push(A); 
                Barrier 
            ]

        let binaryOpWithConst lhs getOp =
            (translateInstr lhs) @ 
            [
                Pop(A);       // LHS operand
                getOp ();   // Result in A
                Push(A); 
                Barrier 
            ]

        let compareOp lhs rhs op = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop(A); // RHS operand
                Pop(B); // LHS operand
                op;   // Compare B (LHS) with A (RHS) and set boolean into A
                Push(A); 
                Barrier 
            ]

        let binaryNonCommutativeOp lhs rhs op = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop(A); // RHS operand
                Pop(B); // LHS operand
                op;   // Result in B
                Let(A,B);
                Push(A); 
                Barrier 
            ]

        let shiftOp lhs rhs op = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop(A);  // RHS operand
                Pop(B);  // LHS operand
                Let(C,A);
                op;    // Result in B
                Let(A,B);
                Push(A); 
                Barrier 
            ]

        let translateConstruct sourceBody putInOrder =
            let constructLabel = pushNewLabel ()
            let translatedBody = translateInstrList sourceBody
            popLabel ()
            putInOrder translatedBody [ Label(constructLabel) ]

        match w with

            | Unreachable -> 
                [ Breakpoint ; Barrier ]

            | Nop -> 
                []

            | Instr.Drop(ins) -> 
                translateInstr(ins) @ [ Drop ; Barrier ]
        
            | Select(a,b,c) -> 
                let l1 = newLabel ()
                let l2 = newLabel ()
                translateInstr(a) @ 
                translateInstr(b) @ 
                translateInstr(c) @ 
                [
                    Pop(A) ; 
                    Pop(B) ; // val2  (desired if A == 0)
                    Pop(C) ; // val1  (desired if A <> 0)
                    BranchAZ(l1) ;
                    Let(A,C) ;
                    Goto(l2) ;
                    Label(l1) ;
                    Let(A,B) ;
                    Label(l2) ;
                    Push(A) ;
                    Barrier
                ]
   
            | Block(_, body) -> 
                translateConstruct body (List.append) @ [Barrier]

            | Loop(_, body) -> 
                translateConstruct body (fun a b -> List.append (b @ [Barrier]) a)

            | If(_, body) -> 

                let skipLabel = pushNewLabel ()
                let translatedBody = translateInstrList body

                popLabel ()
                [ Pop(A) ; BranchAZ(skipLabel); ] 
                    @ translatedBody 
                    @ [ Label(skipLabel) ; Barrier ]
                    
            | IfElse(_, ifbody, elsebody) ->

                let skipIfLabel = pushNewLabel ()
                let translatedIfBody = translateInstrList ifbody
                popLabel ()

                let skipElseLabel = pushNewLabel ()
                let translatedElseBody = translateInstrList elsebody
                popLabel ()

                [ Pop(A) ; BranchAZ(skipIfLabel); ] 
                    @ translatedIfBody 
                    @ [ Goto(skipElseLabel) ; Label(skipIfLabel) ; Barrier; ] 
                    @ translatedElseBody 
                    @ [ Label(skipElseLabel) ; Barrier ]

            | Br(target) -> 
                [ Goto(labelFor target) ; Barrier ]

            | BrIf(cond,target) ->
                translateInstr(cond) @ [ Pop(A) ; BranchANZ(labelFor target) ; Barrier ]

            | BrTable(indexExpression, labelArray, defaultLabel) -> 
                let tableLabel = newLabel ()
                (translateInstr indexExpression)  @ 
                [
                    Pop(A) ; 
                    GotoIndex(tableLabel, labelArray.Length, labelFor defaultLabel, Array.map labelFor labelArray) ;
                    Barrier
                ]

            | Return -> 
                [ Goto(returnLabel) ; Barrier ]
            
            | Call(funcIdx, argsList) -> 
                let codeToPushArguments = argsList |> translateInstrList
                codeToPushArguments
                    @ [ CallFunc(FuncLabelFor funcIdx moduleFuncsArray) ]
                    @ (thunkInIfNeeded funcIdx) 
                    @ (pushAnyReturn funcIdx) 
                    @ [ Barrier ]

            | CallIndirect(funcType, argsList, indexExpr) ->
                // TODO: runtime validation of funcType, in order to be safe per-spec
                let codeToPushArguments = argsList |> translateInstrList
                let translatedIndex     = indexExpr |> translateInstr   // TODO: We could see if this would statically evaluate, and reduce this to a regular call, although the compiler would probably optimise that.
                codeToPushArguments 
                    @ translatedIndex 
                    @ [ Pop(A) ; CallTableIndirect ; Barrier ]

            | I32Const(I32(c)) -> 
                [ Const(A, Const32(c)) ; Push(A) ; Barrier ]

            | GetLocal(l) -> 
                [ FetchLoc(A,l) ; Push(A) ; Barrier ]

            | SetLocal(l,v) -> 
                (translateInstr v) @ [ Pop(A) ; StoreLoc(A,l) ; Barrier ]

            | TeeLocal(l,v) ->
                (translateInstr v) @ [ PeekA ; StoreLoc(A,l) ; Barrier ]
        
            | GetGlobal(g) ->
                [ FetchGlo(A,g) ; Push(A) ; Barrier ]

            | SetGlobal(g,v) ->
                (translateInstr v) @ [ Pop(A) ; StoreGlo(A,g) ; Barrier ]

                    // TODO: runtime restriction of addressing to the Linear Memory extent.
                    // (Wouldn't fit my application anyway, since it will not be possible
                    // to guarantee contiguous extension of the Linear Memory).

            | I32Store8(  {Align=_;       Offset=O}, I32Const(O2), I32Const(v)) -> [ StoreConst8( Y, O -+- O2,v); Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32(1u); Offset=O}, I32Const(O2), I32Const(v)) -> [ StoreConst16(Y, O -+- O2,v); Barrier ]
            | I32Store(   {Align=U32(2u); Offset=O}, I32Const(O2), I32Const(v)) -> [ StoreConst32(Y, O -+- O2,v); Barrier ]

            | I32Store8(  {Align=_;       Offset=O},          lhs, I32Const(v)) -> (translateInstr lhs) @ [ Pop(A) ; Add(A,Y) ; StoreConst8(A,O,v);  Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32(1u); Offset=O},          lhs, I32Const(v)) -> (translateInstr lhs) @ [ Pop(A) ; Add(A,Y) ; StoreConst16(A,O,v); Barrier ]
            | I32Store(   {Align=U32(2u); Offset=O},          lhs, I32Const(v)) -> (translateInstr lhs) @ [ Pop(A) ; Add(A,Y) ; StoreConst32(A,O,v); Barrier ]

            | I32Store8(  {Align=_;       Offset=O}, I32Const(O2),         rhs) -> (translateInstr rhs) @ [ Pop(A) ; Store8A( Y,O -+- O2) ; Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32(1u); Offset=O}, I32Const(O2),         rhs) -> (translateInstr rhs) @ [ Pop(A) ; Store16A(Y,O -+- O2) ; Barrier ]
            | I32Store(   {Align=U32(2u); Offset=O}, I32Const(O2),         rhs) -> (translateInstr rhs) @ [ Pop(A) ; Store32A(Y,O -+- O2) ; Barrier ]

            | I32Store8(  {Align=_;       Offset=O},          lhs,         rhs) -> (translateInstr lhs) @ (translateInstr rhs) @ [ Pop(A) ; Pop(B) ; Add(B,Y) ; Store8A(B,O)  ; Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32(1u); Offset=O},          lhs,         rhs) -> (translateInstr lhs) @ (translateInstr rhs) @ [ Pop(A) ; Pop(B) ; Add(B,Y) ; Store16A(B,O) ; Barrier ]
            | I32Store(   {Align=U32(2u); Offset=O},          lhs,         rhs) -> (translateInstr lhs) @ (translateInstr rhs) @ [ Pop(A) ; Pop(B) ; Add(B,Y) ; Store32A(B,O) ; Barrier ]

            | I32Store16( {Align=U32(_);  Offset=_},   _,   _) -> failwith "Cannot translate 16-bit store unless alignment is 2 bytes"
            | I32Store(   {Align=U32(_);  Offset=_},   _,   _) -> failwith "Cannot translate 32-bit store unless alignment is 4 bytes"

            | I32Load8s(  {Align=_;       Offset=O}, I32Const(O2)) -> [ Fetch8s( Y, O -+- O2) ; Push(A) ; Barrier ]
            | I32Load8u(  {Align=_;       Offset=O}, I32Const(O2)) -> [ Fetch8u( Y, O -+- O2) ; Push(A) ; Barrier ]
            | I32Load16s( {Align=U32(1u); Offset=O}, I32Const(O2)) -> [ Fetch16s(Y, O -+- O2) ; Push(A) ; Barrier ]
            | I32Load16u( {Align=U32(1u); Offset=O}, I32Const(O2)) -> [ Fetch16u(Y, O -+- O2) ; Push(A) ; Barrier ]
            | I32Load(    {Align=U32(2u); Offset=O}, I32Const(O2)) -> [ Fetch32( Y, O -+- O2) ; Push(A) ; Barrier ]

            | I32Load8s(  {Align=_;       Offset=O}, operand) -> (translateInstr operand) @ [ Pop(A) ; Add(A,Y) ; Fetch8s(A,O)  ; Push(A) ; Barrier ]
            | I32Load8u(  {Align=_;       Offset=O}, operand) -> (translateInstr operand) @ [ Pop(A) ; Add(A,Y) ; Fetch8u(A,O)  ; Push(A) ; Barrier ]
            | I32Load16s( {Align=U32(1u); Offset=O}, operand) -> (translateInstr operand) @ [ Pop(A) ; Add(A,Y) ; Fetch16s(A,O) ; Push(A) ; Barrier ]
            | I32Load16u( {Align=U32(1u); Offset=O}, operand) -> (translateInstr operand) @ [ Pop(A) ; Add(A,Y) ; Fetch16u(A,O) ; Push(A) ; Barrier ]
            | I32Load(    {Align=U32(2u); Offset=O}, operand) -> (translateInstr operand) @ [ Pop(A) ; Add(A,Y) ; Fetch32(A,O)  ; Push(A) ; Barrier ]
            
            // TODO: Could capitulate given X86 target, but make that configurable:
            | I32Load16s( {Align=U32(_); Offset=_}, _) -> failwith "Cannot translate 16-bit sign-extended load unless alignment is 2 bytes"
            | I32Load16u( {Align=U32(_); Offset=_}, _) -> failwith "Cannot translate 16-bit unsigned load unless alignment is 2 bytes"
            | I32Load(    {Align=U32(_); Offset=_}, _) -> failwith "Cannot translate 32-bit load unless alignment is 4 bytes"

            | I32Eqz(operand) -> (translateInstr operand) @ [ Pop(A) ; CmpAZ ; Push(A) ; Barrier ]

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

            | I32Add (a,b) -> binaryCommutativeOp     a b (Add(A,B))
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

            | _ -> failwith (sprintf "Cannot translate this instruction to simple 32-bit machine: %A" w)   // TODO: Possibly avoid %A

    // Do the translation with the above nested functions:

    let finalTranslation = (translateInstrList ws) @ [ Label(returnLabel) ] 
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

let LocalIdxNameString localIdx = 
    sprintf "%s%d" AsmLocalNamePrefix (LocalIdxAsUint32 localIdx)

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



let TranslateThunkIn () =
    // The translated code requires the Y register to
    // point to the base of the linear memory region.
    // Note: There is only *one* linear memory supported in WASM 1.0  (mem #0)
    [ sprintf "let Y=%s%d" AsmMemPrefix 0 ]
    

let TranslateGotoIndex tableLabel numMax defaultLabel =
    [
        // A is already the index to branch to
        sprintf "cmp A,%d:if >>= goto %s" numMax (LabelTextOf defaultLabel) ;
        "shl A,logptr" ;
        sprintf "goto [A+%s]" (LabelTextOf tableLabel)
    ]


let TranslateCallTableIndirect () =
    // TODO: We really need to emit some code to validate the signatures.
    // A is already the index to call.
    // TODO: We need to validate index A lies within wasm table [0]
    [
        "shl A,logptr" ;
        (sprintf "goto [A+%s0]" AsmTableNamePrefix)  // WASM 1.0 always looks in table #0
    ]



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

    TranslateThunkIn () |> List.iter writeOutIns
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



let WriteOutWasmMem writeOutData writeOutVar i (thisMem:InternalMemoryRecord) =

    let linearMemorySize = 

        match thisMem with 
            | { MemoryType={ MemoryLimits=lims } } -> 

            match lims with 

                | { LimitMin = U32(0u); LimitMax = None; }      
                    -> failwith "Cannot translate module with Mem that is size 0"   

                | { LimitMin = U32(memSize); LimitMax = None; } 
                    -> memSize * WasmMemoryBlockMultiplier

                | { LimitMin = _; LimitMax = Some(_); }
                    -> failwith "Cannot translate module with Mem that has max size limit"

    writeOutVar "global"
    writeOutVar "    align ptr"
    writeOutVar (sprintf "    %s%d: %d" AsmMemPrefix i linearMemorySize)

    let writeIns s = writeOutData ("    " + s)

    writeOutData (sprintf "// Data for WASM mem %s%d" AsmMemoryNamePrefix i) // TODO: If there is none, omit this.

    thisMem.InitData |> Array.iteri (fun j elem ->
            let _, byteArray = elem
            writeOutData (sprintf "data %s%d_%d" AsmMemoryNamePrefix i j)
            WriteOutHexDump writeIns byteArray
        )




let WriteOutWasmGlobal writeOut i (m:Module) (g:InternalGlobalRecord) =

    // TODO: We do nothing with the immutability information.  Could we avoid a store and hoist the constant into the code?

    let initValue = StaticEvaluate g.InitExpr
    let globalIdx = GlobalIdx(U32(uint32 i))   // TODO: not ideal construction of temporary

    writeOut (sprintf "data %s int %d" (GlobalIdxNameString globalIdx) initValue)



let OfsIfNeeded u = 
    match u with
        | U32(0u) -> ""   // indexed addressing not needed
        | U32(n)  -> ("+" + n.ToString())   // indexed addressing


let TranslateREGU32 s1 r u s2 = 
    [ sprintf "%s%s%s%s" s1 (RegNameOf r) (OfsIfNeeded u) s2 ]


let TranslateREGU32I32 s1 r u s2 n = 
    [ sprintf "%s%s%s%s%d" s1 (RegNameOf r) (OfsIfNeeded u) s2 n ]



let TranslateInstructionToAsmSequence subTree =

    // These translations can assume a 32-bit target for now.

    match subTree with
        | Barrier               -> [ "// ~~~ register barrier ~~~" ]
        | Breakpoint            -> [ "break" ]
        | Drop                  -> [ "add SP,4" ]  // TODO: Assumes 32-bit target
        | Label(l)              -> [ "label " + (LabelTextOf l) ]   // TODO: sort out ASM local label references
        | Const(r,Const32(n))   -> [ sprintf "let %s=%d" (RegNameOf r) n ]
        | Goto(l)               -> [ "goto " + LabelTextOf l ]
        | CallFunc(l)           -> [ "call " + FuncNameOf l ]
        | CallTableIndirect     -> TranslateCallTableIndirect ()
        | BranchAZ(l)           -> [ "cmp A,0:if z goto " + LabelTextOf l ]
        | BranchANZ(l)          -> [ "cmp A,0:if nz goto " + LabelTextOf l ]
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
        | ThunkIn              -> TranslateThunkIn ()




let WriteOutInstructionsToText writeOut instrs thisFuncType =

    let writeIns s = writeOut ("    " + s)

    // Kick off the whole thing here:

    instrs |> List.iter (fun i -> TranslateInstructionToAsmSequence i |> List.iter writeIns)

    // Handle the function's return (may need pop into A):

    match thisFuncType |> ReturnsSingleValue with
        | true  -> writeIns "pop A"
        | false -> ()

    


let WriteOutBranchTables writeOut funcInstructions =

    let writeIns s = writeOut ("    " + s)

    funcInstructions |> List.iter (fun ins ->
        match ins with
            | GotoIndex(tableLabel,_,_,codePointLabels) ->
                writeOut "align ptr"
                writeOut (sprintf "data %s" (LabelTextOf tableLabel))
                codePointLabels |> Array.iter (fun lbl -> writeIns (sprintf "ptr %s" (LabelTextOf lbl)))
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

    let procedureCommand = (sprintf "procedure %s%d%s" AsmInternalFuncNamePrefix funcIndex (AsmSignatureOf f.FuncType))

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
    writeOut (sprintf "goto %s" (match (FuncLabelFor startFuncIdx moduleFuncsArray) with LabelName(s) -> s))



let WriteOutWasmStart writeOut startOption moduleFuncsArray =
    match startOption with 
        | Some({StartFuncIdx=startFuncIdx}) -> 
            WriteOutBranchToEntryLabel writeOut startFuncIdx moduleFuncsArray
        | None -> writeOut "// No entry point in this translation"



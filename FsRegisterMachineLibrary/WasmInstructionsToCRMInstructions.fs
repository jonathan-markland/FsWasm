module WasmInstructionsToCRMInstructions

open System.Text
open WasmFileTypes
open WasmBetterTypes
open CommonRegisterMachineTypes
open AsmPrefixes



let (-+-) (U32 a) (I32 b) =
    U32 (a + uint32 b)



let FuncLabelFor func =
    match func with
        | ImportedFunction2({Import={ImportModuleName=m; ImportName=n}}) -> 
            LabelName(AsmInternalFuncNamePrefix + m + "_" + n)
        | InternalFunction2({ModuleLocalFuncIdx=(FuncIdx (U32 i))}) ->
            LabelName(AsmInternalFuncNamePrefix + i.ToString()) 



type ModuleTranslationState =
    | ModuleTranslationState of labelCount:int



type WasmToCrmTranslationConfig =
    {
        /// Should a stack pointer adjustment Drop be generated
        /// after a call instruction, in order to clear off the
        /// parameters.
        ClearParametersAfterCall : bool
    }



/// Translate WASM instruction body tree to Common Register Machine (CRM) list.
let TranslateInstructions (moduleFuncsArray:Function[]) translationState wasmToCrmTranslationConfig (ws:WasmFileTypes.Instr list) =

    let mutable (ModuleTranslationState labelCount) = translationState
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

    let labelFor (LabelIdx (U32 i)) =
        labelStack.[labelStack.Count - (1 + (int i))]

    let returnLabel = newLabel ()

    let getFunc (FuncIdx (U32 i)) =
        moduleFuncsArray.[int i]

    let getFuncType (FuncIdx (U32 i)) =
        match moduleFuncsArray.[int i] with
            | ImportedFunction2(f) -> f.FuncType
            | InternalFunction2(f) -> f.FuncType

    let thunkInIfNeeded (FuncIdx (U32 i)) = 
        match moduleFuncsArray.[int i] with
            | ImportedFunction2(_) -> [ ThunkIn ]   // Since we called an imported function, we reload Y on return.  TODO: This would be overzealous if the import is something *we* translated!
            | InternalFunction2(_) -> []
 
    let stackCleanupAfterCall funcType =
        if wasmToCrmTranslationConfig.ClearParametersAfterCall then
            let numParamsToRemove = uint32 funcType.ParameterTypes.Length
            [ Drop (U32 numParamsToRemove) ]
        else
            []

    let pushAnyReturn funcType =
        match funcType.ReturnTypes.Length with
            | 0 -> []
            | 1 -> [ Push A ]
            | _ -> failwith "Cannot translate functions which return more than 1 result"

    let rec translateInstrList ws =

        List.concat (ws |> List.map translateInstr)

    and translateInstr w =

        let binaryCommutativeOp lhs rhs op = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop A       // RHS operand
                Pop B       // LHS operand
                op          // Result in A
                Push A 
                Barrier 
            ]

        let binaryOpWithConst lhs getOp =
            (translateInstr lhs) @ 
            [
                Pop A       // LHS operand
                getOp ()    // Result in A
                Push A 
                Barrier 
            ]

        let compareOp lhs rhs op = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop A       // RHS operand
                Pop B       // LHS operand
                op          // Compare B (LHS) with A (RHS) and set boolean into A
                Push A 
                Barrier 
            ]

        let binaryNonCommutativeOp lhs rhs op = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop A       // RHS operand
                Pop B       // LHS operand
                op          // Result in B
                Let (A,B)
                Push A
                Barrier 
            ]

        let shiftOp lhs rhs op = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop A       // RHS operand
                Pop B       // LHS operand
                Let (C,A)
                op          // Result in B
                Let (A,B)
                Push A
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
                translateInstr(ins) @ [ Drop (U32 1u) ; Barrier ]
        
            | Select(a,b,c) -> 
                let l1 = newLabel ()
                let l2 = newLabel ()
                translateInstr(a) @ 
                translateInstr(b) @ 
                translateInstr(c) @ 
                [
                    Pop A  
                    Pop B  // val2  (desired if A == 0)
                    Pop C  // val1  (desired if A <> 0)
                    BranchRegZNZ(A,BZero,l1)
                    Let (A,C)
                    Goto l2
                    Label l1
                    Let (A,B)
                    Label l2
                    Push A
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
                [ Pop A ; BranchRegZNZ(A,BZero,skipLabel) ] 
                    @ translatedBody 
                    @ [ Label skipLabel ; Barrier ]
                    
            | IfElse(_, ifbody, elsebody) ->

                let skipIfLabel = pushNewLabel ()
                let translatedIfBody = translateInstrList ifbody
                popLabel ()

                let skipElseLabel = pushNewLabel ()
                let translatedElseBody = translateInstrList elsebody
                popLabel ()

                [ Pop A ; BranchRegZNZ(A,BZero,skipIfLabel) ] 
                    @ translatedIfBody 
                    @ [ Goto skipElseLabel ; Label skipIfLabel ; Barrier ] 
                    @ translatedElseBody 
                    @ [ Label skipElseLabel ; Barrier ]

            | Br(target) -> 
                [ Goto (labelFor target) ; Barrier ]

            | BrIf(cond,target) ->
                translateInstr(cond) @ [ Pop A ; BranchRegZNZ(A,BNonZero,(labelFor target)) ; Barrier ]

            | BrTable(indexExpression, labelArray, defaultLabel) -> 
                let tableLabel = newLabel ()
                (translateInstr indexExpression)  @ 
                [
                    Pop A
                    GotoIndex (tableLabel, labelArray.Length, labelFor defaultLabel, Array.map labelFor labelArray)
                    Barrier
                ]

            | Return -> 
                [ Goto returnLabel ; Barrier ]
            
            | Call(funcIdx, argsList) -> 
                let codeToPushArguments = argsList |> translateInstrList
                let funcType = getFuncType funcIdx
                codeToPushArguments
                    @ [ CallFunc(FuncLabelFor (getFunc funcIdx)) ]
                    @ (thunkInIfNeeded funcIdx) 
                    @ (stackCleanupAfterCall funcType)
                    @ (pushAnyReturn funcType) 
                    @ [ Barrier ]

            | CallIndirect(funcType, argsList, indexExpr) ->
                // TODO: runtime validation of funcType, in order to be safe per-spec
                let codeToPushArguments = argsList |> translateInstrList
                let translatedIndex     = indexExpr |> translateInstr   // TODO: We could see if this would statically evaluate, and reduce this to a regular call, although the compiler would probably optimise that.
                codeToPushArguments 
                    @ translatedIndex 
                    @ [ Pop A ; CallTableIndirect ]
                    // TODO: Do we ever need to do this? I suppose it depends on what the table points to!   @ (thunkInIfNeeded funcIdx) 
                    @ (stackCleanupAfterCall funcType)
                    @ (pushAnyReturn funcType)
                    @ [ Barrier ]

            | I32Const(I32(c)) -> 
                [ Const (A, Const32 c) ; Push A ; Barrier ]

            | GetLocal(l) -> 
                [ FetchLoc(A,l) ; Push A ; Barrier ]

            | SetLocal(l,v) -> 
                (translateInstr v) @ [ Pop A ; StoreLoc(A,l) ; Barrier ]

            | TeeLocal(l,v) ->
                (translateInstr v) @ [ PeekA ; StoreLoc(A,l) ; Barrier ]
        
            | GetGlobal(g) ->
                [ FetchGlo(A,g) ; Push A ; Barrier ]

            | SetGlobal(g,v) ->
                (translateInstr v) @ [ Pop A ; StoreGlo(A,g) ; Barrier ]

                    // TODO: runtime restriction of addressing to the Linear Memory extent.
                    // (Wouldn't fit my application anyway, since it will not be possible
                    // to guarantee contiguous extension of the Linear Memory).

            | I32Store8(  {Align=_;       Offset=O}, I32Const O2, I32Const v) -> [ StoreConst(Stored8, Y, O -+- O2,v); Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32 1u ; Offset=O}, I32Const O2, I32Const v) -> [ StoreConst(Stored16, Y, O -+- O2,v); Barrier ]
            | I32Store(   {Align=U32 2u ; Offset=O}, I32Const O2, I32Const v) -> [ StoreConst(Stored32, Y, O -+- O2,v); Barrier ]

            | I32Store8(  {Align=_;       Offset=O},         lhs, I32Const v) -> (translateInstr lhs) @ [ Pop A ; CalcRegReg(AddRbRa,A,Y) ; StoreConst(Stored8, A,O,v);  Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32 1u ; Offset=O},         lhs, I32Const v) -> (translateInstr lhs) @ [ Pop A ; CalcRegReg(AddRbRa,A,Y) ; StoreConst(Stored16, A,O,v); Barrier ]
            | I32Store(   {Align=U32 2u ; Offset=O},         lhs, I32Const v) -> (translateInstr lhs) @ [ Pop A ; CalcRegReg(AddRbRa,A,Y) ; StoreConst(Stored32, A,O,v); Barrier ]

            | I32Store8(  {Align=_;       Offset=O}, I32Const O2,        rhs) -> (translateInstr rhs) @ [ Pop A ; Store(A, Stored8, Y,O -+- O2) ; Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32 1u ; Offset=O}, I32Const O2,        rhs) -> (translateInstr rhs) @ [ Pop A ; Store(A, Stored16, Y,O -+- O2) ; Barrier ]
            | I32Store(   {Align=U32 2u ; Offset=O}, I32Const O2,        rhs) -> (translateInstr rhs) @ [ Pop A ; Store(A, Stored32, Y,O -+- O2) ; Barrier ]

            | I32Store8(  {Align=_;       Offset=O},         lhs,        rhs) -> (translateInstr lhs) @ (translateInstr rhs) @ [ Pop A ; Pop B ; CalcRegReg(AddRbRa,B,Y) ; Store(A, Stored8, B,O)  ; Barrier ]   // TODO: separate routines!!
            | I32Store16( {Align=U32 1u ; Offset=O},         lhs,        rhs) -> (translateInstr lhs) @ (translateInstr rhs) @ [ Pop A ; Pop B ; CalcRegReg(AddRbRa,B,Y) ; Store(A, Stored16, B,O) ; Barrier ]
            | I32Store(   {Align=U32 2u ; Offset=O},         lhs,        rhs) -> (translateInstr lhs) @ (translateInstr rhs) @ [ Pop A ; Pop B ; CalcRegReg(AddRbRa,B,Y) ; Store(A, Stored32, B,O) ; Barrier ]

            | I32Store16( {Align=U32 _ ;  Offset=_},   _,   _) -> failwith "Cannot translate 16-bit store unless alignment is 2 bytes"
            | I32Store(   {Align=U32 _ ;  Offset=_},   _,   _) -> failwith "Cannot translate 32-bit store unless alignment is 4 bytes"

            | I32Load8s(  {Align=_;       Offset=O}, I32Const O2) -> [ Fetch(A, SignExt8,  Y, O -+- O2) ; Push A ; Barrier ]
            | I32Load8u(  {Align=_;       Offset=O}, I32Const O2) -> [ Fetch(A, ZeroExt8,  Y, O -+- O2) ; Push A ; Barrier ]
            | I32Load16s( {Align=U32 1u ; Offset=O}, I32Const O2) -> [ Fetch(A, SignExt16, Y, O -+- O2) ; Push A ; Barrier ]
            | I32Load16u( {Align=U32 1u ; Offset=O}, I32Const O2) -> [ Fetch(A, ZeroExt16, Y, O -+- O2) ; Push A ; Barrier ]
            | I32Load(    {Align=U32 2u ; Offset=O}, I32Const O2) -> [ Fetch(A, SignExt32, Y, O -+- O2) ; Push A ; Barrier ]

            | I32Load8s(  {Align=_;       Offset=O}, operand) -> (translateInstr operand) @ [ Pop A ; CalcRegReg(AddRbRa,A,Y) ; Fetch(A, SignExt8,  A,O) ; Push A ; Barrier ]
            | I32Load8u(  {Align=_;       Offset=O}, operand) -> (translateInstr operand) @ [ Pop A ; CalcRegReg(AddRbRa,A,Y) ; Fetch(A, ZeroExt8,  A,O) ; Push A ; Barrier ]
            | I32Load16s( {Align=U32 1u ; Offset=O}, operand) -> (translateInstr operand) @ [ Pop A ; CalcRegReg(AddRbRa,A,Y) ; Fetch(A, SignExt16, A,O) ; Push A ; Barrier ]
            | I32Load16u( {Align=U32 1u ; Offset=O}, operand) -> (translateInstr operand) @ [ Pop A ; CalcRegReg(AddRbRa,A,Y) ; Fetch(A, ZeroExt16, A,O) ; Push A ; Barrier ]
            | I32Load(    {Align=U32 2u ; Offset=O}, operand) -> (translateInstr operand) @ [ Pop A ; CalcRegReg(AddRbRa,A,Y) ; Fetch(A, SignExt32, A,O) ; Push A ; Barrier ]
           
            // TODO: Could capitulate given X86 target, but make that configurable:
            | I32Load16s( {Align=U32 _ ; Offset=_}, _) -> failwith "Cannot translate 16-bit sign-extended load unless alignment is 2 bytes"
            | I32Load16u( {Align=U32 _ ; Offset=_}, _) -> failwith "Cannot translate 16-bit unsigned load unless alignment is 2 bytes"
            | I32Load(    {Align=U32 _ ; Offset=_}, _) -> failwith "Cannot translate 32-bit load unless alignment is 4 bytes"

            | I32Eqz(operand) -> (translateInstr operand) @ [ Pop A ; CmpAZ ; Push A ; Barrier ]

            | I32Eq(a,b)   -> compareOp a b (CmpBA CrmCondEq )
            | I32Ne(a,b)   -> compareOp a b (CmpBA CrmCondNe )
            | I32Lts(a,b)  -> compareOp a b (CmpBA CrmCondLts)
            | I32Ltu(a,b)  -> compareOp a b (CmpBA CrmCondLtu)
            | I32Gts(a,b)  -> compareOp a b (CmpBA CrmCondGts)
            | I32Gtu(a,b)  -> compareOp a b (CmpBA CrmCondGtu)
            | I32Les(a,b)  -> compareOp a b (CmpBA CrmCondLes)
            | I32Leu(a,b)  -> compareOp a b (CmpBA CrmCondLeu)
            | I32Ges(a,b)  -> compareOp a b (CmpBA CrmCondGes)
            | I32Geu(a,b)  -> compareOp a b (CmpBA CrmCondGeu)

            | I32Add (a,I32Const n) -> binaryOpWithConst a (fun () -> CalcWithConst(AddRN,A,n))
            | I32Sub (a,I32Const n) -> binaryOpWithConst a (fun () -> CalcWithConst(SubRN,A,n))
            | I32And (a,I32Const n) -> binaryOpWithConst a (fun () -> CalcWithConst(AndRN,A,n))
            | I32Or  (a,I32Const n) -> binaryOpWithConst a (fun () -> CalcWithConst(OrRN,A,n))
            | I32Xor (a,I32Const n) -> binaryOpWithConst a (fun () -> CalcWithConst(XorRN,A,n))

            | I32Add (a,b) -> binaryCommutativeOp     a b (CalcRegReg (AddRbRa,A,B))
            | I32Sub (a,b) -> binaryNonCommutativeOp  a b (CalcRegReg (SubRbRa,B,A))
            | I32Mul (a,b) -> binaryCommutativeOp     a b (CalcRegReg (MulRbRa,A,B)) 
            | I32Divs(a,b) -> binaryNonCommutativeOp  a b (CalcRegReg (DivsRbRa,A,B))
            | I32Divu(a,b) -> binaryNonCommutativeOp  a b (CalcRegReg (DivuRbRa,A,B))
            | I32Rems(a,b) -> binaryNonCommutativeOp  a b (CalcRegReg (RemsRbRa,A,B))
            | I32Remu(a,b) -> binaryNonCommutativeOp  a b (CalcRegReg (RemuRbRa,A,B))
            | I32And (a,b) -> binaryCommutativeOp     a b (CalcRegReg (AndRbRa,A,B)) 
            | I32Or  (a,b) -> binaryCommutativeOp     a b (CalcRegReg (OrRbRa,A,B))  
            | I32Xor (a,b) -> binaryCommutativeOp     a b (CalcRegReg (XorRbRa,A,B)) 
            
            | I32Shl (a,b) -> shiftOp a b (ShiftRot Shl) 
            | I32Shrs(a,b) -> shiftOp a b (ShiftRot Shrs)
            | I32Shru(a,b) -> shiftOp a b (ShiftRot Shru)
            | I32Rotl(a,b) -> shiftOp a b (ShiftRot Rotl)
            | I32Rotr(a,b) -> shiftOp a b (ShiftRot Rotr)

            | _ -> failwith (sprintf "Cannot translate this instruction to simple 32-bit machine: %A" w)   // TODO: Possibly avoid %A

    // Do the translation with the above nested functions:

    let finalTranslation = (translateInstrList ws) @ [ Label returnLabel ] 
    let updatedTranslationState = ModuleTranslationState(labelCount)

    (finalTranslation, updatedTranslationState)

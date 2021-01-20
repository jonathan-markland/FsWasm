module WasmInstructionsToCRMInstructions

open System.Text
open WasmFileTypes
open WasmBetterTypes
open CommonRegisterMachineTypes
open AsmPrefixes



/// Addition operation that performs type conversion for convenience.
let (-+-) (U32 a) (I32 b) =
    U32 (a + uint32 b)



/// Return the assembly language function label name for the given Function.
let FuncLabelFor func =
    match func with

        | ImportedFunction2({Import={ImportModuleName=m; ImportName=n}}) -> 
            LabelName(AsmInternalFuncNamePrefix + m + "_" + n)

        | InternalFunction2({ModuleLocalFuncIdx=(FuncIdx (U32 i))}) ->
            LabelName(AsmInternalFuncNamePrefix + i.ToString()) 



type ModuleTranslationState =
    | ModuleTranslationState of labelCount:int



type ShiftGenerationStrategy = 

    /// (X86 but would work on ARM)
    | RuntimeShiftCountMustBeInRegC 
    
    /// (ARM preferred)
    | ShiftCountInAnyRegister



type NonCommutativeOpStrategy =

    /// (X86 but would work on ARM)
    | NonCommutativeOnTwoRegisterMachine

    /// (ARM preferred)
    | NonCommutativeOnThreeRegisterMachine



type WasmToCrmTranslationConfig =
    {
        /// Should a stack pointer adjustment Drop be generated
        /// after a call instruction, in order to clear off the
        /// parameters.
        ClearParametersAfterCall : bool

        /// How shift instructions (and companions) are generated
        ShiftStrategy : ShiftGenerationStrategy

        /// How subtract (and others) are generated
        NonCommutativeOpStrategy : NonCommutativeOpStrategy
    }



let EndsWithBarrier crmInstructions =
    match crmInstructions with
        | [] -> false
        | _  -> 
            match crmInstructions |> List.last with
                | Barrier _ -> true
                | _ -> false



/// Translate WASM instruction body tree to Common Register Machine (CRM) list.
let TranslateInstructions (moduleFuncsArray:Function[]) translationState wasmToCrmTranslationConfig (ws:WasmFileTypes.Instr list) =

    let mutable (ModuleTranslationState labelAllocatorCount) = translationState
    let mutable labelStack = []

    let newLabel () =
        labelAllocatorCount <- labelAllocatorCount + 1
        LabelName (sprintf "%s%d" AsmCodeLabelPrefix labelAllocatorCount)

    let pushNewLabel () =
        let l = newLabel ()
        labelStack <- l :: labelStack
        l

    let popLabel () =
        labelStack <-
            match labelStack with
                | [] -> failwith "Internal error:  Too many labels popped from internal label stack!"
                | _::tail -> tail

    let labelFor (LabelIdx (U32 i)) =

        let rec recurse lst i =
            match  i , lst with
                |  _ , []      -> failwith "Internal error:  Insufficient labels for reverse-indexing attempt."
                | 0u , head::_ -> head
                |  n , _::tail -> recurse tail (n-1u)

        recurse labelStack i

    let returnLabel = 
        newLabel ()

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

    and translateInstr (w:Instr) =

        let translatedList = translateInstrWithoutBarrier w

        // Avoid unnecessary repeats when we return back out of two or more nested levels in one go:
        if translatedList |> EndsWithBarrier then 
            translatedList 
        else 
            translatedList @ [ Barrier (Some w) ]

    and translateInstrWithoutBarrier (w:Instr) =

        let binaryCommutativeOp lhs rhs op = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop A                  // RHS operand
                Pop B                  // LHS operand
                CalcRegs (op,A,B, A)   // Result in A
                Push A 
            ]

        let binaryOpWithConst lhs operation n =
            (translateInstr lhs) @ 
            [
                Pop A                      // LHS operand
                CalcRegNum (operation,A,n) // Result in A
                Push A 
            ]

        let compareOp lhs rhs cond = 
            (translateInstr lhs) @ 
            (translateInstr rhs) @ 
            [
                Pop A       // RHS operand
                Pop B       // LHS operand
                CmpBA cond  // Compare B (LHS) with A (RHS) and set boolean into A
                Push A 
            ]

        let binaryNonCommutativeOp lhs rhs op = 

            let operatorSequence =
                match wasmToCrmTranslationConfig.NonCommutativeOpStrategy with
                    | NonCommutativeOnTwoRegisterMachine ->
                        [
                            Pop A       // RHS operand
                            Pop B       // LHS operand
                            CalcRegs (SubRegReg,B,A, B)          // Result in B
                            Let (A,B)   // TODO: Favoured because "push A - barrier- pop A" is removed by peephole, but "push B - barrier - pop A" isn't yet.
                            Push A
                        ]

                    | NonCommutativeOnThreeRegisterMachine ->
                        [
                            Pop A       // RHS operand
                            Pop B       // LHS operand
                            CalcRegs (SubRegReg,B,A, A)        // Result in A
                            Push A
                        ]

            (translateInstr lhs) @ (translateInstr rhs) @ operatorSequence

        let shiftOp lhs rhs shiftRotateType =   // TODO: Use config to activate a much better version for the ARM

            let shiftSequence =
                match wasmToCrmTranslationConfig.ShiftStrategy with
                    | RuntimeShiftCountMustBeInRegC ->
                        [
                            Pop A       // RHS operand
                            Pop B       // LHS operand
                            Let (C,A)
                            ShiftRot (shiftRotateType, B,C, B) // Result in B
                            Let (A,B)
                            Push A
                        ]
                        
                    | ShiftCountInAnyRegister ->
                        [
                            Pop A       // RHS operand
                            Pop B       // LHS operand
                            ShiftRot (shiftRotateType, B,A, A)  // Result in B
                            Push A
                        ]

            (translateInstr lhs) @ (translateInstr rhs) @ shiftSequence

        let translateConstruct sourceBody putInOrder =

            let constructLabel = pushNewLabel ()
            let translatedBody = translateInstrList sourceBody
            popLabel ()
            putInOrder translatedBody [ Label constructLabel ]

        let fetchInstruction fetchedType {Align=al ; Offset=O} adr =

            match (fetchedType , al) with
                | (SignExt8  , _)     
                | (ZeroExt8  , _)     
                | (SignExt16 , U32 1u)
                | (ZeroExt16 , U32 1u)
                | (SignExt32 , U32 2u) -> ()
                | _ -> failwith "Cannot translate fetch with given alignment"

            match adr with
                
                | I32Const O2 -> 
                    [ 
                        Fetch (A, fetchedType, Y, O -+- O2)
                        Push A 
                    ]

                | _ ->
                    (translateInstr adr) @ 
                    [ 
                        Pop A
                        CalcRegs (AddRegReg,A,Y, A)
                        Fetch (A, fetchedType, A,O)    // TODO: Could we extend Fetch() to have two registers, one optional?  Then avoid the addition and use Rn+Rm addressing mode instead.  (Should make that generation configurable).
                        Push A
                    ]

        let storeInstruction storedType {Align=al ; Offset=O} lhs rhs =

            match (storedType , al) with
                | (Stored8,  _)
                | (Stored16, U32 1u)
                | (Stored32, U32 2u) -> ()
                | _ -> failwith "Cannot translate store with given alignment"

            match lhs,rhs with
                
                | I32Const O2, I32Const v -> 
                    [ StoreConst (storedType, Y, O -+- O2,v) ]

                | _, I32Const v -> 
                    (translateInstr lhs) @ 
                    [ 
                        Pop A
                        CalcRegs (AddRegReg,A,Y,A)      // TODO: More effective use of addressing.
                        StoreConst (storedType,A,O,v) 
                    ]

                | I32Const O2, _ -> 
                    (translateInstr rhs) @ 
                    [ 
                        Pop A
                        Store (A, storedType, Y,O -+- O2)
                    ]

                | _,_ -> 
                    (translateInstr lhs) @ 
                    (translateInstr rhs) @ 
                    [ 
                        Pop A
                        Pop B
                        CalcRegs (AddRegReg,B,Y,B)
                        Store (A, storedType, B,O)
                    ]


        match w with

            | Unreachable -> 
                [ Breakpoint ]

            | Nop -> 
                []

            | Instr.Drop(ins) -> 
                translateInstr(ins) @ [ Drop (U32 1u) ]
        
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
                ]
   
            | Block(_, body) -> 
                translateConstruct body (List.append)

            | Loop(_, body) -> 
                translateConstruct body (fun a b -> List.append b a)

            | If(_, body) -> 

                let skipLabel = pushNewLabel ()
                let translatedBody = translateInstrList body

                popLabel ()
                [ Pop A ; BranchRegZNZ(A,BZero,skipLabel) ] 
                    @ translatedBody 
                    @ [ Label skipLabel ]
                    
            | IfElse(_, ifbody, elsebody) ->

                let skipIfLabel = pushNewLabel ()
                let translatedIfBody = translateInstrList ifbody
                popLabel ()

                let skipElseLabel = pushNewLabel ()
                let translatedElseBody = translateInstrList elsebody
                popLabel ()

                [ Pop A ; BranchRegZNZ(A,BZero,skipIfLabel) ] 
                    @ translatedIfBody 
                    @ [ Goto skipElseLabel ; Label skipIfLabel ] 
                    @ translatedElseBody 
                    @ [ Label skipElseLabel ]

            | Br(target) -> 
                [ Goto (labelFor target) ]

            | BrIf(cond,target) ->
                translateInstr(cond) @ 
                [ 
                    Pop A
                    BranchRegZNZ(A,BNonZero,(labelFor target)) 
                ]

            | BrTable(indexExpression, labelArray, defaultLabel) -> 
                let tableLabel = newLabel ()
                (translateInstr indexExpression)  @ 
                [
                    Pop A
                    GotoIndex (tableLabel, labelArray.Length, labelFor defaultLabel, Array.map labelFor labelArray)
                ]

            | Return -> 
                [ Goto returnLabel ]
            
            | Call(funcIdx, argsList) -> 
                let codeToPushArguments = argsList |> translateInstrList
                let funcType = getFuncType funcIdx
                codeToPushArguments
                    @ [ CallFunc(FuncLabelFor (getFunc funcIdx)) ]
                    @ (thunkInIfNeeded funcIdx) 
                    @ (stackCleanupAfterCall funcType)
                    @ (pushAnyReturn funcType) 

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

            | I32Const (I32(c)) -> [ Const (A, Const32 c) ; Push A ]
            
            | GetLocal (l)      -> [ FetchLoc (A,l) ; Push A ]
            | SetLocal (l,v)    -> (translateInstr v) @ [ Pop A ; StoreLoc (A,l) ]
            | TeeLocal (l,v)    -> (translateInstr v) @ [ PeekA ; StoreLoc (A,l) ]
            
            | GetGlobal (g)     -> [ FetchGlo (A,g) ; Push A ]
            | SetGlobal (g,v)   -> (translateInstr v) @ [ Pop A ; StoreGlo (A,g) ]

            // TODO: runtime restriction of addressing to the Linear Memory extent.

            | I32Store8  (memArg, lhs, rhs) -> storeInstruction Stored8  memArg lhs rhs
            | I32Store16 (memArg, lhs, rhs) -> storeInstruction Stored16 memArg lhs rhs
            | I32Store   (memArg, lhs, rhs) -> storeInstruction Stored32 memArg lhs rhs

            | I32Load8s  (memArg, adr) -> fetchInstruction SignExt8  memArg adr
            | I32Load8u  (memArg, adr) -> fetchInstruction ZeroExt8  memArg adr
            | I32Load16s (memArg, adr) -> fetchInstruction SignExt16 memArg adr
            | I32Load16u (memArg, adr) -> fetchInstruction ZeroExt16 memArg adr
            | I32Load    (memArg, adr) -> fetchInstruction SignExt32 memArg adr

            | I32Eqz operand -> (translateInstr operand) @ [ Pop A ; CmpAZ ; Push A ]

            | I32Eq  (a,b)  -> compareOp a b CrmCondEq 
            | I32Ne  (a,b)  -> compareOp a b CrmCondNe 
            | I32Lts (a,b)  -> compareOp a b CrmCondLts
            | I32Ltu (a,b)  -> compareOp a b CrmCondLtu
            | I32Gts (a,b)  -> compareOp a b CrmCondGts
            | I32Gtu (a,b)  -> compareOp a b CrmCondGtu
            | I32Les (a,b)  -> compareOp a b CrmCondLes
            | I32Leu (a,b)  -> compareOp a b CrmCondLeu
            | I32Ges (a,b)  -> compareOp a b CrmCondGes
            | I32Geu (a,b)  -> compareOp a b CrmCondGeu

            | I32Add  (a,I32Const n) -> binaryOpWithConst a AddRegNum n
            | I32Sub  (a,I32Const n) -> binaryOpWithConst a SubRegNum n
            | I32And  (a,I32Const n) -> binaryOpWithConst a AndRegNum n
            | I32Or   (a,I32Const n) -> binaryOpWithConst a OrRegNum  n
            | I32Xor  (a,I32Const n) -> binaryOpWithConst a XorRegNum n

            | I32Add  (a,b) -> binaryCommutativeOp     a b AddRegReg
            | I32Sub  (a,b) -> binaryNonCommutativeOp  a b SubRegReg
            | I32Mul  (a,b) -> binaryCommutativeOp     a b MulRegReg 
            | I32Divs (a,b) -> failwith "Division and remainder not supported yet"  // binaryNonCommutativeOp  a b (CalcRegReg (DivsRegReg,A,B))
            | I32Divu (a,b) -> failwith "Division and remainder not supported yet"  // binaryNonCommutativeOp  a b (CalcRegReg (DivuRegReg,A,B))
            | I32Rems (a,b) -> failwith "Division and remainder not supported yet"  // binaryNonCommutativeOp  a b (CalcRegReg (RemsRegReg,A,B))
            | I32Remu (a,b) -> failwith "Division and remainder not supported yet"  // binaryNonCommutativeOp  a b (CalcRegReg (RemuRegReg,A,B))
            | I32And  (a,b) -> binaryCommutativeOp     a b AndRegReg
            | I32Or   (a,b) -> binaryCommutativeOp     a b OrRegReg  
            | I32Xor  (a,b) -> binaryCommutativeOp     a b XorRegReg
            
            | I32Shl  (a,b) -> shiftOp a b Shl  
            | I32Shrs (a,b) -> shiftOp a b Shrs
            | I32Shru (a,b) -> shiftOp a b Shru
            | I32Rotl (a,b) -> shiftOp a b Rotl
            | I32Rotr (a,b) -> shiftOp a b Rotr

            | _ -> failwith (sprintf "Cannot translate this instruction to simple 32-bit machine: %A" w)   // TODO: Possibly avoid %A

    // Do the translation with the above nested functions:

    let finalTranslation = (translateInstrList ws) @ [ Label returnLabel ] 
    let updatedTranslationState = ModuleTranslationState(labelAllocatorCount)

    (finalTranslation, updatedTranslationState)

module PrivateWasm2ToSimpleReg32

open Wasm
open Wasm2
open SimpleReg32



let (+++) a b = Array.append a b



let FuncLabelFor fidx =
    LabelName("wasm_func" + match fidx with FuncIdx(U32(i)) -> i.ToString()) 



let TranslateInstructions (moduleFuncsArray:Function2[]) (ws:Wasm.Instr[]) : InstrSimpleReg32[] =

    let mutable labelCount = 0
    let mutable labelStack = new ResizeArray<LABELNAME>()

    let newLabel () =
        labelCount <- labelCount + 1
        LabelName(sprintf "label%d" labelCount)

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

    let funcReturnsSomething (fidx:FuncIdx) =
        (getFuncType fidx).ReturnTypes.Length > 0   // will actually only be 0 or 1 in MVP   TODO: validation



    let rec TranslateInstrArray ws =

        Array.concat (ws |> Array.map TranslateInstr)

    and TranslateInstr w =

        let binaryCommutativeOp op = 
            [| 
                PopA; // RHS operand
                PopB; // LHS operand
                op;   // Result in A
                PushA; 
                Barrier 
            |]

        let compareOp op = 
            [| 
                PopA; // RHS operand
                PopB; // LHS operand
                op;   // Compare B (LHS) with A (RHS) and set boolean into A
                PushA; 
                Barrier 
            |]

        let binaryNonCommutativeOp op = 
            [| 
                PopA; // RHS operand
                PopB; // LHS operand
                op;   // Result in B
                LetAB;
                PushA; 
                Barrier 
            |]

        let shiftOp op = 
            [| 
                PopA;  // RHS operand
                PopB;  // LHS operand
                LetCA;
                op;    // Result in B
                LetAB;
                PushA; 
                Barrier 
            |]

        let translateConstruct sourceBody putInOrder =
            let constructLabel = pushNewLabel ()
            let translatedBody = TranslateInstrArray sourceBody
            popLabel ()
            (putInOrder translatedBody [| Label(constructLabel) |]) +++ [| Barrier |]

        match w with

            | Unreachable -> [| Breakpoint |]
            | Nop -> [| |]

            // The following translations must all end with Barrier

            | Instr.Drop -> [| Drop; Barrier |]
        
            | Select -> 
                let l1 = newLabel ()
                let l2 = newLabel ()
                [| 
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
                |]
   
            | Block(_, body) -> translateConstruct body (Array.append)
            | Loop(bt, body) -> translateConstruct body (fun a b -> Array.append b a)

            | If(_, body) -> 
                let skipLabel = pushNewLabel ()
                let translatedBody = TranslateInstrArray body
                popLabel ()
                [| PopA; BranchAZ(skipLabel); |] +++ translatedBody +++ [| Label(skipLabel); Barrier |]
                    
            | IfElse(_, ifbody, elsebody) ->

                let skipIfLabel = pushNewLabel ()
                let translatedIfBody = TranslateInstrArray ifbody
                popLabel ()

                let skipElseLabel = pushNewLabel ()
                let translatedElseBody = TranslateInstrArray elsebody
                popLabel ()

                [| PopA; BranchAZ(skipIfLabel); |] +++ 
                    translatedIfBody +++ 
                    [| Goto(skipElseLabel); Label(skipIfLabel); |] +++ 
                    translatedElseBody +++ 
                    [| Label(skipElseLabel); Barrier |]

            | Br(target)   -> [| Goto(labelFor target); Barrier |]

            | BrIf(target) -> [| PopA; BranchANZ(labelFor target); Barrier |]

            | BrTable(labelArray, defaultLabel) -> 
                let tableLabel = newLabel ()
                [| PopA; 
                   GotoIndex(tableLabel, labelArray.Length, labelFor defaultLabel);
                   TableOfAddresses(tableLabel, labelArray |> Array.map labelFor)
                |]

            | Return -> [| Goto(returnLabel) |]
            
            | Call(funcIdx) -> 
                let fl = FuncLabelFor funcIdx
                if funcIdx |> funcReturnsSomething 
                then [| CallFunc(fl); PushA; Barrier |]
                else [| CallFunc(fl); Barrier |]

            | CallIndirect(funcType) ->
                [| PopA; CallTableIndirect |]  // TODO: runtime validation of funcType

            | I32Const(I32(C)) -> [| ConstA(Const32(C)); PushA; Barrier |]

            | GetLocal(L)  -> [| FetchLocA(L); PushA; Barrier |]
            | SetLocal(L)  -> [| PopA; StoreALoc(L); Barrier |]
            | TeeLocal(L)  -> [| PeekA; StoreALoc(L); Barrier |]
        
            | GetGlobal(G) -> [| FetchGloA(G); PushA; Barrier |]
            | SetGlobal(G) -> [| PopA; StoreAGlo(G); Barrier |]

            | I32Store8(  {Align=_; Offset=O})       -> [| PopA; PopB; Store8AtoB(O);  Barrier |]
            | I32Store16( {Align=U32(1u); Offset=O}) -> [| PopA; PopB; Store16AtoB(O); Barrier |]
            | I32Store(   {Align=U32(2u); Offset=O}) -> [| PopA; PopB; Store32AtoB(O); Barrier |]
            | I32Store16( {Align=U32(A); Offset=_})  -> failwith "Cannot translate 16-bit store unless alignment is 2 bytes"
            | I32Store(   {Align=U32(A); Offset=_})  -> failwith "Cannot translate 32-bit store unless alignment is 4 bytes"

            | I32Load8s(  {Align=_; Offset=O})       -> [| PopA; Fetch8sFromA(O);  PushA; Barrier |]
            | I32Load8u(  {Align=_; Offset=O})       -> [| PopA; Fetch8uFromA(O);  PushA; Barrier |]
            | I32Load16s( {Align=U32(1u); Offset=O}) -> [| PopA; Fetch16sFromA(O); PushA; Barrier |]
            | I32Load16u( {Align=U32(1u); Offset=O}) -> [| PopA; Fetch16uFromA(O); PushA; Barrier |]
            | I32Load(    {Align=U32(2u); Offset=O}) -> [| PopA; Fetch32FromA(O);  PushA; Barrier |]
            | I32Load16s( {Align=U32(A); Offset=_})  -> failwith "Cannot translate 16-bit sign-extended load unless alignemtn is 2 bytes"
            | I32Load16u( {Align=U32(A); Offset=_})  -> failwith "Cannot translate 16-bit unsigned load unless alignment is 2 bytes"
            | I32Load(    {Align=U32(A); Offset=_})  -> failwith "Cannot translate 32-bit load unless alignment is 4 bytes"

            | I32Eqz -> [| PopA; CmpAZ; PushA; |] 

            | I32Eq   -> compareOp CmpEqBA 
            | I32Ne   -> compareOp CmpNeBA

            | I32Lts  -> compareOp CmpLtsBA
            | I32Ltu  -> compareOp CmpLtuBA
            | I32Gts  -> compareOp CmpGtsBA
            | I32Gtu  -> compareOp CmpGtuBA
            | I32Les  -> compareOp CmpLesBA
            | I32Leu  -> compareOp CmpLeuBA
            | I32Ges  -> compareOp CmpGesBA
            | I32Geu  -> compareOp CmpGeuBA

            | I32Add  -> binaryCommutativeOp AddBA
            | I32Sub  -> binaryNonCommutativeOp SubBA
            | I32Mul  -> binaryCommutativeOp MulBA
            | I32Divs -> binaryNonCommutativeOp DivsBA
            | I32Divu -> binaryNonCommutativeOp DivuBA
            | I32Rems -> binaryNonCommutativeOp RemsBA
            | I32Remu -> binaryNonCommutativeOp RemuBA
            | I32And  -> binaryCommutativeOp AndBA
            | I32Or   -> binaryCommutativeOp OrBA
            | I32Xor  -> binaryCommutativeOp XorBA
            | I32Shl  -> shiftOp ShlBC
            | I32Shrs -> shiftOp ShrsBC
            | I32Shru -> shiftOp ShruBC
            | I32Rotl -> shiftOp RotlBC
            | I32Rotr -> shiftOp RotrBC

            | _ -> failwith "Cannot translate this instruction to simple 32-bit machine."

    // Do the translation using the nested functions,
    // and append the return label before we're done:

    (TranslateInstrArray ws) +++ [| Label(returnLabel) |]



let ReplaceAll patternLength patternMatcher (replaceWith:InstrSimpleReg32[]) (a:InstrSimpleReg32[]) =

    let al = a.Length

    if patternLength >= al 
    then 
        a  // Trivially can't be any matches
    else
        let result = new ResizeArray<InstrSimpleReg32> ()
        let mutable i = 0

        while i < (al - patternLength) do
            if patternMatcher i a
            then 
                i <- i + patternLength
                result.AddRange(replaceWith)
            else
                result.Add(a.[i])
                i <- i + 1

        while i < al do
            result.Add(a.[i])
            i <- i + 1

        result.ToArray ()




let IsPushA   i = match i with | PushA   -> true | _ -> false
let IsPeekA   i = match i with | PeekA   -> true | _ -> false
let IsPopA    i = match i with | PopA    -> true | _ -> false
let IsBarrier i = match i with | Barrier -> true | _ -> false
let IsDrop    i = match i with | Drop    -> true | _ -> false


let WherePushBarrierPop  i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPopA a.[i+2]
let WherePushBarrierDrop i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsDrop a.[i+2]
let WherePushBarrierPeek i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPeekA a.[i+2]
let WhereBarrier         i (a:InstrSimpleReg32[]) = IsBarrier a.[i]


let Optimise (originalArray:InstrSimpleReg32[]) =

    originalArray
        |> ReplaceAll 3 WherePushBarrierPop  [||]
        |> ReplaceAll 3 WherePushBarrierDrop [||]
        |> ReplaceAll 3 WherePushBarrierPeek [| PushA |]

let RemoveBarriers (originalArray:InstrSimpleReg32[]) =  // <- Must only ever be final

    originalArray
        |> ReplaceAll 1 WhereBarrier  [||]   










let ValTypeTranslationOf =
    function
        | I32Type -> "int"
        | I64Type -> failwith "Cannot translate I64 type with this simple translator"
        | F32Type -> failwith "Cannot translate F32 type with this simple translator"
        | F64Type -> failwith "Cannot translate F64 type with this simple translator"


let ParamListOf (ps:ValType[]) =
    String.concat ", " (ps |> Array.map ValTypeTranslationOf)


let Surrounded (before:string) (after:string) (s:string) =
    match s.Length with
        | 0 -> ""
        | _ -> before + s + after


let Bracketed s =
    Surrounded "(" ")" s


let Prefixed thePrefix s =
    Surrounded thePrefix "" s


let ColonPrefixed s = 
    Prefixed ": " s


let TextSignatureOf (funcType:FuncType) =
    let translatedParameters = ParamListOf funcType.ParameterTypes
    let translatedReturns = ParamListOf funcType.ReturnTypes
    (Bracketed translatedParameters) + (ColonPrefixed translatedReturns)


let AtParamDecls (ps:ValType[]) =
    String.concat ", " (ps |> Array.mapi (fun i t -> (sprintf "@local%d" i)))
   

let AsmSignatureOf (funcType:FuncType) =
    let atParamsString = AtParamDecls funcType.ParameterTypes
    (Bracketed atParamsString) + "  // " + (TextSignatureOf funcType)


let TranslateLocals writeOut (funcType:FuncType) valTypes =

    let localBaseIndex = funcType.ParameterTypes.Length

    valTypes |> Array.iteri (fun arrayIndex v ->
        let indexOfVariable = localBaseIndex + arrayIndex
        let prefixStr = if arrayIndex = 0 then "var " else "  , "
        writeOut (sprintf "%s@local%d:%s" prefixStr indexOfVariable (ValTypeTranslationOf v)))


let ReturnCommandFor (ft:FuncType) =
    match ft.ParameterTypes.Length with
        | 0 -> "ret"
        | _ -> "endproc"    // TODO: Not sure if this can't just be endproc everywhere now in the ASM ?


let LabelTextOf l = match l with LabelName(n) -> n
let FuncNameOf  l = match l with LabelName(n) -> n   // TODO: type usage isn't right: no distinction


let InstructionsToText writeOut instrs =

    let writeIns s = writeOut ("    " + s)

    let writeU32 s1 u s2 = writeIns (sprintf "%s%s%s" s1 (match u with 
                                                            | U32(0u) -> ""   // indexed addressing not needed
                                                            | U32(n)  -> "+" + n.ToString()) s2)   // indexed addressing

    let writeLoc s1 i s2 = writeIns (sprintf "%s%d%s" s1 (match i with LocalIdx(U32(i)) -> i) s2)

    let rec instructionsToText instrs =
        instrs |> Array.iter instructionToText

    and instructionToText (ins:InstrSimpleReg32) =
        match ins with
            | Barrier               -> writeIns "// ~~~ register barrier ~~~"
            | Breakpoint            -> writeIns "break"
            | Drop                  -> writeIns "add SP,4"
            | Label(l)              -> writeOut ("=> " + LabelTextOf(l))   // TODO: sort out ASM local label references
            | ConstA(Const32(n))    -> writeIns ("let A=" + n.ToString())
            | Goto(l)               -> writeIns ("goto " + LabelTextOf(l))
            | CallFunc(l)           -> writeIns ("call " + FuncNameOf(l))
            | CallTableIndirect     -> writeIns "call table indirect -- TODO" // TODO
            | BranchAZ(l)           -> writeIns ("cmp A,0:if z goto " + LabelTextOf(l))
            | BranchANZ(l)          -> writeIns ("cmp A,0:if nz goto " + LabelTextOf(l))
            | GotoIndex(_,_,_)      -> writeIns "goto index not yet implemented -- TODO" // TODO
            | TableOfAddresses(_,_) -> writeIns "table of addresses not yet implemented -- TODO" // TODO
            | PushA  -> writeIns "push A"
            | PeekA  -> writeIns "let A=int [SP]"
            | PopA   -> writeIns "pop A"
            | PopB   -> writeIns "pop B"
            | PopC   -> writeIns "pop C"
            | LetAB  -> writeIns "let A=B"
            | LetAC  -> writeIns "let A=C"
            | LetCA  -> writeIns "let C=A"
            | AddBA  -> writeIns "add A,B"  // commutative
            | SubBA  -> writeIns "sub B,A"
            | MulBA  -> writeIns "mul A,B"  // commutative
            | DivsBA | DivuBA | RemsBA | RemuBA -> failwith "Assembler does not have a division instruction"
            | AndBA  -> writeIns "and A,B"  // commutative
            | OrBA   -> writeIns "or A,B"    // commutative
            | XorBA  -> writeIns "xor A,B"  // commutative
            | ShlBC  -> writeIns "shl B,C"
            | ShrsBC -> writeIns "sar B,C"
            | ShruBC -> writeIns "shr B,C"
            | RotlBC | RotrBC -> failwith "Assembler does not have a rotate instruction"
            | CmpEqBA          -> writeIns "cmp B,A:set z A"
            | CmpNeBA          -> writeIns "cmp B,A:set nz A"
            | CmpLtsBA         -> writeIns "cmp B,A:set < A"
            | CmpLtuBA         -> writeIns "cmp B,A:set << A"
            | CmpGtsBA         -> writeIns "cmp B,A:set > A"
            | CmpGtuBA         -> writeIns "cmp B,A:set >> A"
            | CmpLesBA         -> writeIns "cmp B,A:set <= A"
            | CmpLeuBA         -> writeIns "cmp B,A:set <<= A"
            | CmpGesBA         -> writeIns "cmp B,A:set >= A"
            | CmpGeuBA         -> writeIns "cmp B,A:set >>= A"
            | CmpAZ            -> writeIns "cmp B,A:set z A"
            | FetchLocA(i)     -> writeLoc "let A=int[@local" i "]"  // TODO: the text "local" should be defined once
            | StoreALoc(i)     -> writeLoc "let int[@local" i "]=A"
            | FetchGloA(i)     -> writeIns "fetch global todo" // TODO
            | StoreAGlo(i)     -> writeIns "store global todo" // TODO
            | Store8AtoB(i)    -> writeU32 "let byte[B" i "]=A"
            | Store16AtoB(i)   -> writeU32 "let ushort[B" i "]=A"
            | Store32AtoB(i)   -> writeU32 "let uint[B" i "]=A"  // TODO:  Won't work for gcc using address 4 as stack pointer
            | Fetch8sFromA(i)  -> writeU32 "let A=sbyte[A" i "]"
            | Fetch8uFromA(i)  -> writeU32 "let A=byte[A" i "]"
            | Fetch16sFromA(i) -> writeU32 "let A=short[A" i "]"
            | Fetch16uFromA(i) -> writeU32 "let A=ushort[A" i "]"
            | Fetch32FromA(i)  -> writeU32 "let A=uint[A" i "]"  // TODO:  Won't work for gcc using address 4 as stack pointer

    // Kick off the whole thing here:

    instructionsToText instrs
    


let TranslateFunction writeOut funcIndex (m:Module2) (f:InternalFunction2Record) =   // TODO: module only needed to query function metadata in TranslateInstructions
    
    writeOut (sprintf "procedure wasm_func%d%s" funcIndex (AsmSignatureOf f.FuncType))
    TranslateLocals writeOut f.FuncType f.Locals

    let funcInstructions = f.Body |> (TranslateInstructions m.Funcs) 
    let optimisedInstructions = funcInstructions |> Optimise
    let withoutBarriers = optimisedInstructions |> RemoveBarriers

    writeOut "// NON-OPTIMISED:"
    InstructionsToText writeOut funcInstructions    // TODO: Don't want both of these outputs

    writeOut "// OPTIMISED:"
    InstructionsToText writeOut optimisedInstructions

    writeOut "// OPTIMISED WITHOUT BARRIERS:"
    InstructionsToText writeOut withoutBarriers

    writeOut (ReturnCommandFor f.FuncType)
    writeOut ""


let TranslateStart writeOut (s:Start option) =
    match s with 
        | None -> writeOut "// No entry point in this translation"
        | Some(st) -> 
            writeOut "procedure wasm_start"
            let labelName = FuncLabelFor st.StartFuncIdx
            writeOut (sprintf "goto %s" (match labelName with LabelName(str) -> str))



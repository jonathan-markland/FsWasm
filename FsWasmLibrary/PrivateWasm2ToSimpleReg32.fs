module PrivateWasm2ToSimpleReg32

open Wasm
open Wasm2
open SimpleReg32
open SimpleReg32Constants
open OptimiseSimpleReg32



let (+++) a b = List.append a b



let FuncLabelFor fidx =
    LabelName(AsmFuncNamePrefix + match fidx with FuncIdx(U32(i)) -> i.ToString()) 



let TranslateInstructions (moduleFuncsArray:Function2[]) (ws:Wasm.Instr list) : InstrSimpleReg32 list =

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
        let numReturns = (getFuncType fidx).ReturnTypes.Length
        if numReturns > 1 then failwith "Cannot translate functions which return more than 1 result"
        numReturns > 0



    let rec TranslateInstrArray ws =

        List.concat (ws |> List.map TranslateInstr)

    and TranslateInstr w =

        let binaryCommutativeOp lhs rhs op = 
            (TranslateInstr lhs)+++(TranslateInstr rhs)+++
            [
                PopA; // RHS operand
                PopB; // LHS operand
                op;   // Result in A
                PushA; 
                Barrier 
            ]

        let compareOp lhs rhs op = 
            (TranslateInstr lhs)+++(TranslateInstr rhs)+++
            [
                PopA; // RHS operand
                PopB; // LHS operand
                op;   // Compare B (LHS) with A (RHS) and set boolean into A
                PushA; 
                Barrier 
            ]

        let binaryNonCommutativeOp lhs rhs op = 
            (TranslateInstr lhs)+++(TranslateInstr rhs)+++
            [
                PopA; // RHS operand
                PopB; // LHS operand
                op;   // Result in B
                LetAB;
                PushA; 
                Barrier 
            ]

        let shiftOp lhs rhs op = 
            (TranslateInstr lhs)+++(TranslateInstr rhs)+++
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
            let translatedBody = TranslateInstrArray sourceBody
            popLabel ()
            List.rev ((putInOrder translatedBody [ Label(constructLabel) ]) +++ [ Barrier ])

        match w with

            | Unreachable -> [ Breakpoint ]
            | Nop -> []

            // The following translations must all end with Barrier

            | Instr.Drop -> [ Drop; Barrier ]
        
            | Select -> 
                let l1 = newLabel ()
                let l2 = newLabel ()
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
                let translatedBody = TranslateInstrArray body
                popLabel ()
                [ PopA; BranchAZ(skipLabel); ] +++ translatedBody +++ [ Label(skipLabel); Barrier ]
                    
            | IfElse(_, ifbody, elsebody) ->

                let skipIfLabel = pushNewLabel ()
                let translatedIfBody = TranslateInstrArray ifbody
                popLabel ()

                let skipElseLabel = pushNewLabel ()
                let translatedElseBody = TranslateInstrArray elsebody
                popLabel ()

                [ PopA; BranchAZ(skipIfLabel); ] +++ 
                    translatedIfBody +++ 
                    [ Goto(skipElseLabel); Label(skipIfLabel); ] +++ 
                    translatedElseBody +++ 
                    [ Label(skipElseLabel); Barrier ]

            | Br(target)   -> [ Goto(labelFor target); Barrier ]

            | BrIf(target) -> [ PopA; BranchANZ(labelFor target); Barrier ]

            | BrTable(indexExpression, labelArray, defaultLabel) -> 
                let tableLabel = newLabel ()
                (TranslateInstr indexExpression) +++
                [
                    PopA; 
                    GotoIndex(tableLabel, labelArray.Length, labelFor defaultLabel);
                    TableOfAddresses(tableLabel, Array.map labelFor labelArray)
                ]

            | Return -> [ Goto(returnLabel) ]
            
            | Call(funcIdx,paramList) -> 
                let translatedParams = TranslateInstrArray paramList
                let fl = FuncLabelFor funcIdx
                if funcIdx |> funcReturnsSomething 
                then translatedParams +++ [ CallFunc(fl); PushA; Barrier ]
                else translatedParams +++ [ CallFunc(fl); Barrier ]

            | CallIndirect(funcType,paramList) ->
                // TODO: runtime validation of funcType
                let translatedParams = TranslateInstrArray paramList
                translatedParams +++ [ PopA; CallTableIndirect ]

            | I32Const(I32(C)) -> [ ConstA(Const32(C)); PushA; Barrier ]

            | GetLocal(L)  -> [ FetchLocA(L); PushA; Barrier ]
            | SetLocal(L)  -> [ PopA;  StoreALoc(L); Barrier ]
            | TeeLocal(L)  -> [ PeekA; StoreALoc(L); Barrier ]
        
            | GetGlobal(G) -> [ FetchGloA(G); PushA; Barrier ]
            | SetGlobal(G) -> [ PopA; StoreAGlo(G);  Barrier ]

            | I32Store8(  {Align=_;       Offset=O}, lhs, rhs) -> (TranslateInstr lhs)+++(TranslateInstr rhs)+++[ PopA; PopB; AddBY; Store8AtoB(O);  Barrier ]
            | I32Store16( {Align=U32(1u); Offset=O}, lhs, rhs) -> (TranslateInstr lhs)+++(TranslateInstr rhs)+++[ PopA; PopB; AddBY; Store16AtoB(O); Barrier ]
            | I32Store(   {Align=U32(2u); Offset=O}, lhs, rhs) -> (TranslateInstr lhs)+++(TranslateInstr rhs)+++[ PopA; PopB; AddBY; Store32AtoB(O); Barrier ]
            | I32Store16( {Align=U32(_);  Offset=_},   _,   _) -> failwith "Cannot translate 16-bit store unless alignment is 2 bytes"
            | I32Store(   {Align=U32(_);  Offset=_},   _,   _) -> failwith "Cannot translate 32-bit store unless alignment is 4 bytes"

            | I32Load8s(  {Align=_;       Offset=O}, operand) -> (TranslateInstr operand)+++[ PopA; AddAY; Fetch8sFromA(O);  PushA; Barrier ]
            | I32Load8u(  {Align=_;       Offset=O}, operand) -> (TranslateInstr operand)+++[ PopA; AddAY; Fetch8uFromA(O);  PushA; Barrier ]
            | I32Load16s( {Align=U32(1u); Offset=O}, operand) -> (TranslateInstr operand)+++[ PopA; AddAY; Fetch16sFromA(O); PushA; Barrier ]
            | I32Load16u( {Align=U32(1u); Offset=O}, operand) -> (TranslateInstr operand)+++[ PopA; AddAY; Fetch16uFromA(O); PushA; Barrier ]
            | I32Load(    {Align=U32(2u); Offset=O}, operand) -> (TranslateInstr operand)+++[ PopA; AddAY; Fetch32FromA(O);  PushA; Barrier ]
            | I32Load16s( {Align=U32(A);  Offset=_}, _) -> failwith "Cannot translate 16-bit sign-extended load unless alignment is 2 bytes"
            | I32Load16u( {Align=U32(A);  Offset=_}, _) -> failwith "Cannot translate 16-bit unsigned load unless alignment is 2 bytes"
            | I32Load(    {Align=U32(A);  Offset=_}, _) -> failwith "Cannot translate 32-bit load unless alignment is 4 bytes"

            | I32Eqz(operand) -> (TranslateInstr operand)+++[ PopA; CmpAZ; PushA; ] 

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

            | I32Add (a,b) -> binaryCommutativeOp    a b AddAB
            | I32Sub (a,b) -> binaryNonCommutativeOp a b SubBA
            | I32Mul (a,b) -> binaryCommutativeOp    a b MulAB
            | I32Divs(a,b) -> binaryNonCommutativeOp a b DivsBA
            | I32Divu(a,b) -> binaryNonCommutativeOp a b DivuBA
            | I32Rems(a,b) -> binaryNonCommutativeOp a b RemsBA
            | I32Remu(a,b) -> binaryNonCommutativeOp a b RemuBA
            | I32And (a,b) -> binaryCommutativeOp    a b AndAB
            | I32Or  (a,b) -> binaryCommutativeOp    a b OrAB
            | I32Xor (a,b) -> binaryCommutativeOp    a b XorAB
            | I32Shl (a,b) -> shiftOp a b ShlBC
            | I32Shrs(a,b) -> shiftOp a b ShrsBC
            | I32Shru(a,b) -> shiftOp a b ShruBC
            | I32Rotl(a,b) -> shiftOp a b RotlBC
            | I32Rotr(a,b) -> shiftOp a b RotrBC

            | _ -> failwith "Cannot translate this instruction to simple 32-bit machine."

    // Do the translation using the nested functions,
    // and append the return label before we're done:

    (TranslateInstrArray ws) +++ [ Label(returnLabel) ]










let StaticEvaluate (instrs:Instr list) : int =

    match instrs.Length with // TODO: proper list matching
        | 1 -> ()
        | _ -> failwith "Cannot statically evaluate instruction sequence"  // TODO: clarify

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
    sprintf "%s%d" AsmFuncNamePrefix (FuncIdxAsUint32 funcIdx)





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




let TranslateLocals writeOut (funcType:FuncType) valTypes =

    let localBaseIndex = funcType.ParameterTypes.Length

    valTypes |> Array.iteri (fun arrayIndex v ->
        let indexOfVariable = localBaseIndex + arrayIndex
        let prefixStr = if arrayIndex = 0 then "var " else "  , "
        writeOut (sprintf "%s@%s%d:%s" prefixStr AsmLocalNamePrefix indexOfVariable (ValTypeTranslationOf v)))


let ReturnCommandFor (ft:FuncType) =
    match ft.ParameterTypes.Length with
        | 0 -> "ret"
        | _ -> "endproc"    // TODO: Not sure if this can't just be endproc everywhere now in the ASM ?






let TranslateTable writeOut i (m:Module2) (t:InternalTable2Record) =

    let writeIns s = writeOut ("    " + s)

    writeOut (sprintf "data %s%d" AsmTableNamePrefix i)

    if t.InitData.Length > 1 then failwith "Cannot translate module with more than one Elem in a TableSec table"

    t.InitData |> Array.iter (fun elem ->
            let ofsExpr, funcIdxList = elem
            let ofsValue = StaticEvaluate ofsExpr
            if ofsValue <> 0 then failwith "Cannot translate module with TableSec table that has Elem with non-zero data initialisation offset"
            funcIdxList |> Array.iter (fun funcIdx -> writeIns (FuncIdxNameString funcIdx))
        )




let TranslateMemory writeOut i (t:InternalMemory2Record) =

    let writeIns s = writeOut ("    " + s)

    writeOut (sprintf "data %s%d" AsmMemoryNamePrefix i)

    t.InitData |> Array.iter (fun elem ->
            let ofsExpr, byteArray = elem
            // TODO: let ofsValue = StaticEvaluate ofsExpr
            byteArray |> Array.iter (fun byteVal -> writeIns (sprintf "byte %d" byteVal))   // TODO: hex, and rows of 16
        )




let TranslateGlobal writeOut i (m:Module2) (g:InternalGlobal2Record) =

    // TODO: We do nothing with the immutability information.  Could we avoid a store and hoist the constant into the code?

    let initValue = StaticEvaluate g.InitExpr
    let globalIdx = GlobalIdx(U32(uint32 i))   // TODO: not ideal construction of temporary

    writeOut (sprintf "data %s int %d" (GlobalIdxNameString globalIdx) initValue)




let TablesOfAddressesToDataSectionText writeOut (m:Module2) (f:InternalFunction2Record) =

    let funcInstructions = f.Body |> TranslateInstructions m.Funcs   // TODO: bad we do this in func outputting too!

    let writeIns s = writeOut ("    " + s)

    funcInstructions |> List.iter (fun ins ->
        match ins with
            | TableOfAddresses(tableLabel,codePointLabels) ->
                writeOut (sprintf "data %s" (LabelTextOf tableLabel))
                codePointLabels |> Array.iter (fun lbl -> writeIns (sprintf "int %s" (LabelTextOf lbl)))
                writeOut ""
            | _ -> ()
        )



let InstructionsToText writeOut instrs =

    let writeIns s = writeOut ("    " + s)

    let writeU32 s1 u s2 = writeIns (sprintf "%s%s%s" s1 (match u with 
                                                            | U32(0u) -> ""   // indexed addressing not needed
                                                            | U32(n)  -> "+" + n.ToString()) s2)   // indexed addressing

    let writeLoc s1 i s2 = writeIns (sprintf "%s%d%s" s1 (LocalIdxAsUint32 i) s2)

    let instructionToText ins =  // These translations can assume a 32-bit target for now.

        match ins with
            | Barrier               -> writeIns "// ~~~ register barrier ~~~"
            | Breakpoint            -> writeIns "break"
            | Drop                  -> writeIns "add SP,4"
            | Label(l)              -> writeOut ("=> " + LabelTextOf l)   // TODO: sort out ASM local label references
            | ConstA(Const32(n))    -> writeIns ("let A=" + n.ToString())
            | Goto(l)               -> writeIns ("goto " + LabelTextOf l)
            | CallFunc(l)           -> writeIns ("call " + FuncNameOf l)
            | CallTableIndirect     -> writeIns "call table indirect -- TODO" // TODO
            | BranchAZ(l)           -> writeIns ("cmp A,0:if z goto " + LabelTextOf l)
            | BranchANZ(l)          -> writeIns ("cmp A,0:if nz goto " + LabelTextOf l)
            | GotoIndex(_,_,_)      -> writeIns "goto index not yet implemented -- TODO" // TODO
            | TableOfAddresses(_,_) -> ()   // ignore in the code section (separately output these tables)
            | PushA                 -> writeIns "push A"
            | PeekA                 -> writeIns "let A=int [SP]"
            | PopA                  -> writeIns "pop A"
            | PopB                  -> writeIns "pop B"
            | PopC                  -> writeIns "pop C"
            | LetAB                 -> writeIns "let A=B"
            | LetAC                 -> writeIns "let A=C"
            | LetCA                 -> writeIns "let C=A"
            | AddAY                 -> writeIns "add A,Y"  // used for address space relocation
            | AddBY                 -> writeIns "add B,Y"  // used for address space relocation
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
            | FetchLocA(i)          -> writeLoc ("let A=int[@" + AsmLocalNamePrefix) i "]"
            | StoreALoc(i)          -> writeLoc ("let int[@" + AsmLocalNamePrefix) i "]=A"
            | FetchGloA(i)          -> writeIns (sprintf "let A=int[%s]" (GlobalIdxNameString i))  // TODO: Eventually use the type rather than "int"
            | StoreAGlo(i)          -> writeIns (sprintf "let int[%s]=A" (GlobalIdxNameString i))  // TODO: Eventually use the type rather than "int"
            | Store8AtoB(i)         -> writeU32 "let byte[B" i "]=A"
            | Store16AtoB(i)        -> writeU32 "let ushort[B" i "]=A"
            | Store32AtoB(i)        -> writeU32 "let uint[B" i "]=A"  // TODO:  Won't work for gcc using address 4 as stack pointer
            | Fetch8sFromA(i)       -> writeU32 "let A=sbyte[A" i "]"
            | Fetch8uFromA(i)       -> writeU32 "let A=byte[A" i "]"
            | Fetch16sFromA(i)      -> writeU32 "let A=short[A" i "]"
            | Fetch16uFromA(i)      -> writeU32 "let A=ushort[A" i "]"
            | Fetch32FromA(i)       -> writeU32 "let A=uint[A" i "]"  // TODO:  Won't work for gcc using address 4 as stack pointer

    // Kick off the whole thing here:

    instrs |> List.iter instructionToText
    


let TranslateFunction writeOut funcIndex (m:Module2) (f:InternalFunction2Record) =   // TODO: module only needed to query function metadata in TranslateInstructions
    
    writeOut (sprintf "procedure %s%d%s" AsmFuncNamePrefix funcIndex (AsmSignatureOf f.FuncType))
    TranslateLocals writeOut f.FuncType f.Locals

    let funcInstructions      = f.Body |> TranslateInstructions m.Funcs   // TODO: bad we do this in table outputting
    let optimisedInstructions = funcInstructions // TODO: sort out |> Optimise
    let withoutBarriers       = optimisedInstructions // TODO: sort out |> RemoveBarriers

    //writeOut "// NON-OPTIMISED:"
    //InstructionsToText writeOut funcInstructions    // TODO: Don't want both of these outputs

    //writeOut "// OPTIMISED:"
    //InstructionsToText writeOut optimisedInstructions

    //writeOut "// OPTIMISED WITHOUT BARRIERS:"
    InstructionsToText writeOut withoutBarriers

    writeOut (ReturnCommandFor f.FuncType)
    writeOut ""



let TranslateStart writeOut (s:Start option) =

    match s with 

        | None -> writeOut "// No entry point in this translation"

        | Some(st) -> 
            writeOut ("procedure " + AsmEntryPointLabel)
            let labelName = FuncLabelFor st.StartFuncIdx
            writeOut (sprintf "goto %s" (match labelName with LabelName(str) -> str))



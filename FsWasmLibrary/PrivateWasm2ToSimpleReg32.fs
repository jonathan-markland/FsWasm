module PrivateWasm2ToSimpleReg32

open Wasm
open SimpleReg32



let (+++) a b = Array.append a b



let funcLabelFor f =
    LabelName("Func" + match f with FuncIdx(U32(x)) -> x.ToString()) 



let TranslateInstructionsMaster (ws:Wasm.Instr[]) : InstrSimpleReg32[] =

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




    let rec TranslateInstructions ws =

        Array.concat (ws |> Array.map TranslateInstruction)

    and TranslateInstruction w =

        let binaryOp op = 
            [| 
                PopA; // RHS operand
                PopB; // LHS operand
                op; 
                PushA; 
                Barrier 
            |]

        let translateConstruct sourceBody putInOrder =
            let constructLabel = pushNewLabel ()
            let translatedBody = TranslateInstructions sourceBody
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
                let translatedBody = TranslateInstructions body
                popLabel ()
                [| PopA; BranchAZ(skipLabel); |] +++ translatedBody +++ [| Label(skipLabel); Barrier |]
                    
            | IfElse(_, ifbody, elsebody) ->

                let skipIfLabel = pushNewLabel ()
                let translatedIfBody = TranslateInstructions ifbody
                popLabel ()

                let skipElseLabel = pushNewLabel ()
                let translatedElseBody = TranslateInstructions elsebody
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
            
            | Call(funcIdx) -> [| CallFunc(funcLabelFor funcIdx) |]

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

            | I32Eq   -> binaryOp CmpEqBA 
            | I32Ne   -> binaryOp CmpNeBA

            | I32Lts  -> binaryOp CmpLtsBA
            | I32Ltu  -> binaryOp CmpLtuBA
            | I32Gts  -> binaryOp CmpGtsBA
            | I32Gtu  -> binaryOp CmpGtuBA
            | I32Les  -> binaryOp CmpLesBA
            | I32Leu  -> binaryOp CmpLeuBA
            | I32Ges  -> binaryOp CmpGesBA
            | I32Geu  -> binaryOp CmpGeuBA

            | I32Add  -> binaryOp AddBA
            | I32Sub  -> binaryOp SubBA
            | I32Mul  -> binaryOp MulBA
            | I32Divs -> binaryOp DivsBA
            | I32Divu -> binaryOp DivuBA
            | I32Rems -> binaryOp RemsBA
            | I32Remu -> binaryOp RemuBA
            | I32And  -> binaryOp AndBA
            | I32Or   -> binaryOp OrBA
            | I32Xor  -> binaryOp XorBA
            | I32Shl  -> binaryOp ShlBA
            | I32Shrs -> binaryOp ShrsBA
            | I32Shru -> binaryOp ShruBA
            | I32Rotl -> binaryOp RotlBA
            | I32Rotr -> binaryOp RotrBA

            | _ -> failwith "Cannot translate this instruction to simple 32-bit machine."

    // Do the translation using the nested functions,
    // and append the return label before we're done:

    (TranslateInstructions ws) +++ [| Label(returnLabel) |]



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




let Optimise (originalArray:InstrSimpleReg32[]) =

    let WherePushPop  i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPopA a.[i+2]
    let WherePushPeek i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPeekA a.[i+2]
    let WhereBarrier  i (a:InstrSimpleReg32[]) = IsBarrier a.[i]

    originalArray
        |> ReplaceAll 3 WherePushPop  [||]
        |> ReplaceAll 3 WherePushPeek [| PushA |]
        |> ReplaceAll 1 WhereBarrier  [||]   // <- Must only ever be final


module PrivateWasm2ToSimpleReg32

open Wasm



type REG32 = Reg32 of int
type CONST32 = Const32 of int
type LABELNAME = LabelName of string



type InstrSimpleReg32 =

    // A 32-bit register machine with a few registers

    | Barrier     // Marks where all registers are unassigned

    | Breakpoint  // Emit breakpoint instruction
    | Drop        // Discard top of stack

    | Function    of LABELNAME    // Emit function label
    | Label       of LABELNAME    // Emit local label

    | ConstA      of CONST32      // Load constant into reg A

    | Goto        of LABELNAME    // Go to given label
    | CallFunc    of LABELNAME    // Call to given function label
    | CallTableIndirect           // Reg A contains index into WASM Table[0]

    | BranchA0    of LABELNAME    // Branch if A=0
    | BranchA1    of LABELNAME    // Branch if A<>0
    | GotoIndex   of tableLabel:LABELNAME * tableLength:int * defaultLabel:LABELNAME  // Reg A contains index.  Go to defaultLabel if out of range.

    | TableOfAddresses of tableLabel:LABELNAME * LABELNAME array

    | PushA       // Push reg A onto stack
    | PeekA       // Load top-of-stack to A, without any stack adjustment
    | PopA        // Pop top of stack into reg A
    | PopB        // Pop top of stack into reg B
    | PopC        // Pop top of stack into reg C

    | LetAB       // Copy reg B value into reg A
    | LetAC       // Copy reg C value into reg A

    | AddAB       // Calculate A+B, result in A
    | SubAB       // TODO: sort out stack ordering for these non-commutatives
    | MulAB       // Calculate A*B, result in A
    | DivsAB
    | DivuAB
    | RemsAB
    | RemuAB
    | AndAB       // Calculate A AND B, result in A
    | OrAB        // Calculate A OR B, result in A
    | XorAB       // Calculate A XOR B, result in A
    | ShlAB
    | ShrsAB
    | ShruAB
    | RotlAB
    | RotrAB

    | CmpEqAB 
    | CmpNeAB 
    | CmpLtsAB
    | CmpLtuAB
    | CmpGtsAB
    | CmpGtuAB
    | CmpLesAB
    | CmpLeuAB
    | CmpGesAB
    | CmpGeuAB

    | CmpA0                        // Compare A with 0 and set A=1 if so, else set A=0

    | FetchLocA     of LocalIdx    // Fetch local variable value into A
    | StoreALoc     of LocalIdx    // Store A to local variable
    | FetchGloA     of GlobalIdx   // Fetch global variable value into A
    | StoreAGlo     of GlobalIdx   // Store A to global variable

    | Store8AtoB    of U32         // Store least significant 8 bits of reg A to address given by reg B
    | Store16AtoB   of U32         // Store least significant 16 bits of reg A to address given by reg B
    | Store32AtoB   of U32         // Store 32 bits of reg A to address given by reg B

    | Fetch8sFromA  of U32         // Fetch byte from address A, sign-extend with result in A
    | Fetch8uFromA  of U32         // Fetch byte from address A, zero-extend with result in A
    | Fetch16sFromA of U32         // Fetch short from address A, sign-extend with result in A
    | Fetch16uFromA of U32         // Fetch short from address A, zero-extend with result in A
    | Fetch32FromA  of U32         // Fetch 32-bits from address A, into A



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

    let funcLabelFor f =
        LabelName("Func" + match f with FuncIdx(U32(x)) -> x.ToString()) 

    let returnLabel = newLabel ()

    let (+++) a b = Array.append a b

    let rec TranslateInstructions (ws:Wasm.Instr[]) : InstrSimpleReg32[] =

        Array.concat (ws |> Array.map TranslateInstruction)

    and TranslateInstruction (w:Wasm.Instr) : InstrSimpleReg32[] =

        let binaryOp op = [| PopA; PopB; op; PushA; Barrier |]

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
                    BranchA0(l1);
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
                [| PopA; BranchA0(skipLabel); |] +++ translatedBody +++ [| Label(skipLabel); Barrier |]
                    
            | IfElse(_, ifbody, elsebody) ->

                let skipIfLabel = pushNewLabel ()
                let translatedIfBody = TranslateInstructions ifbody
                popLabel ()

                let skipElseLabel = pushNewLabel ()
                let translatedElseBody = TranslateInstructions elsebody
                popLabel ()

                [| PopA; BranchA0(skipIfLabel); |] +++ 
                    translatedIfBody +++ 
                    [| Goto(skipElseLabel); Label(skipIfLabel); |] +++ 
                    translatedElseBody +++ 
                    [| Label(skipElseLabel); Barrier |]

            | Br(target)   -> [| Goto(labelFor target); Barrier |]

            | BrIf(target) -> [| PopA; BranchA1(labelFor target); Barrier |]

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

            | GetLocal(L) -> [| FetchLocA(L); PushA; Barrier |]
            | SetLocal(L) -> [| PopA; StoreALoc(L); Barrier |]
            | TeeLocal(L) -> [| PeekA; StoreALoc(L); Barrier |]
        
            | GetGlobal(G) -> [| FetchGloA(G); PushA; Barrier |]
            | SetGlobal(G) -> [| PopA; StoreAGlo(G); Barrier |]

            | I32Store8( {Align=_; Offset=O})       -> [| PopA; PopB; Store8AtoB(O);  Barrier |]
            | I32Store16({Align=U32(1u); Offset=O}) -> [| PopA; PopB; Store16AtoB(O); Barrier |]
            | I32Store(  {Align=U32(2u); Offset=O}) -> [| PopA; PopB; Store32AtoB(O); Barrier |]
            | I32Store16({Align=U32(A); Offset=_})  -> failwith "Cannot translate 16-bit store unless alignment is 2 bytes"
            | I32Store(  {Align=U32(A); Offset=_})  -> failwith "Cannot translate 32-bit store unless alignment is 4 bytes"

            | I32Load8s(  {Align=_; Offset=O})       -> [| PopA; Fetch8sFromA(O);  Barrier |]
            | I32Load8u(  {Align=_; Offset=O})       -> [| PopA; Fetch8uFromA(O);  Barrier |]
            | I32Load16s( {Align=U32(1u); Offset=O}) -> [| PopA; Fetch16sFromA(O); Barrier |]
            | I32Load16u( {Align=U32(1u); Offset=O}) -> [| PopA; Fetch16uFromA(O); Barrier |]
            | I32Load(    {Align=U32(2u); Offset=O}) -> [| PopA; Fetch32FromA(O);  Barrier |]
            | I32Load16s( {Align=U32(A); Offset=_})  -> failwith "Cannot translate 16-bit sign-extended load unless alignemtn is 2 bytes"
            | I32Load16u( {Align=U32(A); Offset=_})  -> failwith "Cannot translate 16-bit unsigned load unless alignment is 2 bytes"
            | I32Load(    {Align=U32(A); Offset=_})  -> failwith "Cannot translate 32-bit load unless alignment is 4 bytes"

            | I32Eqz -> [| PopA; CmpA0; PushA; |] 

            | I32Eq  -> binaryOp CmpEqAB 
            | I32Ne  -> binaryOp CmpNeAB

            | I32Lts -> binaryOp CmpLtsAB
            | I32Ltu -> binaryOp CmpLtuAB
            | I32Gts -> binaryOp CmpGtsAB
            | I32Gtu -> binaryOp CmpGtuAB
            | I32Les -> binaryOp CmpLesAB
            | I32Leu -> binaryOp CmpLeuAB
            | I32Ges -> binaryOp CmpGesAB
            | I32Geu -> binaryOp CmpGeuAB

            | I32Add  -> binaryOp AddAB
            | I32Sub  -> binaryOp SubAB
            | I32Mul  -> binaryOp MulAB
            | I32Divs -> binaryOp DivsAB
            | I32Divu -> binaryOp DivuAB
            | I32Rems -> binaryOp RemsAB
            | I32Remu -> binaryOp RemuAB
            | I32And  -> binaryOp AndAB
            | I32Or   -> binaryOp OrAB
            | I32Xor  -> binaryOp XorAB
            | I32Shl  -> binaryOp ShlAB
            | I32Shrs -> binaryOp ShrsAB
            | I32Shru -> binaryOp ShruAB
            | I32Rotl -> binaryOp RotlAB
            | I32Rotr -> binaryOp RotrAB

            | _ -> failwith "Cannot translate this instruction to simple 32-bit machine."

    // Do the translation using the nested functions,
    // and append the return label before we're done:

    (TranslateInstructions ws) +++ [| Label(returnLabel) |]

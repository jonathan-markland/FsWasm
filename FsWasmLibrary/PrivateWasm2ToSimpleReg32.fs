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

    | BranchAZ    of LABELNAME    // Branch to given label when A==0
    | BranchANZ   of LABELNAME    // Branch to given label when A<>0
    | GotoIndex   of tableLabel:LABELNAME * tableLength:int * defaultLabel:LABELNAME  // Reg A contains index.  Go to defaultLabel if out of range.

    | TableOfAddresses of tableLabel:LABELNAME * LABELNAME array

    | PushA       // Push reg A onto stack
    | PeekA       // Load top-of-stack to A, without any stack adjustment
    | PopA        // Pop top of stack into reg A
    | PopB        // Pop top of stack into reg B
    | PopC        // Pop top of stack into reg C

    | LetAB       // Copy reg B value into reg A
    | LetAC       // Copy reg C value into reg A

    | AddBA       // Calculate B+A, result in A
    | SubBA       // Calculate B-A, result in A
    | MulBA       // Calculate B*A, result in A
    | DivsBA      // Calculate (signed   B) * (signed   A), result in A
    | DivuBA      // Calculate (unsigned B) * (unsigned A), result in A
    | RemsBA      // Calculate (signed   B) % (signed   A), result in A
    | RemuBA      // Calculate (unsigned B) % (unsigned A), result in A
    | AndBA       // Calculate B AND A, result in A
    | OrBA        // Calculate B OR  A, result in A
    | XorBA       // Calculate B XOR A, result in A
    | ShlBA       // Calculate (unsigned B) SHL A, result in A
    | ShrsBA      // Calculate (signed   B) SHR A, result in A
    | ShruBA      // Calculate (unsigned B) SHR A, result in A
    | RotlBA      // Calculate B ROL A, result in A
    | RotrBA      // Calculate B ROR A, result in A

    | CmpEqBA     // Set A=1 when B == A, else set A=0
    | CmpNeBA     // Set A=1 when B <> A, else set A=0
    | CmpLtsBA    // Set A=1 when (signed   B) < (signed   A) , else set A=0
    | CmpLtuBA    // Set A=1 when (unsigned B) < (unsigned A) , else set A=0
    | CmpGtsBA    // Set A=1 when (signed   B) > (signed   A) , else set A=0
    | CmpGtuBA    // Set A=1 when (unsigned B) > (unsigned A) , else set A=0
    | CmpLesBA    // Set A=1 when (signed   B) <= (signed   A), else set A=0
    | CmpLeuBA    // Set A=1 when (unsigned B) <= (unsigned A), else set A=0
    | CmpGesBA    // Set A=1 when (signed   B) >= (signed   A), else set A=0
    | CmpGeuBA    // Set A=1 when (unsigned B) >= (unsigned A), else set A=0

    | CmpAZ                        // Set A=1 when A == 0, else set A=0

    | FetchLocA     of LocalIdx    // Fetch local variable value into A
    | StoreALoc     of LocalIdx    // Store A to local variable
    | FetchGloA     of GlobalIdx   // Fetch WASM global variable value into A
    | StoreAGlo     of GlobalIdx   // Store A to WASM global variable

    | Store8AtoB    of U32         // Store least significant 8 bits of reg A to WASM linear memory, address given by reg B
    | Store16AtoB   of U32         // Store least significant 16 bits of reg A to WASM linear memory, address given by reg B
    | Store32AtoB   of U32         // Store 32 bits of reg A to WASM linear memory, address given by reg B

    | Fetch8sFromA  of U32         // Fetch byte from WASM linear memory, address A, sign-extend with result in A
    | Fetch8uFromA  of U32         // Fetch byte from WASM linear memory, address A, zero-extend with result in A
    | Fetch16sFromA of U32         // Fetch short from WASM linear memory, address A, sign-extend with result in A
    | Fetch16uFromA of U32         // Fetch short from WASM linear memory, address A, zero-extend with result in A
    | Fetch32FromA  of U32         // Fetch 32-bits from WASM linear memory, address A, into A



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

            | I32Eqz -> [| PopA; CmpAZ; PushA; |] 

            | I32Eq  -> binaryOp CmpEqBA 
            | I32Ne  -> binaryOp CmpNeBA

            | I32Lts -> binaryOp CmpLtsBA
            | I32Ltu -> binaryOp CmpLtuBA
            | I32Gts -> binaryOp CmpGtsBA
            | I32Gtu -> binaryOp CmpGtuBA
            | I32Les -> binaryOp CmpLesBA
            | I32Leu -> binaryOp CmpLeuBA
            | I32Ges -> binaryOp CmpGesBA
            | I32Geu -> binaryOp CmpGeuBA

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

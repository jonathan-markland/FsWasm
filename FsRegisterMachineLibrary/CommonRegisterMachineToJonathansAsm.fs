module CommonRegisterMachineToJonathansAsm

open WasmFileTypes
open WasmBetterTypes
open CommonRegisterMachineTypes
open AsmPrefixes


    

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


module CommonRegisterMachineTypes

open WasmFileTypes

/// A register of the 32-bit machine
type REG32 = Reg32 of int

/// A 32-bit constant
type CONST32 = Const32 of int

/// A code label
type LABELNAME = LabelName of string

/// One of the general registers
type REG = A | B | C | Y



// TODO: open Arm32SpecificCrmOptimisationTypes

/// These are for optimising CRM code when the target is the Arm32.
// type Arm32SpecificCrmInstruction =


// TODO: open X8632SpecificCrmOptimisationTypes

/// These are for optimising CRM code when the target is the X86/32.
type X8632SpecificCrmInstruction =

    /// The X86 push constant instruction supporting up to a full 32-bits.
    | X8632PushConstant of CONST32

    /// Intended for:  mov [EDI+EBX+ofs],(AL | AX | EAX)
    | X8632StoreAatEBXplusEDIplusOffset of offset:U32 * sourceRegName:string

    /// (add | sub | etc) dword ptr [EBP+n],value  // Locals are always a full stack slot
    | X8632OperateOnLocal32 of opcode:string * LocalIdx * I32



/// Common Register Machine condition codes
type CRMCondition =

    /// Equal
    | CrmCondEq

    /// Not Equal
    | CrmCondNe

    /// Less-than (signed)
    | CrmCondLts

    /// Less-than (unsigned)
    | CrmCondLtu  

    /// Greater-than (signed)
    | CrmCondGts   

    /// Greater-than (unsigned)
    | CrmCondGtu

    /// Less-than-or-equal (signed)
    | CrmCondLes    

    /// Less-than-or-equal (unsigned)
    | CrmCondLeu    

    /// Greater-than-or-equal (signed)
    | CrmCondGes    

    /// Greater-than-or-equal (unsigned)
    | CrmCondGeu    




let OppositeCrmConditionFor crmCondition =
    match crmCondition with
    | CrmCondLts -> CrmCondGes   | CrmCondGes -> CrmCondLts
    | CrmCondGts -> CrmCondLes   | CrmCondLes -> CrmCondGts
    | CrmCondLtu -> CrmCondGeu   | CrmCondGeu -> CrmCondLtu
    | CrmCondGtu -> CrmCondLeu   | CrmCondLeu -> CrmCondGtu
    | CrmCondEq  -> CrmCondNe    | CrmCondNe  -> CrmCondEq





/// A 32-bit register machine instruction.
/// All of these need supporting on a new architecture, except those "Specific" ones.
type CRMInstruction32 =

    /// Thunk In.  (Loads Y register to point to linear memory in this implementation).
    | ThunkIn

    /// Marks where all registers are unassigned.  Used to make peephole optimisation reliable.
    | Barrier     

    /// Breakpoint instruction
    | Breakpoint  

    /// Discard top n stack slots
    | Drop        of n:U32

    /// Emit local label
    | Label       of LABELNAME    

    /// Load constant into given register
    | Const       of REG * CONST32      

    /// Go to given label
    | Goto        of LABELNAME    

    /// Call to given function label
    | CallFunc    of LABELNAME    

    /// Reg A contains index into WASM Table[0]
    | CallTableIndirect

    /// Branch to given label when A==0
    | BranchAZ    of LABELNAME    

    /// Branch to given label when A<>0
    | BranchANZ   of LABELNAME    

    /// Reg A contains index.  Go to defaultLabel if out of range.
    /// The lookup table would be separately stored in the generated data section.
    | GotoIndex   of tableLabel:LABELNAME * tableLength:int * defaultLabel:LABELNAME * lookupTable:LABELNAME array


    /// Push reg onto stack
    | Push of REG
    
    /// Pop top of stack into reg
    | Pop of REG
    
    /// Load top-of-stack to A, without any stack adjustment
    | PeekA       
    

    /// Copy RHS reg value into LHS reg
    | Let of REG * REG


    /// Calculate sum of two registers, result in LHS
    | Add of REG * REG
    
    /// Calculate A+n, result in A
    | AddAN of I32
    
    /// Calculate B-A, result in A
    | SubBA       

    /// Calculate A-n, result in A
    | SubAN of I32
    
    /// Calculate A*B, result in A
    | MulAB       
    
    /// Calculate (signed   B) * (signed   A), result in A
    | DivsBA      
    
    /// Calculate (unsigned B) * (unsigned A), result in A
    | DivuBA      
    
    /// Calculate (signed   B) % (signed   A), result in A
    | RemsBA      
    
    /// Calculate (unsigned B) % (unsigned A), result in A
    | RemuBA      
    
    /// Calculate A AND B, result in A
    | AndAB
    
    /// Calculate A OR  B, result in A
    | OrAB
    
    /// Calculate A XOR B, result in A
    | XorAB
    
    /// Calculate A AND N, result in A
    | AndAN of I32
    
    /// Calculate A OR  N, result in A
    | OrAN of I32
    
    /// Calculate A XOR N, result in A
    | XorAN of I32
    
    /// Calculate (unsigned B) SHL C, result in B
    | ShlBC
    
    /// Calculate (signed   B) SHR C, result in B
    | ShrsBC
    
    /// Calculate (unsigned B) SHR C, result in B
    | ShruBC
    
    /// Calculate B ROL C, result in B
    | RotlBC
    
    /// Calculate B ROR C, result in B
    | RotrBC


    /// Compare B and A for the given condition, 
    /// with B on the left, A on the right.
    | CmpBA of CRMCondition
    
    /// Set A=1 when A == 0, else set A=0
    | CmpAZ                        


    /// Fetch local variable value into given register
    | FetchLoc      of REG * LocalIdx    
    
    /// Store given register to local variable
    | StoreLoc      of REG * LocalIdx    
    
    /// Fetch WASM global variable value into given regiser
    | FetchGlo      of REG * GlobalIdx   

    /// Store given regiser to WASM global variable
    | StoreGlo      of REG * GlobalIdx   

    /// Store 8-bit constant I32 to WASM linear memory, address given by REG+U32
    | StoreConst8   of REG * U32 * I32   

    /// Store 16-bit constant I32 to WASM linear memory, address given by REG+U32
    | StoreConst16  of REG * U32 * I32         

    /// Store 32-bit constant I32 to WASM linear memory, address given by REG+U32
    | StoreConst32  of REG * U32 * I32

    /// Store least significant 8 bits of reg A to WASM linear memory, address given by REG+U32
    | Store8A    of REG * U32         

    /// Store least significant 16 bits of reg A to WASM linear memory, address given by REG+U32
    | Store16A   of REG * U32         

    /// Store 32 bits of reg A to WASM linear memory, address given by REG+U32
    | Store32A   of REG * U32         



    /// Fetch byte from WASM linear memory, address REG+U32, sign-extend with result in A
    | Fetch8s  of REG * U32         

    /// Fetch byte from WASM linear memory, address REG+U32, zero-extend with result in A
    | Fetch8u  of REG * U32         

    /// Fetch short from WASM linear memory, address REG+U32, sign-extend with result in A
    | Fetch16s of REG * U32         

    /// Fetch short from WASM linear memory, address REG+U32, zero-extend with result in A
    | Fetch16u of REG * U32         

    /// Fetch 32-bits from WASM linear memory, address REG+U32, into A
    | Fetch32  of REG * U32         

    // ==================================================================================

    /// Not part of primary generation.  Used to optimise.
    /// The operands are in B and A for left/right respectively.
    | SecondaryCmpBranch of CRMCondition * LABELNAME

    // ==================================================================================

    /// An x86/32-specific optimisation instruction.
    | X8632Specific of X8632SpecificCrmInstruction

    /// An Arm/32-specific optimisation instruction.
    // TODO: | Arm32Specific of Arm32SpecificCrmInstruction


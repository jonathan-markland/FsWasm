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



type BranchZNZType = BZero | BNonZero
type StoredType    = Stored8 | Stored16 | Stored32
type FetchedType   = SignExt8 | ZeroExt8 | SignExt16 | ZeroExt16 | SignExt32

type ShiftRotateType = 

    /// Calculate (unsigned B) SHL C, result in B
    | Shl

    /// Calculate (signed   B) SHR C, result in B
    | Shrs

    /// Calculate (unsigned B) SHR C, result in B
    | Shru

    /// Calculate B ROL C, result in B
    | Rotl

    /// Calculate B ROR C, result in B
    | Rotr


type RegConstCalcType =

    /// Calculate Reg+Num, result in Reg
    | AddRegNum
    
    /// Calculate Reg-Num, result in Reg
    | SubRegNum

    /// Calculate Reg AND Num, result in Reg
    | AndRegNum

    /// Calculate Reg OR Num, result in Reg
    | OrRegNum

    /// Calculate Reg XOR Num, result in Reg
    | XorRegNum

    
type CalcRegsOp =

    /// Add registers, result in left-side register.
    | AddRegReg

    /// Subtract registers, result in left-side register.
    | SubRegReg       

    /// Multiply registers, result in left-side register.
    | MulRegReg     
    
    /// Divide registers (signed), (NOT IMPLEMENTED).
    | DivsRegReg     
    
    /// Divide registers (unsigned), (NOT IMPLEMENTED).
    | DivuRegReg
    
    /// Remainder (signed), (NOT IMPLEMENTED).
    | RemsRegReg    
    
    /// Remainder (unsigned), (NOT IMPLEMENTED).
    | RemuRegReg    
    
    /// AND registers together, result in left-side register.
    | AndRegReg
    
    /// OR registers together, result in left-side register.
    | OrRegReg
    
    /// XOR registers together, result in left-side register.
    | XorRegReg
   
   

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

    /// Branch to given label when REG is zero or non-zero
    | BranchRegZNZ of REG * BranchZNZType * LABELNAME    

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
    | CalcRegs of CalcRegsOp * lhs:REG * rhs:REG * out:REG
    
    /// Calculate REG (op) n, result in REG
    | CalcRegNum of RegConstCalcType * REG * I32

    /// Shift and rotate
    | ShiftRot of ShiftRotateType * source:REG * count:REG * out:REG


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


    /// Store constant I32 to WASM linear memory, address given by REG+U32
    | StoreConst    of StoredType * REG * U32 * I32   

    /// Store of given size from least significant end of reg 'src' 
    /// to WASM linear memory, address given by REG+U32
    | Store         of src:REG * StoredType * adr:REG * U32         

    /// Fetch from WASM linear memory to reg 'dest', with sign/zero extension if given.
    /// Address is given by REG + U32
    | Fetch         of dest:REG * FetchedType * adr:REG * U32         

    // ==================================================================================

    /// Not part of primary generation.  Used to optimise.
    /// The operands are in B and A for left/right respectively.
    | SecondaryCmpBranch of CRMCondition * LABELNAME

    // ==================================================================================

    /// An x86/32-specific optimisation instruction.
    | X8632Specific of X8632SpecificCrmInstruction

    /// An Arm/32-specific optimisation instruction.
    // TODO: | Arm32Specific of Arm32SpecificCrmInstruction


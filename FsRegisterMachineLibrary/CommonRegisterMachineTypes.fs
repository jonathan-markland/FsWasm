﻿module CommonRegisterMachineTypes

open WasmFileTypes


/// A register of the 32-bit machine
type REG32 = Reg32 of int

/// A 32-bit constant
type CONST32 = Const32 of int

/// A code label
type LABELNAME = LabelName of string

/// One of the general registers
type REG = A | B | C | Y



/// A 32-bit register machine instruction
type InstrSimpleReg32 =

    /// Thunk In.  (Loads Y register to point to linear memory in this implementation).
    | ThunkIn

    /// Marks where all registers are unassigned.  Used to make peephole optimisation reliable.
    | Barrier     

    /// Breakpoint instruction
    | Breakpoint  

    /// Discard top of stack
    | Drop        

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
    
    /// Calculate A AND B, result in A
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


    /// Set A=1 when B == A, else set A=0
    | CmpEqBA     
    
    /// Set A=1 when B <> A, else set A=0
    | CmpNeBA     
    
    /// Set A=1 when (signed   B) < (signed   A) , else set A=0
    | CmpLtsBA    
    
    /// Set A=1 when (unsigned B) < (unsigned A) , else set A=0
    | CmpLtuBA    
    
    /// Set A=1 when (signed   B) > (signed   A) , else set A=0
    | CmpGtsBA    

    
    /// Set A=1 when (unsigned B) > (unsigned A) , else set A=0
    | CmpGtuBA    
    
    /// Set A=1 when (signed   B) <= (signed   A), else set A=0
    | CmpLesBA    
    
    /// Set A=1 when (unsigned B) <= (unsigned A), else set A=0
    | CmpLeuBA    
    
    /// Set A=1 when (signed   B) >= (signed   A), else set A=0
    | CmpGesBA    
    
    /// Set A=1 when (unsigned B) >= (unsigned A), else set A=0
    | CmpGeuBA    


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




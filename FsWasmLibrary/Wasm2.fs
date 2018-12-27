module Wasm2

open Wasm

type Export2 = { ExportName:Name; }
type Import2 = { Export2:Export2 option; ImportModuleName:Name; ImportName:Name }

type Instr2 =

    // 5.4.1  Control Instructions

    | Unreachable  // 00
    | Nop          // 01
    | Block        of BlockType * Instr2[]  // 02 0B
    | Loop         of BlockType * Instr2[]  // 03 0B
    | If           of BlockType * Instr2[]  // 04 0B
    | IfElse       of BlockType * If:Instr2[] * Else:Instr2[]  // 04 05 0B
    | Br           of LabelIdx  // 0C
    | BrIf         of LabelIdx  // 0D
    | BrTable      of LabelIdx array * LabelIdx  // 0E
    | Return
    | Call         of Function2   // 10
    | CallIndirect of FuncType    // 11 00

    // 5.4.2  Parameteric Instructions

    | Drop    // 1A
    | Select  // 1B

    // 5.4.3  Variable Instructions

    | GetLocal  of LocalIdx    // 20
    | SetLocal  of LocalIdx    // 21
    | TeeLocal  of LocalIdx    // 22
    | GetGlobal of Global2     // 23
    | SetGlobal of Global2     // 24

    // 5.4.4  Memory Instructions

    | I32Load of MemArg     // 28
    | I64Load of MemArg
    | F32Load of MemArg
    | F64Load of MemArg
    | I32Load8s of MemArg   // 2C
    | I32Load8u of MemArg
    | I32Load16s of MemArg
    | I32Load16u of MemArg

    | I64Load8s of MemArg   // 30
    | I64Load8u of MemArg
    | I64Load16s of MemArg
    | I64Load16u of MemArg
    | I64Load32s of MemArg  // 34
    | I64Load32u of MemArg
    | I32Store of MemArg
    | I64Store of MemArg
    | F32Store of MemArg    // 38
    | F64Store of MemArg
    | I32Store8 of MemArg
    | I32Store16 of MemArg
    | I64Store8 of MemArg   // 3C
    | I64Store16 of MemArg
    | I64Store32 of MemArg

    | MemorySize    // 3F 00
    | GrowMemory    // 40 00

    // 5.4.5  Numeric Instructions

    | I32Const of I32  // 41
    | I64Const of I64  // 42
    | F32Const of F32  // 43
    | F64Const of F64  // 44

    | I32Eqz 
    | I32Eq 
    | I32Ne 
    | I32Lts 
    | I32Ltu 
    | I32Gts 
    | I32Gtu 
    | I32Les 
    | I32Leu 
    | I32Ges 
    | I32Geu 

    | I64Eqz   // 50
    | I64Eq 
    | I64Ne 
    | I64Lts 
    | I64Ltu 
    | I64Gts 
    | I64Gtu 
    | I64Les 
    | I64Leu 
    | I64Ges 
    | I64Geu 
    | F32Eq 
    | F32Ne 
    | F32Lt 
    | F32Gt 
    | F32Le
        
    | F32Ge   // 60
    | F64Eq 
    | F64Ne 
    | F64Lt 
    | F64Gt
    | F64Le 
    | F64Ge 
    | I32Clz 
    | I32Ctz 
    | I32PopCnt 
    | I32Add 
    | I32Sub 
    | I32Mul 
    | I32Divs 
    | I32Divu 
    | I32Rems 

    | I32Remu    // 70
    | I32And 
    | I32Or 
    | I32Xor 
    | I32Shl
    | I32Shrs
    | I32Shru
    | I32Rotl
    | I32Rotr
    | I64Clz
    | I64Ctz
    | I64PopCnt
    | I64Add
    | I64Sub
    | I64Mul
    | I64Divs

    | I64Divu   // 80
    | I64Rems
    | I64Remu
    | I64And
    | I64Or
    | I64Xor
    | I64Shl
    | I64Shrs
    | I64Shru
    | I64Rotl
    | I64Rotr
    | F32Abs
    | F32Neg
    | F32Ceil
    | F32Floor
    | F32Trunc

    | F32Nearest   // 90
    | F32Sqrt
    | F32Add
    | F32Sub
    | F32Mul
    | F32Div
    | F32Min
    | F32Max
    | F32CopySign
    | F64Abs
    | F64Neg
    | F64Ceil
    | F64Floor
    | F64Trunc
    | F64Nearest
    | F64Sqrt

    | F64Add   // A0
    | F64Sub
    | F64Mul
    | F64Div
    | F64Min
    | F64Max
    | F64CopySign
    | I32WrapI64 
    | I32TruncsF32 
    | I32TruncuF32 
    | I32TruncsF64 
    | I32TruncuF64 
    | I64ExtendsI32 
    | I64ExtenduI32 
    | I64TruncsF32 
    | I64TruncuF32 

    | I64TruncsF64   // B0
    | I64TruncuF64 
    | F32ConvertsI32 
    | F32ConvertuI32 
    | F32ConvertsI64 
    | F32ConvertuI64 
    | F32DemoteF64 
    | F64ConvertsI32 
    | F64ConvertuI32 
    | F64ConvertsI64 
    | F64ConvertuI64 
    | F64PromoteF32 
    | I32ReinterpretF32  
    | I64ReinterpretF64  
    | F32ReinterpretI32  
    | F64ReinterpretI64 

// FUNCTIONS

and ImportedFunction2Arg = { Import2:Import2; FuncType:FuncType }
and InternalFunction2Arg = { Export2:Export2 option; OriginalCodeSecIndex:U32; CodeSize:U32; FuncType:FuncType; Locals:Locals[]; Body:Instr2[] }

and Function2 =
    | ImportedFunction2 of ImportedFunction2Arg
    | InternalFunction2 of InternalFunction2Arg
    | FunctionNotYetResolved of FuncIdx

// GLOBALS

and ImportedGlobal2Arg = { Import2:Import2; GlobalType:GlobalType }
and InternalGlobal2Arg = { Export2:Export2 option; GlobalType:GlobalType; InitExpr:Instr2[] }

and Global2 =
    | ImportedGlobal2 of ImportedGlobal2Arg
    | InternalGlobal2 of InternalGlobal2Arg

// MEMORY

type ImportedMemory2Arg = { Import2:Import2; MemoryType:MemoryType }
type InternalMemory2Arg = { Export2:Export2 option; MemoryType:MemoryType; InitData:(Instr2[] * byte array)[]; }

type Memory2 =
    | ImportedMemory2 of ImportedMemory2Arg
    | InternalMemory2 of InternalMemory2Arg

// TABLES

type ImportedTable2Arg  = { Import2:Import2; TableType:TableType }
type InternalTable2Arg  = { Export2:Export2 option; TableType:TableType; InitOffsetExpr:Instr2[]; InitWith:Function2 array }

type Table2 =
    | ImportedTable2 of ImportedTable2Arg
    | InternalTable2 of InternalTable2Arg

// START

type Start2 = { StartFunction2:Function2 }

// MODULE

type Module2 = {
    // NB: Decision made to drop custom sections, I could not, in general, interpret them anyway.
    funcs:Function2[];
    mems:Memory2[];
    tables:Table2[];
    globals:Global2[];
    start:Start2 option; }


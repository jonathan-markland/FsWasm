﻿module Wasm

open System

    type I32 = I32 of int
    type I64 = I64 of int64
    type U32 = U32 of uint32
    type F32 = F32 of float32
    type F64 = F64 of float

    type Mutability  = Constant | Variable      // 00 01 resp.
    type Limits      = { LimitMin:U32; LimitMax:U32 option }

    type ValType     = I32Type | I64Type | F32Type | F64Type                  // 7F 7E 7D 7C resp.
    type BlockType   = EmptyBlockType | BlockValType of ValType
    type FuncType    = { ParameterTypes:ValType[]; ReturnTypes:ValType[] }
    type MemoryType  = { MemoryLimits:Limits }
    type ElementType = AnyFuncType
    type TableType   = { TableElementType:ElementType; TableLimits:Limits }
    type GlobalType  = { GlobalType:ValType; GlobalMutability:Mutability }

    type TypeIdx   = TypeIdx   of U32
    type FuncIdx   = FuncIdx   of U32  // Indexes into any ImportFuncs first, then the FuncSec
    type TableIdx  = TableIdx  of U32  // Indexes into any ImportTables first, then the TableSec
    type MemIdx    = MemIdx    of U32  // Indexes into any ImportMems first, then the MemSec
    type GlobalIdx = GlobalIdx of U32  // Indexes into any ImportGlobals first, then the GlobalSec
    type LocalIdx  = LocalIdx  of U32
    type LabelIdx  = LabelIdx  of U32

    type MemArg = { Align:U32; Offset:U32 } 

    type Instr =

        // 5.4.1  Control Instructions

        | Unreachable  // 00
        | Nop          // 01
        | Block        of BlockType * Instr array  // 02 0B
        | Loop         of BlockType * Instr array  // 03 0B
        | If           of BlockType * Instr array  // 04 0B
        | IfElse       of BlockType * If:Instr array * Else:Instr array  // 04 05 0B
        | Br           of LabelIdx  // 0C
        | BrIf         of LabelIdx  // 0D
        | BrTable      of LabelIdx array * LabelIdx  // 0E
        | Return
        | Call         of FuncIdx   // 10
        | CallIndirect of FuncType  // 11 00

        // 5.4.2  Parameteric Instructions

        | Drop    // 1A
        | Select  // 1B

        // 5.4.3  Variable Instructions

        | GetLocal  of LocalIdx    // 20
        | SetLocal  of LocalIdx    // 21
        | TeeLocal  of LocalIdx    // 22
        | GetGlobal of GlobalIdx   // 23
        | SetGlobal of GlobalIdx   // 24

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

    // 5.4.6  Expressions

    type InstructionArray = Instr[]

    type Name = string

    type ImportDesc = 
        | ImportFunc   of FuncType
        | ImportTable  of TableType
        | ImportMemory of MemoryType
        | ImportGlobal of GlobalType

    type ExportDesc = 
        | ExportFunc   of FuncIdx
        | ExportTable  of TableIdx
        | ExportMemory of MemIdx
        | ExportGlobal of GlobalIdx

    type Locals = { NumRepeats:U32; LocalsType:ValType }

    type Custom = { Name:Name; Data:byte array }
    type Import = { ImportModuleName:Name; ImportName:Name; ImportDesc:ImportDesc }
    type Func   = { Locals:Locals array; Body:InstructionArray }
    type Table  = { TableType:TableType }
    type Mem    = { MemType:MemoryType }
    type Global = { GlobalType:GlobalType; InitExpr:InstructionArray }
    type Export = { ExportName:Name; ExportDesc:ExportDesc }
    type Start  = { StartFuncIdx:FuncIdx }
    type Elem   = { TableIndex:TableIdx; OffsetExpr:InstructionArray; Init:FuncIdx array }
    type Code   = { CodeSize:U32; Function:Func }
    type Data   = { DataMemoryIndex:MemIdx; OffsetExpr:InstructionArray; InitImageBytes:byte array }

    type Magic   = U32
    type Version = U32

    type Module = {

        // The spec allows custom sections to appear
        // between all the regular sections.  The
        // following allows these to be preserved
        // if the file is saved out:

        Custom1:  Custom[]; Types:FuncType array;
        Custom2:  Custom[]; Imports:Import array;
        Custom3:  Custom[]; Funcs:FuncType array;
        Custom4:  Custom[]; Tables:Table array;
        Custom5:  Custom[]; Mems:Mem array;
        Custom6:  Custom[]; Globals:Global array;
        Custom7:  Custom[]; Exports:Export array;
        Custom8:  Custom[]; Start:Start option;
        Custom9:  Custom[]; Elems:Elem array;
        Custom10: Custom[]; Codes:Code array;
        Custom11: Custom[]; Datas:Data array;
        Custom12: Custom[] }

        
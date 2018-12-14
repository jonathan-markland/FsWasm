namespace FsWasmLibrary

open System

module Wasm =

    type I32 = WasmI32 of int
    type I64 = WasmI64 of int64
    type U32 = WasmU32 of uint32
    type F32 = WasmF32 of float32
    type F64 = WasmF64 of float

    type Mutability  = Const_00 | Var_01
    type Limits      = { LimitMin:U32; LimitMax:U32 option }

    type ValType     = I32Type | I64Type | F32Type | F64Type                  // 7F 7E 7D 7C resp.
    type BlockType   = EmptyBlockType | BlockValType of ValType
    type FuncType_60 = { ParameterTypes:ValType[]; ReturnTypes:ValType[] }
    type MemoryType  = { MemoryLimits:Limits }
    type ElementType = AnyFuncType
    type TableType   = { TableElementType:ElementType; TableLimits:Limits }
    type GlobalType  = { GlobalType:ValType; GlobalMutability:Mutability }

    type TypeIdx   = WasmTypeIdx of U32
    type FuncIdx   = WasmFuncIdx of U32
    type TableIdx  = WasmTableIdx of U32
    type MemIdx    = WasmMemIdx of U32
    type GlobalIdx = WasmGlobalIdx of U32
    type LocalIdx  = WasmLocalIdx of U32
    type LabelIdx  = WasmLabelIdx of U32

    type MemArg = { Align:U32; Offset:U32 } 

    type Instr =

        // 5.4.1  Control Instructions

        | Unreachable  // 00
        | Nop          // 01
        | Block        of t:BlockType * ins:Instr array  // 02 0B
        | Loop         of t:BlockType * ins:Instr array  // 03 0B
        | If           of t:BlockType * ins:Instr array  // 04 0B
        | IfElse       of t:BlockType * If:Instr array * Else:Instr array  // 04 05 0B
        | Br           of LabelIndex:LabelIdx  // 0C
        | BrIf         of LabelIndex:LabelIdx  // 0D
        | BrTable      of LabelIdx array * LabelIdx  // 0E
        | Return
        | Call         of FuncIdx   // 10
        | CallIndirect of TypeIdx   // 11 00

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
        | I32Lt_s 
        | I32Lt_u 
        | I32Gt_s 
        | I32Gt_u 
        | I32Le_s 
        | I32Le_u 
        | I32Ge_s 
        | I32Ge_u 

        | I64Eqz   // 50
        | I64Eq 
        | I64Ne 
        | I64Lt_s 
        | I64Lt_u 
        | I64Gt_s 
        | I64Gt_u 
        | I64Le_s 
        | I64Le_u 
        | I64Ge_s 
        | I64Ge_u 
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
        | I32Div_s 
        | I32Div_u 
        | I32Rem_s 

        | I32Rem_u    // 70
        | I32And 
        | I32Or 
        | I32Xor 
        | I32Shl_74
        | I32Shr_s_75 
        | I32Shr_u_76 
        | I32Rotl_77 
        | I32Rotr_78 
        | I64Clz_79 
        | I64Ctz_7A 
        | I64PopCnt_7B 
        | I64Add_7C 
        | I64Sub_7D 
        | I64Mul_7E 
        | I64Div_s_7F 
        | I64Div_u_80 
        | I64Rem_s_81 
        | I64Rem_u_82 
        | I64And_83 
        | I64Or_84 
        | I64Xor_85 
        | I64Shl_86 
        | I64Shr_s_87 
        | I64Shr_u_88 
        | I64Rotl_89 
        | I64Rotr_8A 
        | F32Abs_8B 
        | F32Neg_8C 
        | F32Ceil_8D 
        | F32Floor_8E 
        | F32Trunc_8F 
        | F32Nearest_90 
        | F32Sqrt_91 
        | F32Add_92 
        | F32Sub_93 
        | F32Mul_94 
        | F32Div_95 
        | F32Min_96 
        | F32Max_97 
        | F32CopySign_98 
        | F64Abs_99 
        | F64Neg_9A 
        | F64Ceil_9B 
        | F64Floor_9C 
        | F64Trunc_9D 
        | F64Nearest_9E 
        | F64Sqrt_9F 
        | F64Add_A0 
        | F64Sub_A1 
        | F64Mul_A2 
        | F64Div_A3 
        | F64Min_A4 
        | F64Max_A5 
        | F64CopySign_A6 
        | I32Wrap_I64_A7  
        | I32Trunc_s_F32_A8  
        | I32Trunc_u_F32_A9  
        | I32Trunc_s_F64_AA  
        | I32Trunc_u_F64_AB  
        | I64Extend_s_i32_AC  
        | I64Extend_u_i32_AD  
        | I64Trunc_s_F32_AE  
        | I64Trunc_u_F32_AF  
        | I64Trunc_s_F64_B0  
        | I64Trunc_u_F64_B1  
        | F32Convert_s_i32_B2  
        | F32Convert_u_i32_B3  
        | F32Convert_s_i64_B4  
        | F32Convert_u_i64_B5  
        | F32Demote_F64_B6  
        | F64Convert_s_i32_B7  
        | F64Convert_u_i32_B8  
        | F64Convert_s_i64_B9  
        | F64Convert_u_i64_BA  
        | F64Promote_F32_BB  
        | I32Reinterpret_F32_BC   
        | I64Reinterpret_F64_BD   
        | F32Reinterpret_i32_BE   
        | F64Reinterpret_i64_BF  

    // 5.4.6  Expressions

    type Expr_0B = Instr[]

    type Name = string

    type ImportDesc = 
        | ImpFunc_00   of TypeIdx
        | ImpTable_01  of TableType
        | ImpMem_02    of MemoryType
        | ImpGlobal_03 of GlobalType

    type ExportDesc = 
        | ExpFunc_00   of FuncIdx
        | ExpTable_01  of TableIdx
        | ExpMem_02    of MemIdx
        | ExpGlobal_03 of GlobalIdx

    type Locals = { NumRepeats:U32; LocalsType:ValType }

    type Custom = { Name:Name; Data:byte array }
    type Import = { Mod:Name; nm:Name; d:ImportDesc }
    type Func   = { Locals:Locals array; Body:Expr_0B }
    type Table  = { TableType:TableType }
    type Mem    = { MemType:MemoryType }
    type Global = { GlobalType:GlobalType; InitExpr:Expr_0B }
    type Export = { nm:Name; d:ExportDesc }
    type Start  = { StartFuncIdx:FuncIdx }
    type Elem   = { TableIndex:TableIdx; Offset:Expr_0B; Init:FuncIdx array }
    type Code   = { Size:U32; Code:Func }
    type Data   = { DataMemoryIndex:MemIdx; OffsetExpr:Expr_0B; InitImage:byte array }

    type CustomSec = WasmCustomSec of Custom
    type TypeSec   = WasmTypeSec   of FuncType_60 array
    type ImportSec = WasmImportSec of Import array
    type FuncSec   = WasmFuncSec   of TypeIdx array
    type TableSec  = WasmTableSec  of Table array
    type MemSec    = WasmMemSec    of Mem array
    type GlobalSec = WasmGlobalSec of Global array
    type ExportSec = WasmExportSec of Export array
    type StartSec  = WasmStartSec  of Start
    type ElemSec   = WasmElemSec   of Elem array
    type CodeSec   = WasmCodeSec   of Code array
    type DataSec   = WasmDataSec   of Data array

    type Magic   = U32
    type Version = U32

    type Module = {

        // The spec allows custom sections to appear
        // between all the regular sections.  The
        // following allows these to be preserved
        // if the file is saved out:

        Custom1:  CustomSec[]; Types:TypeSec option;
        Custom2:  CustomSec[]; Imports:ImportSec option;
        Custom3:  CustomSec[]; Funcs:FuncSec option;
        Custom4:  CustomSec[]; Tables:TableSec option;
        Custom5:  CustomSec[]; Mems:MemSec option;
        Custom6:  CustomSec[]; Globals:GlobalSec option;
        Custom7:  CustomSec[]; Exports:ExportSec option;
        Custom8:  CustomSec[]; Start:StartSec option;
        Custom9:  CustomSec[]; Elems:ElemSec option;
        Custom10: CustomSec[]; Codes:CodeSec option;
        Custom11: CustomSec[]; Datas:DataSec option;
        Custom12: CustomSec[] }

        
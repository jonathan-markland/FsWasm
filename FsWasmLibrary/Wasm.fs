namespace FsWasmLibrary

open System

module Wasm =

    type I32 = int
    type I64 = int64
    type U32 = uint32
    type F32 = float32
    type F64 = float

    type Mut = Const_00 | Var_01
    type Limits = { Min:U32; Max:U32 option }

    type ValType = I32_7F | I64_7E | F32_7D | F64_7C
    type BlockType = EmptyBlockType_40 | BlockValType of ValType
    type FuncType_60 = { FuncInputs: ValType list; FuncOutputs: ValType list }
    type MemType = { MemLim:Limits }
    type ElemType = AnyFunc_70
    type TableType = { ElementType:ElemType; TableLim:Limits }
    type GlobalType = { Type:ValType; Mutability:Mut }

    type TypeIdx = U32
    type FuncIdx = U32
    type TableIdx = U32
    type MemIdx = U32
    type GlobalIdx = U32
    type LocalIdx = U32
    type LabelIdx = U32

    type MemArg = { Align:U32; Offset:U32 } 

    type Instr =

        // 5.4.1  Control Instructions

        | Unreachable_00
        | Nop_01
        | Block_02_0B of t:BlockType * ins:Instr list
        | Loop_03_0B of t:BlockType * ins:Instr list
        | If_04_0B of t:BlockType * ins:Instr list
        | IfElse_04_05_0B of t:BlockType * If:Instr list * Else:Instr list
        | Br_0C of LabelIndex:LabelIdx
        | BrIf_0D of LabelIndex:LabelIdx
        | BrTable_0E of LabelIdx list * LabelIdx
        | Return_0F
        | Call_10 of FuncIdx
        | CallIndirect_11_00 of TypeIdx
        | SetLocal of U32
        | I32Load of flags:U32 * offset:U32

        // 5.4.2  Parameteric Instructions

        | Drop_1A
        | Select_1B

        // 5.4.3  Variable Instructions

        | GetLocal_20 of LocalIdx
        | SetLocal_21 of LocalIdx
        | TeeLocal_22 of LocalIdx
        | GetGlobal_23 of GlobalIdx
        | SetGlobal_24 of GlobalIdx

        // 5.4.4  Memory Instructions

        | I32Load_28 of MemArg
        | I64Load_29 of MemArg
        | F32Load_2A of MemArg
        | F64Load_2B of MemArg
        | I32Load8s_2C of MemArg
        | I32Load8u_2D of MemArg
        | I32Load16s_2E of MemArg
        | I32Load16u_2F of MemArg
        | I64Load8s_30 of MemArg
        | I64Load8u_31 of MemArg
        | I64Load16s_32 of MemArg
        | I64Load16u_33 of MemArg
        | I64Load32s_34 of MemArg
        | I64Load32u_35 of MemArg
        | I32Store_36 of MemArg
        | I64Store_37 of MemArg
        | F32Store_38 of MemArg
        | F64Store_39 of MemArg
        | I32Store8_3A of MemArg
        | I32Store16_3B of MemArg
        | I64Store8_3C of MemArg
        | I64Store16_3D of MemArg
        | I64Store32_3E of MemArg

        | MemorySize_3F_00
        | GrowMemory_40_00

        // 5.4.5  Numeric Instructions

        | I32Const_41 of I32
        | I64Const_42 of I64
        | F32Const_43 of F32
        | F64Const_44 of F64

        | I32Eqz_45 
        | I32Eq_46 
        | I32Ne_47 
        | I32Lt_s_48 
        | I32Lt_u_49 
        | I32Gt_s_4A 
        | I32Gt_u_4B 
        | I32Le_s_4C 
        | I32Le_u_4D 
        | I32Ge_s_4E 
        | I32Ge_u_4F 
        | I64Eqz_50 
        | I64Eq_51 
        | I64Ne_52 
        | I64Lt_s_53 
        | I64Lt_u_54 
        | I64Gt_s_55 
        | I64Gt_u_56 
        | I64Le_s_57 
        | I64Le_u_58 
        | I64Ge_s_59 
        | I64Ge_u_5A 
        | F32Eq_5B 
        | F32Ne_5C 
        | F32Lt_5D 
        | F32Gt_5E 
        | F32Le_5F 
        | F32Ge_60 
        | F64Eq_61 
        | F64Ne_62 
        | F64Lt_63 
        | F64Gt_64 
        | F64Le_65 
        | F64Ge_66 
        | I32Clz_67 
        | I32Ctz_68 
        | I32PopCnt_69 
        | I32Add_6A 
        | I32Sub_6B 
        | I32Mul_6C 
        | I32Div_s_6D 
        | I32Div_u_6E 
        | I32Rem_s_6F 
        | I32Rem_u_70 
        | I32And_71 
        | I32Or_72 
        | I32Xor_73 
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

    type Expr_0B = Instr list

    type Name = string

    type ImportDesc = 
        | ImpFunc_00 of TypeIdx
        | ImpTable_01 of TableType
        | ImpMem_02 of MemType
        | ImpGlobal_03 of GlobalType

    type ExportDesc = 
        | ExpFunc_00 of TypeIdx
        | ExpTable_01 of TableType
        | ExpMem_02 of MemType
        | ExpGlobal_03 of GlobalType

    type Locals = { n:U32; t:ValType }

    type Custom = { Name:Name; Data:byte array }
    type Import = { Mod:Name; nm:Name; d:ImportDesc }
    type Func = { Locals:Locals list; Body:Expr_0B }
    type Table = { TableType:TableType }
    type Mem = { MemType:MemType }
    type Global = { GlobalType:GlobalType; InitExpr:Expr_0B }
    type Export = { nm:Name; d:ExportDesc }
    type Start = { x:FuncIdx }
    type Elem = { TableIndex:TableIdx; Offset:Expr_0B; Init:FuncIdx list }
    type Code = { Size:U32; Code:Func }
    type Data = { DataMemoryIndex:MemIdx; OffsetExpr:Expr_0B; InitImage:byte array }

    type CustomSec = Custom
    type TypeSec = FuncType_60 list
    type ImportSec = Import list
    type FuncSec = TypeIdx list
    type TableSec = Table list
    type MemSec = Mem list
    type GlobalSec = Global list
    type ExportSec = Export list
    type StartSec = Start option
    type ElemSec = Elem list
    type CodeSec = Code list
    type DataSec = Data list

    (* type Section =
        | CustomSec_0 of CustomSec
        | TypeSec_1 of TypeSec
        | ImportSec_2 of ImportSec
        | FuncSec_3 of FuncSec
        | TableSec_4 of TableSec
        | MemSec_5 of MemSec
        | GlobalSec_6 of GlobalSec
        | ExportSec_7 of ExportSec
        | StartSec_8 of StartSec
        | ElemSec_9 of ElemSec
        | CodeSec_10 of CodeSec
        | DataSec_11 of DataSec *)

    type Magic = U32
    type Version = U32

    type Module = { Magic: Magic;
        Version: Version;

        // The spec allows custom sections to appear
        // between all the regular sections.  The
        // following allows these to be preserved
        // if the file is saved out:

        Custom1:  CustomSec list; Types:TypeSec;
        Custom2:  CustomSec list; Imports:ImportSec;
        Custom3:  CustomSec list; Funcs:FuncSec;
        Custom4:  CustomSec list; Tables:TableSec;
        Custom5:  CustomSec list; Mems:MemSec;
        Custom6:  CustomSec list; Globals:GlobalSec;
        Custom7:  CustomSec list; Exports:ExportSec;
        Custom8:  CustomSec list; Start:StartSec;
        Custom9:  CustomSec list; Elems:ElemSec;
        Custom10: CustomSec list; Codes:CodeSec;
        Custom11: CustomSec list; Datas:DataSec;
        Custom12: CustomSec list }

        
/// Types defined by the W3C WebAssembly commission.
/// These support WASM 1.0 (MVP) files.
module WasmFileTypes

open System

    type I32 = I32 of int
    type I64 = I64 of int64
    type U32 = U32 of uint32
    type F32 = F32 of float32
    type F64 = F64 of float

    type Mutability  = Constant | Variable   // 00 01 resp.
    type Limits      = { LimitMin:U32; LimitMax:U32 option }

    type ValType     = I32Type | I64Type | F32Type | F64Type   // 7F 7E 7D 7C resp.
    type BlockType   = EmptyBlockType | BlockValType of ValType
    type FuncType    = { ParameterTypes:ValType[]; ReturnTypes:ValType[] }  // TODO: use list?
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
        | Block        of BlockType * Instr list  // 02 0B
        | Loop         of BlockType * Instr list  // 03 0B
        | If           of BlockType * Instr list  // 04 0B
        | IfElse       of BlockType * If:Instr list * Else:Instr list  // 04 05 0B
        | Br           of LabelIdx  // 0C
        | BrIf         of Instr * LabelIdx  // 0D
        | BrTable      of Instr * LabelIdx array * LabelIdx  // 0E
        | Return
        | Call         of FuncIdx * Instr list  // 10
        | CallIndirect of FuncType * Instr list * indexExpr:Instr // 11 00

        // 5.4.2  Parameteric Instructions

        | Drop        of Instr                   // 1A
        | Select      of Instr * Instr * Instr   // 1B

        // 5.4.3  Variable Instructions

        | GetLocal    of LocalIdx            // 20
        | SetLocal    of LocalIdx * Instr    // 21
        | TeeLocal    of LocalIdx * Instr    // 22
        | GetGlobal   of GlobalIdx           // 23
        | SetGlobal   of GlobalIdx * Instr   // 24

        // 5.4.4  Memory Instructions

        | I32Load     of MemArg * Instr   // 28
        | I64Load     of MemArg * Instr
        | F32Load     of MemArg * Instr
        | F64Load     of MemArg * Instr
        | I32Load8s   of MemArg * Instr   // 2C
        | I32Load8u   of MemArg * Instr
        | I32Load16s  of MemArg * Instr
        | I32Load16u  of MemArg * Instr

        | I64Load8s   of MemArg * Instr   // 30
        | I64Load8u   of MemArg * Instr
        | I64Load16s  of MemArg * Instr
        | I64Load16u  of MemArg * Instr
        | I64Load32s  of MemArg * Instr   // 34
        | I64Load32u  of MemArg * Instr
        | I32Store    of MemArg * Instr * Instr
        | I64Store    of MemArg * Instr * Instr
        | F32Store    of MemArg * Instr * Instr    // 38
        | F64Store    of MemArg * Instr * Instr
        | I32Store8   of MemArg * Instr * Instr
        | I32Store16  of MemArg * Instr * Instr
        | I64Store8   of MemArg * Instr * Instr    // 3C
        | I64Store16  of MemArg * Instr * Instr
        | I64Store32  of MemArg * Instr * Instr

        | MemorySize    // 3F 00
        | GrowMemory    // 40 00

        // 5.4.5  Numeric Instructions

        | I32Const    of I32  // 41
        | I64Const    of I64  // 42
        | F32Const    of F32  // 43
        | F64Const    of F64  // 44

        | I32Eqz      of Instr 
        | I32Eq       of Instr * Instr
        | I32Ne       of Instr * Instr
        | I32Lts      of Instr * Instr
        | I32Ltu      of Instr * Instr
        | I32Gts      of Instr * Instr
        | I32Gtu      of Instr * Instr
        | I32Les      of Instr * Instr
        | I32Leu      of Instr * Instr
        | I32Ges      of Instr * Instr
        | I32Geu      of Instr * Instr

        | I64Eqz      of Instr  // 50
        | I64Eq       of Instr * Instr
        | I64Ne       of Instr * Instr
        | I64Lts      of Instr * Instr 
        | I64Ltu      of Instr * Instr 
        | I64Gts      of Instr * Instr 
        | I64Gtu      of Instr * Instr 
        | I64Les      of Instr * Instr 
        | I64Leu      of Instr * Instr 
        | I64Ges      of Instr * Instr 
        | I64Geu      of Instr * Instr 
        | F32Eq       of Instr * Instr
        | F32Ne       of Instr * Instr
        | F32Lt       of Instr * Instr
        | F32Gt       of Instr * Instr
        | F32Le       of Instr * Instr
        
        | F32Ge       of Instr * Instr // 60
        | F64Eq       of Instr * Instr
        | F64Ne       of Instr * Instr
        | F64Lt       of Instr * Instr
        | F64Gt       of Instr * Instr
        | F64Le       of Instr * Instr
        | F64Ge       of Instr * Instr
        | I32Clz      of Instr
        | I32Ctz      of Instr
        | I32PopCnt   of Instr
        | I32Add      of Instr * Instr
        | I32Sub      of Instr * Instr
        | I32Mul      of Instr * Instr
        | I32Divs     of Instr * Instr
        | I32Divu     of Instr * Instr
        | I32Rems     of Instr * Instr

        | I32Remu     of Instr * Instr   // 70
        | I32And      of Instr * Instr
        | I32Or       of Instr * Instr
        | I32Xor      of Instr * Instr
        | I32Shl      of Instr * Instr
        | I32Shrs     of Instr * Instr
        | I32Shru     of Instr * Instr
        | I32Rotl     of Instr * Instr
        | I32Rotr     of Instr * Instr
        | I64Clz      of Instr
        | I64Ctz      of Instr
        | I64PopCnt   of Instr
        | I64Add      of Instr * Instr
        | I64Sub      of Instr * Instr
        | I64Mul      of Instr * Instr
        | I64Divs     of Instr * Instr

        | I64Divu     of Instr * Instr  // 80
        | I64Rems     of Instr * Instr
        | I64Remu     of Instr * Instr
        | I64And      of Instr * Instr
        | I64Or       of Instr * Instr
        | I64Xor      of Instr * Instr
        | I64Shl      of Instr * Instr
        | I64Shrs     of Instr * Instr
        | I64Shru     of Instr * Instr
        | I64Rotl     of Instr * Instr
        | I64Rotr     of Instr * Instr
        | F32Abs      of Instr
        | F32Neg      of Instr
        | F32Ceil     of Instr
        | F32Floor    of Instr
        | F32Trunc    of Instr

        | F32Nearest  of Instr  // 90
        | F32Sqrt     of Instr
        | F32Add      of Instr * Instr
        | F32Sub      of Instr * Instr
        | F32Mul      of Instr * Instr
        | F32Div      of Instr * Instr
        | F32Min      of Instr * Instr
        | F32Max      of Instr * Instr
        | F32CopySign of Instr * Instr
        | F64Abs      of Instr
        | F64Neg      of Instr
        | F64Ceil     of Instr
        | F64Floor    of Instr
        | F64Trunc    of Instr
        | F64Nearest  of Instr
        | F64Sqrt     of Instr

        | F64Add        of Instr * Instr  // A0
        | F64Sub        of Instr * Instr
        | F64Mul        of Instr * Instr
        | F64Div        of Instr * Instr
        | F64Min        of Instr * Instr
        | F64Max        of Instr * Instr
        | F64CopySign   of Instr * Instr
        | I32WrapI64    of Instr
        | I32TruncsF32  of Instr
        | I32TruncuF32  of Instr
        | I32TruncsF64  of Instr
        | I32TruncuF64  of Instr
        | I64ExtendsI32 of Instr
        | I64ExtenduI32 of Instr
        | I64TruncsF32  of Instr
        | I64TruncuF32  of Instr

        | I64TruncsF64      of Instr // B0
        | I64TruncuF64      of Instr
        | F32ConvertsI32    of Instr
        | F32ConvertuI32    of Instr
        | F32ConvertsI64    of Instr
        | F32ConvertuI64    of Instr
        | F32DemoteF64      of Instr
        | F64ConvertsI32    of Instr
        | F64ConvertuI32    of Instr
        | F64ConvertsI64    of Instr
        | F64ConvertuI64    of Instr
        | F64PromoteF32     of Instr
        | I32ReinterpretF32 of Instr 
        | I64ReinterpretF64 of Instr 
        | F32ReinterpretI32 of Instr 
        | F64ReinterpretI64 of Instr

    // 5.4.6  Expressions

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
    type Func   = { Locals:Locals array; Body:Instr list }
    type Table  = { TableType:TableType }
    type Mem    = { MemType:MemoryType }
    type Global = { GlobalType:GlobalType; InitExpr:Instr list }
    type Export = { ExportName:Name; ExportDesc:ExportDesc }
    type Start  = { StartFuncIdx:FuncIdx }
    type Elem   = { TableIndex:TableIdx; OffsetExpr:Instr list; Init:FuncIdx[] }
    type Code   = { CodeSize:U32; Function:Func }
    type Data   = { DataMemoryIndex:MemIdx; OffsetExpr:Instr list; InitImageBytes:byte[] }

    type Magic   = U32
    type Version = U32

    type Module = {

        // The spec allows custom sections to appear
        // between all the regular sections.  The
        // following allows these to be preserved
        // if the file is saved out:

        Custom1:  Custom[];  Types   : FuncType array;
        Custom2:  Custom[];  Imports : Import array;
        Custom3:  Custom[];  Funcs   : FuncType array;
        Custom4:  Custom[];  Tables  : Table array;
        Custom5:  Custom[];  Mems    : Mem array;
        Custom6:  Custom[];  Globals : Global array;
        Custom7:  Custom[];  Exports : Export array;
        Custom8:  Custom[];  Start   : Start option;
        Custom9:  Custom[];  Elems   : Elem array;
        Custom10: Custom[];  Codes   : Code array;
        Custom11: Custom[];  Datas   : Data array;
        Custom12: Custom[] 
    }

        
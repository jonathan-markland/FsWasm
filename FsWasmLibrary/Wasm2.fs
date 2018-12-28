module Wasm2

open Wasm


type Export2 = { ExportName:Name; }
type Import2 = { Export:Export2 option; ImportModuleName:Name; ImportName:Name }


type ImportedFunction2Arg = { Import:Import2; FuncType:FuncType }
type InternalFunction2Arg = { Export:Export2 option; OriginalCodeSecIndex:U32; CodeSize:U32; FuncType:FuncType; Locals:ValType[]; Body:Instr[] }
type Function2 =
    | ImportedFunction2 of ImportedFunction2Arg
    | InternalFunction2 of InternalFunction2Arg


type ImportedGlobal2Arg = { Import:Import2; GlobalType:GlobalType }
type InternalGlobal2Arg = { Export:Export2 option; GlobalType:GlobalType; InitExpr:Instr[] }
type Global2 =
    | ImportedGlobal2 of ImportedGlobal2Arg
    | InternalGlobal2 of InternalGlobal2Arg


type ImportedMemory2Arg = { Import:Import2; MemoryType:MemoryType }
type InternalMemory2Arg = { Export:Export2 option; MemoryType:MemoryType; InitData:(Instr[] * byte array)[]; }
type Memory2 =
    | ImportedMemory2 of ImportedMemory2Arg
    | InternalMemory2 of InternalMemory2Arg


type ImportedTable2Arg  = { Import:Import2; TableType:TableType }
type InternalTable2Arg  = { Export:Export2 option; TableType:TableType; InitOffsetExpr:Instr[]; InitWith:FuncIdx array }
type Table2 =
    | ImportedTable2 of ImportedTable2Arg
    | InternalTable2 of InternalTable2Arg


type Module2 = {
    // NB: Decision made to drop custom sections, I could not, in general, interpret them anyway.
    Funcs:   Function2[];
    Mems:    Memory2[];
    Tables:  Table2[];
    Globals: Global2[];
    Start:   Start option; }


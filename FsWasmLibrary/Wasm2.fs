module Wasm2

open Wasm

type Export2 = { ExportName:Name; }
type Import2 = { Export2:Export2 option; ImportModuleName:Name; ImportName:Name }

// FUNCTIONS

type ImportedFunction2Arg = { Import2:Import2; FuncType:FuncType }
type InternalFunction2Arg = { Export2:Export2 option; OriginalCodeSecIndex:U32; CodeSize:U32; FuncType:FuncType; Locals:Locals[]; Body:Instr[] }

type Function2 =
    | ImportedFunction2 of ImportedFunction2Arg
    | InternalFunction2 of InternalFunction2Arg

// GLOBALS

type ImportedGlobal2Arg = { Import2:Import2; GlobalType:GlobalType }
type InternalGlobal2Arg = { Export2:Export2 option; GlobalType:GlobalType; InitExpr:Instr[] }

type Global2 =
    | ImportedGlobal2 of ImportedGlobal2Arg
    | InternalGlobal2 of InternalGlobal2Arg

// MEMORY

type ImportedMemory2Arg = { Import2:Import2; MemoryType:MemoryType }
type InternalMemory2Arg = { Export2:Export2 option; MemoryType:MemoryType; InitData:(Instr[] * byte array)[]; }

type Memory2 =
    | ImportedMemory2 of ImportedMemory2Arg
    | InternalMemory2 of InternalMemory2Arg

// TABLES

type ImportedTable2Arg  = { Import2:Import2; TableType:TableType }
type InternalTable2Arg  = { Export2:Export2 option; TableType:TableType; InitOffsetExpr:Instr[]; InitWith:FuncIdx array }

type Table2 =
    | ImportedTable2 of ImportedTable2Arg
    | InternalTable2 of InternalTable2Arg

// MODULE

type Module2 = {
    // NB: Decision made to drop custom sections, I could not, in general, interpret them anyway.
    funcs:Function2[];
    mems:Memory2[];
    tables:Table2[];
    globals:Global2[];
    start:Start option; }


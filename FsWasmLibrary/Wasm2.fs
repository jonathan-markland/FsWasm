module Wasm2

open WasmFileTypes


type Export2 = { ExportName:Name; }
type Import2 = { Export:Export2 option; ImportModuleName:Name; ImportName:Name }



type ImportedFunction2Record = { Import:Import2; FuncType:FuncType }
type InternalFunction2Record = { Export:Export2 option; OriginalCodeSecIndex:U32; CodeSize:U32; FuncType:FuncType; Locals:ValType[]; Body:Instr list }

type ImportedGlobal2Record = { Import:Import2; GlobalType:GlobalType }
type InternalGlobal2Record = { Export:Export2 option; GlobalType:GlobalType; InitExpr:Instr list }

type ImportedMemory2Record = { Import:Import2; MemoryType:MemoryType }
type InternalMemory2Record = { Export:Export2 option; MemoryType:MemoryType; InitData:(Instr list * byte array)[]; }

type ImportedTable2Record  = { Import:Import2; TableType:TableType }
type InternalTable2Record  = { Export:Export2 option; TableType:TableType; InitData:(Instr list * FuncIdx array) array }



type Function2 =
    | ImportedFunction2 of ImportedFunction2Record
    | InternalFunction2 of InternalFunction2Record

type Global2 =
    | ImportedGlobal2 of ImportedGlobal2Record
    | InternalGlobal2 of InternalGlobal2Record

type Memory2 =
    | ImportedMemory2 of ImportedMemory2Record
    | InternalMemory2 of InternalMemory2Record

type Table2 =
    | ImportedTable2 of ImportedTable2Record
    | InternalTable2 of InternalTable2Record



type Module2 = {
    // NB: Decision made to drop custom sections, I could not, in general, interpret them anyway.
    Funcs:   Function2[];
    Mems:    Memory2[];
    Tables:  Table2[];
    Globals: Global2[];
    Start:   Start option; }

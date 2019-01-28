module WasmBetterTypes

open WasmFileTypes


type Export = { ExportName:Name; }
type Import = { Export:Export option; ImportModuleName:Name; ImportName:Name }



type ImportedFunctionRecord = { Import:Import; FuncType:FuncType }
type InternalFunctionRecord = { Export:Export option; OriginalCodeSecIndex:U32; CodeSize:U32; FuncType:FuncType; Locals:ValType[]; Body:Instr list }

type ImportedGlobalRecord = { Import:Import; GlobalType:GlobalType }
type InternalGlobalRecord = { Export:Export option; GlobalType:GlobalType; InitExpr:Instr list }

type ImportedMemoryRecord = { Import:Import; MemoryType:MemoryType }
type InternalMemoryRecord = { Export:Export option; MemoryType:MemoryType; InitData:(Instr list * byte array)[]; }

type ImportedTableRecord  = { Import:Import; TableType:TableType }
type InternalTableRecord  = { Export:Export option; TableType:TableType; InitData:(Instr list * FuncIdx array) array }



type Function =
    | ImportedFunction2 of ImportedFunctionRecord
    | InternalFunction2 of InternalFunctionRecord

type Global =
    | ImportedGlobal2 of ImportedGlobalRecord
    | InternalGlobal2 of InternalGlobalRecord

type Memory =
    | ImportedMemory2 of ImportedMemoryRecord
    | InternalMemory2 of InternalMemoryRecord

type Table =
    | ImportedTable2 of ImportedTableRecord
    | InternalTable2 of InternalTableRecord



type Module = {
    // NB: Decision made to drop custom sections, I could not, in general, interpret them anyway.
    Funcs:   Function[];
    Mems:    Memory[];
    Tables:  Table[];
    Globals: Global[];
    Start:   Start option; }

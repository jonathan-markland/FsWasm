﻿/// This module is designed to be 'open'ed *after* WasmFileTypes.
/// Re-orders the file's data into a much more convenient format.
module WasmBetterTypes

open WasmFileTypes


type Export = { ExportName:Name; }
type Import = { Export:Export option; ImportModuleName:Name; ImportName:Name }



type ImportedFunctionRecord = { Import:Import; FuncType:FuncType }
type InternalFunctionRecord = { ModuleLocalFuncIdx:FuncIdx; Export:Export option; OriginalCodeSecIndex:U32; CodeSize:U32; FuncType:FuncType; Locals:ValType[]; Body:Instr list }

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



type Module = 
    {
        // NB: Decision made to drop custom sections, I could not, in general, interpret them anyway.
        Funcs   : Function[]
        Mems    : Memory[]
        Tables  : Table[]
        Globals : Global[]
        Start   : Function option
    }



/// This the metadata for the data section initialisation function.
/// It is needed to pass to the translation-to-target-cpu functions.
let TheInitialisationFunctionMetadata =
    { 
        ModuleLocalFuncIdx   = FuncIdx (U32 0u)   // TODO: Not right to lie about this, or re-use FuncIdx 0.
        Export = None
        OriginalCodeSecIndex = U32 0u // TODO: Not right to lie about this.  It doesn't exist in the original WASM file code sec!
        CodeSize             = U32 0u // TODO: Hopefully nobody ever reads this for this instance.
        FuncType =
            {
                ParameterTypes = [| |]  // No parameters
                ReturnTypes    = [| |]  // No return values
            }
        Locals = [| |]  // No locals
        Body = []       // The body is never used because this function is separately generated.
    }


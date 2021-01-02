module AsmPrefixes

let AsmImportedFuncNamePrefix = "wasm_host_"
let AsmInternalFuncNamePrefix = "wasm_fn"
let AsmGlobalNamePrefix       = "wasm_global"
let AsmLocalNamePrefix        = "loc"
let AsmCodeLabelPrefix        = "wasm_l"
let AsmEntryPointLabel        = "wasm_entry"
let AsmTableNamePrefix        = "wasm_table"  // There is only one, for the TableSec
let AsmMemoryNamePrefix       = "wasm_mem_init_data"
let AsmMemPrefix              = "wasm_mem"

open WasmFileTypes

let LocalIdxNameString (LocalIdx(U32 localIdx)) =    // TODO: There is a case to be made for memoizing these.
    sprintf "%s%d" AsmLocalNamePrefix localIdx

let GlobalIdxNameString (GlobalIdx(U32 globalIdx)) =     // TODO: There is a case to be made for memoizing these.
    sprintf "%s%d" AsmGlobalNamePrefix globalIdx
    
let FuncIdxNameString (FuncIdx(U32 funcIdx)) = 
    sprintf "%s%d" AsmInternalFuncNamePrefix funcIdx


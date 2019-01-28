module JasmPrefixes

let AsmImportedFuncNamePrefix   = "wasm_host_"
let AsmInternalFuncNamePrefix   = "wasm_fn"
let AsmGlobalNamePrefix = "wasm_global"
let AsmLocalNamePrefix  = "loc"
let AsmCodeLabelPrefix  = "wasm_l"
let AsmEntryPointLabel  = "wasm_entry"
let AsmTableNamePrefix  = "wasm_table"  // There is only one, for the TableSec
let AsmMemoryNamePrefix = "wasm_mem_init_data"
let AsmMemPrefix        = "wasm_mem"

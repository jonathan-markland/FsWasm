module WasmStaticExpressionEvaluator

open WasmFileTypes

let StaticEvaluate (instrs:Instr list) : int =

    match instrs.Length with // TODO: proper list matching
        | 0 -> failwith "Cannot statically evaluate an empty sequence of WASM instructions"
        | 1 -> ()
        | _ -> failwith "Cannot statically evaluate WASM instruction sequence because we only support a single instruction, and there is more than one"

    match instrs.[0] with
        | I32Const(I32 n) -> n
        | _ -> failwith "Cannot statically evaluate WASM instruction sequence -- unsupported single instruction"  // TODO: clarify




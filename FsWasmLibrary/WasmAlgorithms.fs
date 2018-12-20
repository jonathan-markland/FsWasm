namespace FsWasmLibrary

open Wasm

module WasmAlgorithms =

    type ConvenientLookupTables = { MasterFuncs:TypeIdx[]; MasterTables:Table[]; MasterMems:Mem[]; MasterGlobals:Global[] }

    let GetConvenientLookupTables (thisModule:Wasm.Module) =

        // 2.5.1  "The index space for functions, tables, memories and 
        //         globals includes respective imports declared in the 
        //         same module. The indices of these imports precede 
        //         the indices of other deﬁnitions in the same index space."

        // TypeIdxs  (function signatures)

        let typeIdxArray = 

            let a1 = 
                match thisModule.Imports with
                    | None -> [||]
                    | Some(ImportSec(importArray)) ->
                        importArray |> Array.choose (fun thisImport -> 
                            match thisImport with
                                | {ImportDesc=ImportFunc(x)} -> Some(x)
                                | _ -> None)

            let a2 = 
                match thisModule.Funcs with
                    | None -> [||]
                    | Some(FuncSec(xs)) -> xs

            Array.append a1 a2

        // Tables

        let tableArray = 

            let a1 = 
                match thisModule.Imports with
                    | None -> [||]
                    | Some(ImportSec(importArray)) ->
                        importArray |> Array.choose (fun thisImport -> 
                            match thisImport with
                                | {ImportDesc=ImportTable(x)} -> Some({TableType=x}) // TODO: didn't want this final re-wrapping
                                | _ -> None)

            let a2 = 
                match thisModule.Tables with
                    | None -> [||]
                    | Some(TableSec(xs)) -> xs

            Array.append a1 a2

        // Memory

        let memoryArray = 

            let a1 = 
                match thisModule.Imports with
                    | None -> [||]
                    | Some(ImportSec(importArray)) ->
                        importArray |> Array.choose (fun thisImport -> 
                            match thisImport with
                                | {ImportDesc=ImportMemory(x)} -> Some({MemType=x}) // TODO: didn't want this final re-wrapping
                                | _ -> None)

            let a2 = 
                match thisModule.Mems with
                    | None -> [||]
                    | Some(MemSec(xs)) -> xs

            Array.append a1 a2

        // Global

        let globalArray = 

            let a1 = 
                match thisModule.Imports with
                    | None -> [||]
                    | Some(ImportSec(importArray)) ->
                        importArray |> Array.choose (fun thisImport -> 
                            match thisImport with
                                | {ImportDesc=ImportGlobal(x)} -> Some({GlobalType=x; InitExpr=[||]}) // TODO: didn't want this final re-wrapping
                                | _ -> None)

            let a2 = 
                match thisModule.Globals with
                    | None -> [||]
                    | Some(GlobalSec(xs)) -> xs

            Array.append a1 a2

        // Return the above:

        { MasterFuncs=typeIdxArray; 
          MasterTables=tableArray; 
          MasterMems=memoryArray;
          MasterGlobals=globalArray }

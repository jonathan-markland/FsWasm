﻿/// Some support functions for WasmFileTypes
module WasmAlgorithms

open WasmFileTypes



type ConvenientLookupTables = { 
    MasterFuncs:FuncType[]; 
    MasterTables:Table[]; 
    MasterMems:Mem[]; 
    MasterGlobals:Global[] }



let GetConvenientTypeIdxArray thisModuleImports thisModuleFuncs =

    let importedFuncs = 
        thisModuleImports |> Array.choose (fun thisImport -> 
            match thisImport with
                | {ImportDesc=ImportFunc(x)} -> Some(x)
                | _ -> None)

    Array.append importedFuncs thisModuleFuncs







let GetConvenientLookupTables (thisModule:WasmFileTypes.Module) =

    // 2.5.1  "The index space for functions, tables, memories and 
    //         globals includes respective imports declared in the 
    //         same module. The indices of these imports precede 
    //         the indices of other deﬁnitions in the same index space."

    // TypeIdxs  (function signatures)

    let typeIdxArray = 
        GetConvenientTypeIdxArray thisModule.Imports thisModule.Funcs

    // Tables

    let tableArray = 

        let importedTables = 
            thisModule.Imports |> Array.choose (fun thisImport -> 
                match thisImport with
                    | {ImportDesc=ImportTable(x)} -> Some({TableType=x}) // TODO: didn't want this final re-wrapping
                    | _ -> None)

        Array.append importedTables thisModule.Tables

    // Memory

    let memoryArray = 

        let importedMemory = 
            thisModule.Imports |> Array.choose (fun thisImport -> 
                match thisImport with
                    | {ImportDesc=ImportMemory(x)} -> Some({MemType=x}) // TODO: didn't want this final re-wrapping
                    | _ -> None)

        Array.append importedMemory thisModule.Mems

    // Global

    let globalArray = 

        let importedGlobals = 
            thisModule.Imports |> Array.choose (fun thisImport -> 
                match thisImport with
                    | {ImportDesc=ImportGlobal(x)} -> Some({GlobalType=x; InitExpr=[]}) // TODO: didn't want this final re-wrapping
                    | _ -> None)

        Array.append importedGlobals thisModule.Globals

    // Return the above:

    { MasterFuncs=typeIdxArray; 
        MasterTables=tableArray; 
        MasterMems=memoryArray;
        MasterGlobals=globalArray }


module WasmFileReader

open Wasm
open WasmAlgorithms
open PrivateWasmFileReader



/// <summary>
/// Read the WASM file from the reader, and return a tree structure
/// representing the file, verbatim.  See also the Module function.
/// </summary>
let RawModule r =

    // Magic stamp:
    
    r |> ExpectByte 0uy
    r |> ExpectByte 97uy
    r |> ExpectByte 115uy
    r |> ExpectByte 109uy

    // Version 1:

    r |> ExpectByte 1uy
    r |> ExpectByte 0uy
    r |> ExpectByte 0uy
    r |> ExpectByte 0uy

    // Read sections:

    let sec1  = r |> CustomSecThenTrySpecificSection TypeSec 1 
    let sec2  = r |> CustomSecThenTrySpecificSection ImportSec 2
    let sec3  = r |> CustomSecThenTrySpecificSection FunctionSec 3
    let sec4  = r |> CustomSecThenTrySpecificSection TableSec 4
    let sec5  = r |> CustomSecThenTrySpecificSection MemSec 5
    let sec6  = r |> CustomSecThenTrySpecificSection GlobalSec 6
    let sec7  = r |> CustomSecThenTrySpecificSection ExportSec 7
    let sec8  = r |> CustomSecThenTrySpecificSectionOption StartSec 8
    let sec9  = r |> CustomSecThenTrySpecificSection ElementSec 9
    let sec10 = r |> CustomSecThenTrySpecificSection CodeSec 10
    let sec11 = r |> CustomSecThenTrySpecificSection DataSec 11
    let finalCustom = r |> CustomSecArray

    // Return loaded result:

    {   Custom1 = fst sec1; Types=snd sec1;
        Custom2 = fst sec2; Imports=snd sec2;
        Custom3 = fst sec3; Funcs=snd sec3;
        Custom4 = fst sec4; Tables=snd sec4;
        Custom5 = fst sec5; Mems=snd sec5;
        Custom6 = fst sec6; Globals=snd sec6;
        Custom7 = fst sec7; Exports=snd sec7;
        Custom8 = fst sec8; Start=snd sec8;
        Custom9 = fst sec9; Elems=snd sec9;
        Custom10 = fst sec10; Codes=snd sec10;
        Custom11 = fst sec11; Datas=snd sec11;
        Custom12 = finalCustom;
    }




/// <summary>
/// Read the WASM file from the reader, and return a tree structure
/// representing the file.  For convenience, the imports are
/// amalgamated into the Funcs, Tables, Mems and Globals, for 
/// convenient indexing per the WebAssembly spec.
/// </summary>
let Module r =

    let rawModule = r |> RawModule
    let newLists = rawModule |> GetConvenientLookupTables
    { rawModule with 
        Funcs = newLists.MasterFuncs;
        Tables = newLists.MasterTables; 
        Mems = newLists.MasterMems; 
        Globals = newLists.MasterGlobals }

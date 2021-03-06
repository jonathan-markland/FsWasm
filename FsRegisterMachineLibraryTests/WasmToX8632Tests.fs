﻿module WasmToX8632Tests

open Xunit
open SharedLibrary
open BWToCRMConfigurationTypes


let FilePassesTestX86 fileNumber optimiseMode entryPointConfig = 
    FilePassesTestWhenTranslatedUsing 
        "x8632" BetterWasmToX86Asm.WriteOutBetterWasmAsX86AssemblerText
        fileNumber optimiseMode entryPointConfig
            |> Assert.True


[<Fact>] 
let ``Program 1 to CRM optimised to X86 32`` () = 
    FilePassesTestX86 1 Optimised WasmStartEntryPointIfPresent

[<Fact>] 
let ``Program 2 to CRM optimised to X86 32`` () = 
    FilePassesTestX86 2 Optimised WasmStartEntryPointIfPresent

[<Fact>] 
let ``Program 3 to CRM optimised to X86 32`` () = 
    FilePassesTestX86 3 Optimised WasmStartEntryPointIfPresent

[<Fact>] 
let ``Program 4 to CRM optimised to X86 32`` () = 
    FilePassesTestX86 4 Optimised (ForceEntryPoint "main")

[<Fact>] 
let ``Program 5 to CRM optimised to X86 32`` () = 
    FilePassesTestX86 5 Optimised (ForceEntryPoint "main")

[<Fact>] 
let ``Program 6 to CRM optimised to X86 32`` () = 
    FilePassesTestX86 6 Optimised WasmStartEntryPointIfPresent

[<Fact>] 
let ``Program 7 to CRM optimised to X86 32`` () = 
    FilePassesTestX86 7 Optimised (ForceEntryPoint "main")


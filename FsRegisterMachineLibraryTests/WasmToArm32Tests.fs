﻿module WasmToArm32Tests

open Xunit
open SharedLibrary
open BWToCRMConfigurationTypes


let FilePassesTestARM = 
    FilePassesTestWhenTranslatedUsing 
        "arm32" BetterWasmToArm32Asm.WriteOutWasm2AsArm32AssemblerText


[<Fact>] 
let ``Program 1 to CRM optimised to ARM 32`` () = 
    Assert.True(FilePassesTestARM 1 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 2 to CRM optimised to ARM 32`` () = 
    Assert.True(FilePassesTestARM 2 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 3 to CRM optimised to ARM 32`` () = 
    Assert.True(FilePassesTestARM 3 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 4 to CRM optimised to ARM 32`` () = 
    Assert.True(FilePassesTestARM 4 Optimised (ForceEntryPoint "main"))

[<Fact>] 
let ``Program 5 to CRM optimised to ARM 32`` () = 
    Assert.True(FilePassesTestARM 5 Optimised (ForceEntryPoint "main"))

[<Fact>] 
let ``Program 6 to CRM optimised to ARM 32`` () = 
    Assert.True(FilePassesTestARM 6 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 7 to CRM optimised to ARM 32`` () = 
    Assert.True(FilePassesTestARM 7 Optimised (ForceEntryPoint "main"))



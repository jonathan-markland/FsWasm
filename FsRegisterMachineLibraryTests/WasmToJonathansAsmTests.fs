module WasmToJonathansAsmTests

// Note:  My assembler is something I developed in a completely separate project.

open Xunit
open SharedLibrary
open BWToCRMConfigurationTypes


let FilePassesTest = 
    FilePassesTestWhenTranslatedUsing 
        "jonathans" BetterWasmToJonathansAsm.WriteOutBetterWasmAsJonathansAssemblerText


[<Fact>] 
let ``Program 1 to CRM optimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 1 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 2 to CRM optimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 2 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 3 to CRM optimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 3 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 4 to CRM optimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 4 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 5 to CRM optimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 5 Optimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 6 to CRM optimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 6 Optimised WasmStartEntryPointIfPresent)



[<Fact>] 
let ``Program 1 to CRM unoptimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 1 Unoptimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 2 to CRM unoptimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 2 Unoptimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 3 to CRM unoptimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 3 Unoptimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 4 to CRM unoptimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 4 Unoptimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 5 to CRM unoptimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 5 Unoptimised WasmStartEntryPointIfPresent)

[<Fact>] 
let ``Program 6 to CRM unoptimised to Jonathans Asm`` () = 
    Assert.True(FilePassesTest 6 Unoptimised WasmStartEntryPointIfPresent)






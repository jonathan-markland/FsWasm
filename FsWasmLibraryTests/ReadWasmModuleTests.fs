module Tests

open Xunit
open System
open System.IO
open WasmToSerialised
open WasmSerialiser
open WasmFileReader



let serialisationMatchesOriginalWasmFile  wasmModulePath expectationTextFilePath =
    let fileName  = Path.GetFullPath wasmModulePath
    let fileImage = File.ReadAllBytes fileName
    let theReader = new BinaryReader(fileImage)
    let theSerialisationAsString = theReader |> ReadWasmModule |> ModuleToUnitTestString fileName
    let expectationFileAsString = File.ReadAllText expectationTextFilePath
    // WARNING: only uncomment to write out the serialisations.
    // File.WriteAllText(expectationTextFilePath, theSerialisationAsString) |> ignore
    String.Compare(expectationFileAsString, theSerialisationAsString, StringComparison.InvariantCulture) = 0




let filePassesTest n =
    let inputFile = (sprintf "program-%d.wasm" n)
    let expectationFile = (sprintf "program-%d-serialised.txt" n)
    serialisationMatchesOriginalWasmFile inputFile expectationFile
        |> Assert.True




[<Fact>]
let ``program (1) loads and serialises to the expected text`` () =
    filePassesTest 1

[<Fact>]
let ``program (2) loads and serialises to the expected text`` () =
    filePassesTest 2

[<Fact>]
let ``program (3) loads and serialises to the expected text`` () =
    filePassesTest 3

[<Fact>]
let ``program (4) loads and serialises to the expected text`` () =
    filePassesTest 4

[<Fact>]
let ``program (5) loads and serialises to the expected text`` () =
    filePassesTest 5

[<Fact>]
let ``program (6) loads and serialises to the expected text`` () =
    filePassesTest 6

[<Fact>]
let ``program (7) loads and serialises to the expected text`` () =
    filePassesTest 7

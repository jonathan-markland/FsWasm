module Tests

open Xunit
open System
open System.IO
open UnitTestSerialiser
open WasmSerialiser
open WasmFileReader



let serialisationMatchesOriginalWasmFile  wasmModulePath expectationTextFilePath =

    let fileName  = Path.GetFullPath wasmModulePath
    let fileImage = File.ReadAllBytes fileName
    let theReader = new BinaryReader(fileImage)
    let theSerialisation = theReader |> Module |> ModuleToUnitTestString fileName

    let expectationFileAsString = File.ReadAllText expectationTextFilePath

    String.Compare(expectationFileAsString, theSerialisation, StringComparison.InvariantCulture) = 0





[<Fact>]
let ``program (8) loads and serialises to the expected text`` () =
    Assert.True(serialisationMatchesOriginalWasmFile "program (8).wasm" "program (8).txt")




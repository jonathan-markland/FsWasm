
open System
open System.IO
open FsWasmLibrary.Wasm


let ReadByte (r:WasmSerialiser.BinaryReader) = 
    r.ReadByte()

let ReadNonLeb32 (r:WasmSerialiser.BinaryReader) =
    let v0 = uint32 (r.ReadByte())
    let v1 = uint32 (r.ReadByte())
    let v2 = uint32 (r.ReadByte())
    let v3 = uint32 (r.ReadByte())
    (v3 <<< 24) ||| (v2 <<< 16) ||| (v1 <<< 8) ||| v0

let ReadLeb32  (r:WasmSerialiser.BinaryReader) =
    r.ReadLebUnsigned32()

let ReadSectionHeader (r:WasmSerialiser.BinaryReader) =
    let sectionType = r.ReadByte()
    let sectionLength = r |> ReadLeb32
    (sectionType, sectionLength)

let ReadValType (r:WasmSerialiser.BinaryReader) =
    match r.ReadByte() with
        | 0x7Fuy -> I32_7F
        | 0x7Euy -> I64_7E
        | 0x7Duy -> F32_7D
        | 0x7Cuy -> F64_7C
        | _ -> failwith "Unrecognised valtype byte"  // TODO: elaborate

let ReadSequence (f:WasmSerialiser.BinaryReader -> 'a) (r:WasmSerialiser.BinaryReader) =
    let mutable elementCount = r |> ReadLeb32
    seq { while elementCount > 0u 
    do
        elementCount <- elementCount - 1u
        yield r |> f }

let ReadVector f r =
    Seq.toList (r |> ReadSequence f)

let ExpectByte theByte (r:WasmSerialiser.BinaryReader) =
    if r.ReadByte() = theByte 
    then ()
    else failwith "Missing byte"  // TODO: elaborate

let ReadFuncType r =
    r |> ExpectByte 0x60uy
    let funcInputs = r |> ReadVector ReadValType
    let funcOutputs = r |> ReadVector ReadValType
    { FuncInputs=funcInputs; FuncOutputs=funcOutputs }

let ReadTypeIdx r =
    r |> ReadLeb32

(* Sections *)

let ReadTypeSec r =
    r |> ReadVector ReadFuncType

let ReadFuncSec r =
    r |> ReadVector ReadTypeIdx


//let ReadTypeSec (r:WasmSerialiser.BinaryReader) =
//    match r.EndOfFile with
//        | true -> None
//        | false -> Some (r |> ReadSectionHeader)


[<EntryPoint>]
let main argv =

    let fileImage = File.ReadAllBytes("program (1).wasm");
    
    let theReader = new WasmSerialiser.BinaryReader(fileImage)

    let magic1 = theReader.ReadByte()
    let magic2 = theReader.ReadByte()
    let magic3 = theReader.ReadByte()
    let magic4 = theReader.ReadByte()
    let magic5 = theReader.ReadByte()
    let magic6 = theReader.ReadByte()
    let magic7 = theReader.ReadByte()
    let magic8 = theReader.ReadByte()

    let sec1 = theReader |> ReadSectionHeader
    let dat1 = theReader |> ReadTypeSec

    let sec3 = theReader |> ReadSectionHeader
    let dat3 = theReader |> ReadFuncSec

    printfn "Hello World from F#!"
    0 // return an integer exit code

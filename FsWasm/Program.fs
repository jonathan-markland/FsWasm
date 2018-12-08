
open System


[<EntryPoint>]
let main argv =

    let arr = Array.create<byte> 100 0uy

    let a = new WasmSerialiser.BinaryReader(arr)

    let v = a.ReadLebUnsigned32()

    printfn "Hello World from F#!"
    0 // return an integer exit code


open System.IO
open FsWasmLibrary.Wasm
open FsWasmLibrary.WasmAlgorithms

// Interfacing the BinaryReader:

let EOF         (r:WasmSerialiser.BinaryReader) = r.EndOfFile
let Byte        (r:WasmSerialiser.BinaryReader) = r.ReadByte()
let PeekByte    (r:WasmSerialiser.BinaryReader) = r.PeekByte()
let Leb32       (r:WasmSerialiser.BinaryReader) = r.ReadLebUnsigned32()
let Leb64       (r:WasmSerialiser.BinaryReader) = r.ReadLebUnsigned64()
let Float32     (r:WasmSerialiser.BinaryReader) = r.ReadFloat32()
let Float64     (r:WasmSerialiser.BinaryReader) = r.ReadDouble64()
let NameString  (r:WasmSerialiser.BinaryReader) = r.ReadString()
let Bytes       (r:WasmSerialiser.BinaryReader) = r.ReadByteVector()

// Interfacing the BinaryReader with Wasm type wrappers, for convenience:

let I32 r = I32(int (r |> Leb32))
let U32 r = U32(r |> Leb32)
let I64 r = I64(int64 (r |> Leb64))
let F32 r = F32(r |> Float32)
let F64 r = F64(r |> Float64)

// Generic parsing assistance

let ParseFailWith messageText byteThatWasRead (r:WasmSerialiser.BinaryReader) =
    failwith (sprintf "%s (%d) at file offset %d" messageText byteThatWasRead r.ReadOffset)

let ReadSequence (f:WasmSerialiser.BinaryReader -> 'a) r =
    let mutable elementCount = r |> Leb32
    seq { while elementCount > 0u 
    do
        elementCount <- elementCount - 1u
        yield r |> f }

let Vector f r = 
    Seq.toArray (r |> ReadSequence f)

let ExpectByte theByte (r:WasmSerialiser.BinaryReader) =
    if (r |> Byte) = theByte 
    then ()
    else failwith "Missing byte"  // TODO: elaborate all such messages

let rec ForEachRecognised readX actionToDo r =
    let optX = r |> readX
    match optX with
        | Some(x) -> 
            actionToDo x
            ForEachRecognised readX actionToDo r
        | None -> 
            ignore

let MakeArrayWhileSome (recordReaderFunc:'readerType -> 'recordType option) theReader =
    let container = new ResizeArray<'recordType>()
    ForEachRecognised recordReaderFunc (fun newRecord -> container.Add newRecord) theReader |> ignore
    container.ToArray()

// Read WASM indexes

let TypeIdx   r = TypeIdx  (r |> U32)
let FuncIdx   r = FuncIdx  (r |> U32)
let TableIdx  r = TableIdx (r |> U32)
let MemIdx    r = MemIdx   (r |> U32)
let GlobalIdx r = GlobalIdx(r |> U32)
let LocalIdx  r = LocalIdx (r |> U32)
let LabelIdx  r = LabelIdx (r |> U32)

// Wasm basic reading

let Limits r =
    let b = r |> Byte
    match b with
        | 0x00uy -> 
            let lmin = r |> U32
            { LimitMin=lmin; LimitMax=None }
        | 0x01uy ->
            let lmin = r |> U32
            let lmax = r |> U32
            { LimitMin=lmin; LimitMax=Some(lmax) }
        | _ -> ParseFailWith "Unknown limit type code byte" b r

let Mutability r =
    let b = r |> Byte
    match b with
        | 0x00uy -> Constant
        | 0x01uy -> Variable
        | _ -> ParseFailWith "Unknown mutability type code byte" b r

let ExportDesc r =
    let b = r |> Byte
    match b with
        | 0x00uy -> ExportFunc(r |> FuncIdx)
        | 0x01uy -> ExportTable(r |> TableIdx)
        | 0x02uy -> ExportMemory(r |> MemIdx)
        | 0x03uy -> ExportGlobal(r |> GlobalIdx)
        | _ -> ParseFailWith "Unrecognised ExportDesc code byte" b r

let ValType (r:WasmSerialiser.BinaryReader) =
    let b = r |> Byte
    match b with
        | 0x7Fuy -> I32Type
        | 0x7Euy -> I64Type
        | 0x7Duy -> F32Type
        | 0x7Cuy -> F64Type
        | _ -> ParseFailWith "Unrecognised valtype byte" b r

// Wasm file reading assistance

let SectionHeader (r:WasmSerialiser.BinaryReader) =
    let sectionType = r |> Byte
    let sectionLength = r |> Leb32
    (sectionType, sectionLength)

// Wasm Type reading

let MemArg (r:WasmSerialiser.BinaryReader) =
    let a = r |> U32
    let o = r |> U32
    { Align=a; Offset=o }

let MemType r =
    let l = r |> Limits
    { MemoryLimits=l }

let Mem r = 
    let t = r |> MemType
    { MemType=t }

let FuncType r =
    r |> ExpectByte 0x60uy
    let i = r |> Vector ValType
    let o = r |> Vector ValType
    { ParameterTypes=i; ReturnTypes=o }

let TableType r = 
    r |> ExpectByte 0x70uy   // There is only one kind defined.
    let l = r |> Limits
    { TableElementType=AnyFuncType; TableLimits=l }

let Table r =
    let t = r |> TableType
    { TableType=t }

let GlobalType r = 
    let t = r |> ValType
    let m = r |> Mutability
    { GlobalType=t; GlobalMutability=m }

let BlockType (r:WasmSerialiser.BinaryReader) =
    if r.PeekByte() = 0x40uy then 
        r.SkipByte()
        EmptyBlockType
    else
        let t = r |> ValType
        BlockValType(t)

let ImportDesc r =
    let b = r |> Byte
    match b with
        | 0x00uy -> ImportFunc(r |> TypeIdx)
        | 0x01uy -> ImportTable(r |> TableType)
        | 0x02uy -> ImportMemory(r |> MemType)
        | 0x03uy -> ImportGlobal(r |> GlobalType)
        | _ -> ParseFailWith "Unrecognised ImportDesc code byte" b r

let ExpectEnd r = 
    r |> ExpectByte 0x0Buy

// Instructions

let rec InstructionList r =
    r |> MakeArrayWhileSome Instruction

and Instruction (r:WasmSerialiser.BinaryReader) =

    let then00 (code, (r:WasmSerialiser.BinaryReader)) =
        r |> ExpectByte 0x00uy
        code

    let opcodeByte = r |> Byte

    match opcodeByte with

        | 0x00uy -> Some(Unreachable)

        | 0x01uy -> Some(Nop)

        | 0x02uy -> 
            let t = r |> BlockType
            let l = r |> InstructionList
            r |> ExpectEnd
            Some(Block(t, l))

        | 0x03uy -> 
            let t = r |> BlockType
            let l = r |> InstructionList
            r |> ExpectEnd
            Some(Loop(t, l))

        | 0x04uy -> 
            let blockType = r |> BlockType
            let ifInstructions = r |> InstructionList
            let endOrElse = r |> Byte   // 0x05 or 0x0B
            match endOrElse with
                | 0x05uy ->
                    let elseInstructions = r |> InstructionList
                    r |> ExpectEnd
                    Some(IfElse(blockType, ifInstructions, elseInstructions))
                | 0x0Buy ->
                    Some(If(blockType, ifInstructions))
                | _ ->
                    ParseFailWith "END or ELSE expected after IF instructions, unexpected byte" endOrElse r

        | 0x0Cuy -> Some(Br (r |> LabelIdx))
        | 0x0Duy -> Some(BrIf (r |> LabelIdx))

        | 0x0Euy -> 
            let v = r |> Vector LabelIdx
            let i = r |> LabelIdx
            Some(BrTable(v, i))

        | 0x0Fuy -> Some(Return)
        | 0x10uy -> Some(Call(r |> FuncIdx))
        | 0x11uy -> Some(CallIndirect(r |> TypeIdx))

        // 5.4.2  Parameteric Instructions

        | 0x1Auy -> Some(Drop)
        | 0x1Buy -> Some(Select)

        // 5.4.3  Variable Instructions

        | 0x20uy -> Some(GetLocal(r |> LocalIdx))
        | 0x21uy -> Some(SetLocal(r |> LocalIdx))
        | 0x22uy -> Some(TeeLocal(r |> LocalIdx))
        | 0x23uy -> Some(GetGlobal(r |> GlobalIdx))
        | 0x24uy -> Some(SetGlobal(r |> GlobalIdx))

        // 5.4.4  Memory Instructions

        | 0x28uy -> Some(I32Load(r |> MemArg))
        | 0x29uy -> Some(I64Load(r |> MemArg))
        | 0x2Auy -> Some(F32Load(r |> MemArg))
        | 0x2Buy -> Some(F64Load(r |> MemArg))
        | 0x2Cuy -> Some(I32Load8s(r |> MemArg))
        | 0x2Duy -> Some(I32Load8u(r |> MemArg))
        | 0x2Euy -> Some(I32Load16s(r |> MemArg))
        | 0x2Fuy -> Some(I32Load16u(r |> MemArg))
        | 0x30uy -> Some(I64Load8s(r |> MemArg))
        | 0x31uy -> Some(I64Load8u(r |> MemArg))
        | 0x32uy -> Some(I64Load16s(r |> MemArg))
        | 0x33uy -> Some(I64Load16u(r |> MemArg))
        | 0x34uy -> Some(I64Load32s(r |> MemArg))
        | 0x35uy -> Some(I64Load32u(r |> MemArg))
        | 0x36uy -> Some(I32Store(r |> MemArg))
        | 0x37uy -> Some(I64Store(r |> MemArg))
        | 0x38uy -> Some(F32Store(r |> MemArg))
        | 0x39uy -> Some(F64Store(r |> MemArg))
        | 0x3Auy -> Some(I32Store8(r |> MemArg))
        | 0x3Buy -> Some(I32Store16(r |> MemArg))
        | 0x3Cuy -> Some(I64Store8(r |> MemArg))
        | 0x3Duy -> Some(I64Store16(r |> MemArg))
        | 0x3Euy -> Some(I64Store32(r |> MemArg))

        | 0x3Fuy -> (Some(MemorySize), r) |> then00
        | 0x40uy -> (Some(GrowMemory), r) |> then00

        // 5.4.5  Numeric Instructions

        | 0x41uy -> Some(I32Const(r |> I32))
        | 0x42uy -> Some(I64Const(r |> I64))
        | 0x43uy -> Some(F32Const(r |> F32))
        | 0x44uy -> Some(F64Const(r |> F64))

        | 0x45uy -> Some(I32Eqz)
        | 0x46uy -> Some(I32Eq)
        | 0x47uy -> Some(I32Ne)
        | 0x48uy -> Some(I32Lts)
        | 0x49uy -> Some(I32Ltu)
        | 0x4Auy -> Some(I32Gts)
        | 0x4Buy -> Some(I32Gtu)
        | 0x4Cuy -> Some(I32Les)
        | 0x4Duy -> Some(I32Leu)
        | 0x4Euy -> Some(I32Ges)
        | 0x4Fuy -> Some(I32Geu)

        | 0x50uy -> Some(I64Eqz)
        | 0x51uy -> Some(I64Eq)
        | 0x52uy -> Some(I64Ne)
        | 0x53uy -> Some(I64Lts)
        | 0x54uy -> Some(I64Ltu)
        | 0x55uy -> Some(I64Gts)
        | 0x56uy -> Some(I64Gtu)
        | 0x57uy -> Some(I64Les)
        | 0x58uy -> Some(I64Leu)
        | 0x59uy -> Some(I64Ges)
        | 0x5Auy -> Some(I64Geu)
        | 0x5Buy -> Some(F32Eq)
        | 0x5Cuy -> Some(F32Ne)
        | 0x5Duy -> Some(F32Lt)
        | 0x5Euy -> Some(F32Gt)
        | 0x5Fuy -> Some(F32Le)

        | 0x60uy -> Some(F32Ge)
        | 0x61uy -> Some(F64Eq)
        | 0x62uy -> Some(F64Ne)
        | 0x63uy -> Some(F64Lt)
        | 0x64uy -> Some(F64Gt)
        | 0x65uy -> Some(F64Le)
        | 0x66uy -> Some(F64Ge)
        | 0x67uy -> Some(I32Clz)
        | 0x68uy -> Some(I32Ctz)
        | 0x69uy -> Some(I32PopCnt)
        | 0x6Auy -> Some(I32Add)
        | 0x6Buy -> Some(I32Sub)
        | 0x6Cuy -> Some(I32Mul)
        | 0x6Duy -> Some(I32Divs)
        | 0x6Euy -> Some(I32Divu)
        | 0x6Fuy -> Some(I32Rems)

        | 0x70uy -> Some(I32Remu)
        | 0x71uy -> Some(I32And)
        | 0x72uy -> Some(I32Or)
        | 0x73uy -> Some(I32Xor)
        | 0x74uy -> Some(I32Shl)
        | 0x75uy -> Some(I32Shrs)
        | 0x76uy -> Some(I32Shru)
        | 0x77uy -> Some(I32Rotl)
        | 0x78uy -> Some(I32Rotr)
        | 0x79uy -> Some(I64Clz)
        | 0x7Auy -> Some(I64Ctz)
        | 0x7Buy -> Some(I64PopCnt)
        | 0x7Cuy -> Some(I64Add)
        | 0x7Duy -> Some(I64Sub)
        | 0x7Euy -> Some(I64Mul)
        | 0x7Fuy -> Some(I64Divs)

        | 0x80uy -> Some(I64Divu)
        | 0x81uy -> Some(I64Rems)
        | 0x82uy -> Some(I64Remu)
        | 0x83uy -> Some(I64And)
        | 0x84uy -> Some(I64Or)
        | 0x85uy -> Some(I64Xor)
        | 0x86uy -> Some(I64Shl)
        | 0x87uy -> Some(I64Shrs)
        | 0x88uy -> Some(I64Shru)
        | 0x89uy -> Some(I64Rotl)
        | 0x8Auy -> Some(I64Rotr)
        | 0x8Buy -> Some(F32Abs)
        | 0x8Cuy -> Some(F32Neg)
        | 0x8Duy -> Some(F32Ceil)
        | 0x8Euy -> Some(F32Floor)
        | 0x8Fuy -> Some(F32Trunc)

        | 0x90uy -> Some(F32Nearest)
        | 0x91uy -> Some(F32Sqrt)
        | 0x92uy -> Some(F32Add)
        | 0x93uy -> Some(F32Sub)
        | 0x94uy -> Some(F32Mul)
        | 0x95uy -> Some(F32Div)
        | 0x96uy -> Some(F32Min)
        | 0x97uy -> Some(F32Max)
        | 0x98uy -> Some(F32CopySign)
        | 0x99uy -> Some(F64Abs)
        | 0x9Auy -> Some(F64Neg)
        | 0x9Buy -> Some(F64Ceil)
        | 0x9Cuy -> Some(F64Floor)
        | 0x9Duy -> Some(F64Trunc)
        | 0x9Euy -> Some(F64Nearest)
        | 0x9Fuy -> Some(F64Sqrt)

        | 0xA0uy -> Some(F64Add)
        | 0xA1uy -> Some(F64Sub)
        | 0xA2uy -> Some(F64Mul)
        | 0xA3uy -> Some(F64Div)
        | 0xA4uy -> Some(F64Min)
        | 0xA5uy -> Some(F64Max)
        | 0xA6uy -> Some(F64CopySign)
        | 0xA7uy -> Some(I32WrapI64)
        | 0xA8uy -> Some(I32TruncsF32)
        | 0xA9uy -> Some(I32TruncuF32)
        | 0xAAuy -> Some(I32TruncsF64)
        | 0xABuy -> Some(I32TruncuF64)
        | 0xACuy -> Some(I64ExtendsI32)
        | 0xADuy -> Some(I64ExtenduI32)
        | 0xAEuy -> Some(I64TruncsF32)
        | 0xAFuy -> Some(I64TruncuF32)

        | 0xB0uy -> Some(I64TruncsF64)
        | 0xB1uy -> Some(I64TruncuF64)
        | 0xB2uy -> Some(F32ConvertsI32)
        | 0xB3uy -> Some(F32ConvertuI32)
        | 0xB4uy -> Some(F32ConvertsI64)
        | 0xB5uy -> Some(F32ConvertuI64)
        | 0xB6uy -> Some(F32DemoteF64)
        | 0xB7uy -> Some(F64ConvertsI32)
        | 0xB8uy -> Some(F64ConvertuI32)
        | 0xB9uy -> Some(F64ConvertsI64)
        | 0xBAuy -> Some(F64ConvertuI64)
        | 0xBBuy -> Some(F64PromoteF32)
        | 0xBCuy -> Some(I32ReinterpretF32)
        | 0xBDuy -> Some(I64ReinterpretF64)
        | 0xBEuy -> Some(F32ReinterpretI32)
        | 0xBFuy -> Some(F64ReinterpretI64)
        
        | _ -> 
            r.Reverse(1u)
            None

let Expression r =
    let l = r |> InstructionList
    r |> ExpectEnd
    l

let Global r =
    let t = r |> GlobalType
    let e = r |> Expression
    { GlobalType=t; InitExpr=e }

let Export r = 
    let s = r |> NameString
    let d = r |> ExportDesc
    { ExportName=s; ExportDesc=d }

let LocalVariables r =
    let n = r |> U32
    let t = r |> ValType
    { NumRepeats=n; LocalsType=t }

let Function r = 
    let l = r |> Vector LocalVariables
    let e = r |> Expression
    { Locals=l; Body=e }

let Code r = 
    let s = r |> U32
    let f = r |> Function
    { CodeSize=s; Function=f }

let Import r = 
    let ms = r |> NameString
    let is = r |> NameString
    let id = r |> ImportDesc
    { ImportModuleName=ms; ImportName=is; ImportDesc=id }

let Element r =
    let i = r |> TableIdx
    let e = r |> Expression
    let v = r |> Vector FuncIdx
    { TableIndex=i; OffsetExpr=e; Init=v }

let Data r =
    let i = r |> MemIdx
    let e = r |> Expression
    let b = r |> Bytes
    { DataMemoryIndex=i; OffsetExpr=e; InitImageBytes=b }

// Read Wasm Sections

let CustomSec r =
    let s = r |> NameString
    let b = r |> Bytes
    { Name=s; Data=b }

let TypeSec     r = r |> Vector FuncType
let ImportSec   r = r |> Vector Import
let FunctionSec r = r |> Vector TypeIdx
let TableSec    r = r |> Vector Table
let MemSec      r = r |> Vector Mem
let GlobalSec   r = r |> Vector Global
let ExportSec   r = r |> Vector Export
let CodeSec     r = r |> Vector Code

let StartSec    r = 
    let i = r |> FuncIdx
    { StartFuncIdx=i }

let ElementSec  r = r |> Vector Element
let DataSec     r = r |> Vector Data

// Read section

let TryReadSpecificNumberedSection (sectionReader:WasmSerialiser.BinaryReader -> 'section[]) codeOfRequiredSection r : 'section[] =
    match EOF r with
        | true -> [||]
        | false ->
            let backtrackPos = r.ReadOffset
            let thisHeader = r |> SectionHeader
            match thisHeader with
                | (n, _) when n = byte codeOfRequiredSection -> 
                    r |> sectionReader
                | _ -> 
                    r.ReadOffset <- backtrackPos
                    [||]

let TryReadSpecificNumberedSectionOption sectionReader codeOfRequiredSection r =
    match EOF r with
        | true -> None
        | false ->
            let backtrackPos = r.ReadOffset
            let thisHeader = r |> SectionHeader
            match thisHeader with
                | (n, _) when n = byte codeOfRequiredSection -> 
                    Some(r |> sectionReader)
                | _ -> 
                    r.ReadOffset <- backtrackPos
                    None

let OptionalCustomSec r = 
    r |> TryReadSpecificNumberedSectionOption CustomSec 0

let CustomSecArray r =
    r |> MakeArrayWhileSome OptionalCustomSec

let CustomSecThenTrySpecificSection sectionReader codeOfRequiredSection r =
    let optionalCustomSecArray = r |> CustomSecArray
    let optionalSection = r |> TryReadSpecificNumberedSection sectionReader codeOfRequiredSection
    (optionalCustomSecArray, optionalSection)

let CustomSecThenTrySpecificSectionOption sectionReader codeOfRequiredSection r =
    let optionalCustomSecArray = r |> CustomSecArray
    let optionalSection = r |> TryReadSpecificNumberedSectionOption sectionReader codeOfRequiredSection
    (optionalCustomSecArray, optionalSection)

// Read module

let Module r =

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

// Main

[<EntryPoint>]
let main argv =

    let fileName = "program (4).wasm"
    let fileImage = File.ReadAllBytes fileName
    let r = new WasmSerialiser.BinaryReader(fileImage)
    let thisModule = r |> Module
    let convenientTables = thisModule |> GetConvenientLookupTables

    // Obtain the array that TypeIdx values really index.


    let unitTestString = UnitTestSerialiser.ModuleToUnitTestString fileName thisModule

    printfn "Hello World from F#!"
    0 // return an integer exit code

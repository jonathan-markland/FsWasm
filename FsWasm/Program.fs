
open System.IO
open FsWasmLibrary.Wasm

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

let I32 r = WasmI32(int (r |> Leb32))
let U32 r = WasmU32(r |> Leb32)
let I64 r = WasmI64(int64 (r |> Leb64))
let F32 r = WasmF32(r |> Float32)
let F64 r = WasmF64(r |> Float64)

// Generic parsing assistance

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

let TypeIdx   r = WasmTypeIdx  (r |> U32)
let FuncIdx   r = WasmFuncIdx  (r |> U32)
let TableIdx  r = WasmTableIdx (r |> U32)
let MemIdx    r = WasmMemIdx   (r |> U32)
let GlobalIdx r = WasmGlobalIdx(r |> U32)
let LocalIdx  r = WasmLocalIdx (r |> U32)
let LabelIdx  r = WasmLabelIdx (r |> U32)

// Wasm basic reading

let ParseFailWith msg b (r:WasmSerialiser.BinaryReader) =
    failwith (sprintf "%s (%d) at file offset %d" msg b r.ReadOffset)

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
        | 0x00uy -> Const_00
        | 0x01uy -> Var_01
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
    let memLimits = r |> Limits
    { MemoryLimits=memLimits }

let Mem r = 
    let memType = r |> MemType
    { MemType=memType }

let FuncType r =
    r |> ExpectByte 0x60uy
    let funcInputs = r |> Vector ValType
    let funcOutputs = r |> Vector ValType
    { ParameterTypes=funcInputs; ReturnTypes=funcOutputs }

let TableType r = 
    r |> ExpectByte 0x70uy   // There is only one kind defined.
    let tableLimits = r |> Limits
    { TableElementType=AnyFuncType; TableLimits=tableLimits }

let Table r =
    let tableType = r |> TableType
    { TableType=tableType }

let GlobalType r = 
    let globalType = r |> ValType
    let globalMutability = r |> Mutability
    { GlobalType=globalType; GlobalMutability=globalMutability }

let BlockType (r:WasmSerialiser.BinaryReader) =
    if r.PeekByte() = 0x40uy then 
        r.SkipByte()
        EmptyBlockType
    else
        let valType = r |> ValType
        BlockValType(valType)

let ImportDesc r =
    let b = r |> Byte
    match b with
        | 0x00uy -> ImportFunc(r |> TypeIdx)
        | 0x01uy -> ImportTable(r |> TableType)
        | 0x02uy -> ImportMemory(r |> MemType)
        | 0x03uy -> ImportGlobal(r |> GlobalType)
        | _ -> ParseFailWith "Unrecognised ImportDesc code byte" b r

// Instructions

let rec InstructionList r =

    r |> MakeArrayWhileSome Instruction

and Instruction (r:WasmSerialiser.BinaryReader) =

    let then00 (code, (r:WasmSerialiser.BinaryReader)) =
        r |> ExpectByte 0x00uy
        code

    let expectEnd r = r |> ExpectByte 0x0Buy
    let opcodeByte = r |> Byte

    match opcodeByte with

        | 0x00uy -> Some(Unreachable)

        | 0x01uy -> Some(Nop)

        | 0x02uy -> 
            let blockType = r |> BlockType
            let subBlock = r |> InstructionList
            r |> expectEnd
            Some(Block(blockType, subBlock))

        | 0x03uy -> 
            let blockType = r |> BlockType
            let subBlock = r |> InstructionList
            r |> expectEnd
            Some(Loop(blockType, subBlock))

        | 0x04uy -> 
            let blockType = r |> BlockType
            let ifInstructions = r |> InstructionList
            let endOrElse = r |> Byte   // 0x05 or 0x0B
            match endOrElse with
                | 0x05uy ->
                    let elseInstructions = r |> InstructionList
                    r |> expectEnd
                    Some(IfElse(blockType, ifInstructions, elseInstructions))
                | 0x0Buy ->
                    Some(If(blockType, ifInstructions))
                | _ ->
                    ParseFailWith "END or ELSE expected after IF instructions, unexpected byte" endOrElse r

        | 0x0Cuy -> Some(Br (r |> LabelIdx))
        | 0x0Duy -> Some(BrIf (r |> LabelIdx))

        | 0x0Euy -> 
            let tableContent = r |> Vector LabelIdx
            let labelIdx = r |> LabelIdx
            Some(BrTable(tableContent, labelIdx))

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
        | 0x48uy -> Some(I32Lt_s)
        | 0x49uy -> Some(I32Lt_u)
        | 0x4Auy -> Some(I32Gt_s)
        | 0x4Buy -> Some(I32Gt_u)
        | 0x4Cuy -> Some(I32Le_s)
        | 0x4Duy -> Some(I32Le_u)
        | 0x4Euy -> Some(I32Ge_s)
        | 0x4Fuy -> Some(I32Ge_u)

        | 0x50uy -> Some(I64Eqz)
        | 0x51uy -> Some(I64Eq)
        | 0x52uy -> Some(I64Ne)
        | 0x53uy -> Some(I64Lt_s)
        | 0x54uy -> Some(I64Lt_u)
        | 0x55uy -> Some(I64Gt_s)
        | 0x56uy -> Some(I64Gt_u)
        | 0x57uy -> Some(I64Le_s)
        | 0x58uy -> Some(I64Le_u)
        | 0x59uy -> Some(I64Ge_s)
        | 0x5Auy -> Some(I64Ge_u)
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
        | 0x6Duy -> Some(I32Div_s)
        | 0x6Euy -> Some(I32Div_u)
        | 0x6Fuy -> Some(I32Rem_s)

        | 0x70uy -> Some(I32Rem_u)
        | 0x71uy -> Some(I32And)
        | 0x72uy -> Some(I32Or)
        | 0x73uy -> Some(I32Xor)
        | 0x74uy -> Some(I32Shl)
        | 0x75uy -> Some(I32Shr_s)
        | 0x76uy -> Some(I32Shr_u)
        | 0x77uy -> Some(I32Rotl)
        | 0x78uy -> Some(I32Rotr)
        | 0x79uy -> Some(I64Clz)
        | 0x7Auy -> Some(I64Ctz)
        | 0x7Buy -> Some(I64PopCnt)
        | 0x7Cuy -> Some(I64Add)
        | 0x7Duy -> Some(I64Sub)
        | 0x7Euy -> Some(I64Mul)
        | 0x7Fuy -> Some(I64Div_s)

        | 0x80uy -> Some(I64Div_u)
        | 0x81uy -> Some(I64Rem_s)
        | 0x82uy -> Some(I64Rem_u)
        | 0x83uy -> Some(I64And)
        | 0x84uy -> Some(I64Or)
        | 0x85uy -> Some(I64Xor)
        | 0x86uy -> Some(I64Shl)
        | 0x87uy -> Some(I64Shr_s)
        | 0x88uy -> Some(I64Shr_u)
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
        | 0xA7uy -> Some(I32Wrap_I64)
        | 0xA8uy -> Some(I32Trunc_s_F32)
        | 0xA9uy -> Some(I32Trunc_u_F32)
        | 0xAAuy -> Some(I32Trunc_s_F64)
        | 0xABuy -> Some(I32Trunc_u_F64)
        | 0xACuy -> Some(I64Extend_s_i32)
        | 0xADuy -> Some(I64Extend_u_i32)
        | 0xAEuy -> Some(I64Trunc_s_F32)
        | 0xAFuy -> Some(I64Trunc_u_F32)

        | 0xB0uy -> Some(I64Trunc_s_F64)
        | 0xB1uy -> Some(I64Trunc_u_F64)
        | 0xB2uy -> Some(F32Convert_s_i32)
        | 0xB3uy -> Some(F32Convert_u_i32)
        | 0xB4uy -> Some(F32Convert_s_i64)
        | 0xB5uy -> Some(F32Convert_u_i64)
        | 0xB6uy -> Some(F32Demote_F64)
        | 0xB7uy -> Some(F64Convert_s_i32)
        | 0xB8uy -> Some(F64Convert_u_i32)
        | 0xB9uy -> Some(F64Convert_s_i64)
        | 0xBAuy -> Some(F64Convert_u_i64)
        | 0xBBuy -> Some(F64Promote_F32)
        | 0xBCuy -> Some(I32Reinterpret_F32)
        | 0xBDuy -> Some(I64Reinterpret_F64)
        | 0xBEuy -> Some(F32Reinterpret_i32)
        | 0xBFuy -> Some(F64Reinterpret_i64)
        
        | _ -> 
            r.Reverse(1u)
            None

let Expression r =
    let instrList = r |> InstructionList
    r |> ExpectByte 0x0Buy
    instrList

let Global r =
    let globalType = r |> GlobalType
    let initialisationExpression = r |> Expression
    { GlobalType=globalType; InitExpr=initialisationExpression }

let Export r = 
    let exportName = r |> NameString
    let exportDesc = r |> ExportDesc
    { ExportName=exportName; ExportDesc=exportDesc }

let LocalVariables r =
    let numRepeats = r |> U32
    let theType = r |> ValType
    { NumRepeats=numRepeats; LocalsType=theType }

let Function r = 
    let theLocals = r |> Vector LocalVariables
    let theExpr = r |> Expression
    { Locals=theLocals; Body=theExpr }

let Code r = 
    let codeSize = r |> U32
    let theFunc = r |> Function
    { Size=codeSize; Code=theFunc }

let Import r = 
    let moduleName = r |> NameString
    let importName = r |> NameString
    let importDesc = r |> ImportDesc
    { ImportModule=moduleName; ImportName=importName; ImportDesc=importDesc }

let Element r =
    let tableIdx = r |> TableIdx
    let elemExpr = r |> Expression
    let funcIdxArray = r |> Vector FuncIdx
    { TableIndex=tableIdx; Offset=elemExpr; Init=funcIdxArray }

let Data r =
    let memIdx = r |> MemIdx
    let offsetExpr = r |> Expression
    let dataBytes = r |> Bytes
    { DataMemoryIndex=memIdx; OffsetExpr=offsetExpr; InitImage=dataBytes }

// Read Wasm Sections

let CustomSec r = 
    let customName = r |> NameString
    let customBytes = r |> Bytes
    WasmCustomSec({ Name=customName; Data=customBytes })

let TypeSec     r = WasmTypeSec(r |> Vector FuncType)
let ImportSec   r = WasmImportSec(r |> Vector Import)
let FunctionSec r = WasmFuncSec(r |> Vector TypeIdx)
let TableSec    r = WasmTableSec(r |> Vector Table)
let MemSec      r = WasmMemSec(r |> Vector Mem)
let GlobalSec   r = WasmGlobalSec(r |> Vector Global)
let ExportSec   r = WasmExportSec(r |> Vector Export)
let CodeSec     r = WasmCodeSec(r |> Vector Code)

let StartSec    r = 
    let startFuncIdx = r |> FuncIdx
    WasmStartSec({ StartFuncIdx=startFuncIdx })

let ElementSec  r = WasmElemSec(r |> Vector Element)
let DataSec     r = WasmDataSec(r |> Vector Data)

// Read section

let TryReadSpecificNumberedSection sectionReader codeOfRequiredSection r =
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
    r |> TryReadSpecificNumberedSection CustomSec 0

let CustomSecArray r =
    r |> MakeArrayWhileSome OptionalCustomSec

let CustomSecThenTrySpecificSection sectionReader codeOfRequiredSection r =
    let optionalCustomSecArray = r |> CustomSecArray
    let optionalSection = r |> TryReadSpecificNumberedSection sectionReader codeOfRequiredSection
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
    let sec8  = r |> CustomSecThenTrySpecificSection StartSec 8
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

    let fileImage = File.ReadAllBytes("program (1).wasm");
    let r = new WasmSerialiser.BinaryReader(fileImage)
    let thisModule = r |> Module

    printfn "Hello World from F#!"
    0 // return an integer exit code

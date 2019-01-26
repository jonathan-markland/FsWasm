module PrivateWasmFileReader

open Wasm

// Interfacing the BinaryReader:

type BinaryReaderAndContext = { Reader:WasmSerialiser.BinaryReader; TypeSec:FuncType[]; ConvenientFuncTypeArray:FuncType[] }

let EOF         (r:BinaryReaderAndContext) = r.Reader.EndOfFile
let Byte        (r:BinaryReaderAndContext) = r.Reader.ReadByte()
let PeekByte    (r:BinaryReaderAndContext) = r.Reader.PeekByte()
let SkipByte    (r:BinaryReaderAndContext) = r.Reader.SkipByte()
let LebUns32    (r:BinaryReaderAndContext) = r.Reader.ReadLebUnsigned32()
let LebUns64    (r:BinaryReaderAndContext) = r.Reader.ReadLebUnsigned64()
let LebSig32    (r:BinaryReaderAndContext) = r.Reader.ReadLebSigned32()
let LebSig64    (r:BinaryReaderAndContext) = r.Reader.ReadLebSigned64()
let Float32     (r:BinaryReaderAndContext) = r.Reader.ReadFloat32()
let Float64     (r:BinaryReaderAndContext) = r.Reader.ReadDouble64()
let NameString  (r:BinaryReaderAndContext) = r.Reader.ReadString()
let Bytes       (r:BinaryReaderAndContext) = r.Reader.ReadByteVector()
let ReadOffset  (r:BinaryReaderAndContext) = r.Reader.ReadOffset
let SetReadOffset filePos (r:BinaryReaderAndContext) = r.Reader.ReadOffset <- filePos
let Reverse amount (r:BinaryReaderAndContext) = r.Reader.Reverse amount

// Interfacing the BinaryReader with Wasm type wrappers, for convenience:

let I32 r = I32(r |> LebSig32)
let U32 r = U32(r |> LebUns32)
let I64 r = I64(r |> LebSig64)
let F32 r = F32(r |> Float32)
let F64 r = F64(r |> Float64)

// Generic parsing assistance

let ParseFailWith messageText byteThatWasRead r =
    failwith (sprintf "%s (%d) at file offset %d" messageText byteThatWasRead (r |> ReadOffset))

let ReadSequence (f:BinaryReaderAndContext -> 'a) r =
    let mutable elementCount = r |> LebUns32
    seq { while elementCount > 0u 
    do
        elementCount <- elementCount - 1u
        yield r |> f }

let Vector f r = 
    Seq.toArray (r |> ReadSequence f)

let ExpectByte theByte r =
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
    ForEachRecognised recordReaderFunc (
        fun newRecord -> container.Add newRecord) theReader |> ignore
    container.ToArray()

// Read WASM indexes

let TypeIdx   r = 
    let typeIndex = r |> U32
    r.TypeSec.[match typeIndex with U32(x) -> (int x)]  // TODO: bound check with better error.

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

let ValType r =
    let b = r |> Byte
    match b with
        | 0x7Fuy -> I32Type
        | 0x7Euy -> I64Type
        | 0x7Duy -> F32Type
        | 0x7Cuy -> F64Type
        | _ -> ParseFailWith "Unrecognised valtype byte" b r

// Wasm file reading assistance

let SectionHeader r =
    let sectionType = r |> Byte
    let sectionLength = r |> LebUns32
    (sectionType, sectionLength)

// Wasm Type reading

let MemArg r =
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

let BlockType r =
    if (r |> PeekByte) = 0x40uy then 
        r |> SkipByte
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

let rec InstructionList (r:BinaryReaderAndContext) =

    // Returns a list of instructions (effectively as list of 
    // trees like a WAT file), in program order.

    List.rev (r |> ReversedInstructionList)

and ReversedInstructionList (r:BinaryReaderAndContext) =

    // When building an F# list from a file, it naturally reversed
    // the order when you traverse the list later.  This means the
    // most recent instructions are at the head of such a list.
    // It's useful to have the most recent instructions at the head
    // because we can easily pull them out as parameters when tree-
    // building.  Once built, a sub-tree immediately becomes the
    // new head of this list.

    r |> ReversedInstructionList2 []

and ReversedInstructionList2 previousInstructions r =

    match r |> Instruction previousInstructions with
        | Some(newInstructions) -> r |> ReversedInstructionList2 newInstructions
        | None -> previousInstructions

and Instruction recent r =

    let numParametersOfFuncType (ft:FuncType) =
        (ft.ParameterTypes.Length)

    let numParametersOfFunction (f:FuncIdx) =
        numParametersOfFuncType r.ConvenientFuncTypeArray.[match f with FuncIdx(U32(i)) -> int i]

    let then00 (code, r) =
        r |> ExpectByte 0x00uy
        code

    let first  () = List.head recent
    let second () = List.head (List.tail recent)
    let tail1  () = List.tail recent
    let tail2  () = List.tail (List.tail recent)

    let opcodeByte = r |> Byte

    match opcodeByte with

        | 0x00uy -> Some(Unreachable::recent)

        | 0x01uy -> Some(Nop::recent)

        | 0x02uy ->
            let t = r |> BlockType
            let l = r |> InstructionList
            r |> ExpectEnd
            Some(Block(t, l)::recent)

        | 0x03uy -> 
            let t = r |> BlockType
            let l = r |> InstructionList
            r |> ExpectEnd
            Some(Loop(t, l)::recent)

        | 0x04uy -> 
            let blockType      = r |> BlockType
            let ifInstructions = r |> InstructionList
            let endOrElse      = r |> Byte   // 0x05 or 0x0B
            match endOrElse with
                | 0x05uy ->
                    let elseInstructions = r |> InstructionList
                    r |> ExpectEnd
                    Some(IfElse(blockType, ifInstructions, elseInstructions)::recent)
                | 0x0Buy ->
                    Some(If(blockType, ifInstructions)::recent)
                | _ ->
                    ParseFailWith "END or ELSE expected after IF instructions, unexpected byte" endOrElse r

        | 0x0Cuy -> Some(Br (r |> LabelIdx)::recent)
        | 0x0Duy -> Some(BrIf (first(), r |> LabelIdx)::tail1())

        | 0x0Euy -> 
            let indexExpression = first()
            let vectorOfIndices = r |> Vector LabelIdx
            let defaultIndex    = r |> LabelIdx
            Some(BrTable(indexExpression, vectorOfIndices, defaultIndex)::tail1())

        | 0x0Fuy -> Some(Return::recent)

        | 0x10uy -> 
            let f = r |> FuncIdx
            let numParameters = numParametersOfFunction f
            let instrsAfterParameters = recent |> List.skip numParameters
            let paramsForCall = List.rev (recent |> List.take numParameters)
            Some(Call(f,paramsForCall)::instrsAfterParameters)

        | 0x11uy -> 
            let t = r |> TypeIdx // TODO: has bad name!
            r |> ExpectByte 0x00uy
            let numParameters   = numParametersOfFuncType t
            let instrsAfterCall = recent |> List.skip (numParameters + 1)
            let indexExpr       = recent |> List.head
            let paramsForCall   = List.rev (recent |> List.skip 1 |> List.take numParameters)  // TODO: ideally not need list reversing
            Some(CallIndirect(t,paramsForCall,indexExpr)::instrsAfterCall)

        // 5.4.2  Parameteric Instructions

        | 0x1Auy -> Some(Drop(first())::tail1())

        | 0x1Buy -> 
            match recent with
                | cond::b::a::tail -> Some(Select(a,b,cond)::tail)
                | _ -> failwith "Cannot translate WASM select instruction because of insufficient operand expressions"

        // 5.4.3  Variable Instructions

        | 0x20uy -> Some(GetLocal(r |> LocalIdx)::recent)
        | 0x21uy -> Some(SetLocal(r |> LocalIdx, first())::tail1())
        | 0x22uy -> Some(TeeLocal(r |> LocalIdx, first())::tail1())
        | 0x23uy -> Some(GetGlobal(r |> GlobalIdx)::recent)
        | 0x24uy -> Some(SetGlobal(r |> GlobalIdx, first())::tail1())

        // 5.4.4  Memory Instructions

        | 0x28uy -> Some(I32Load(r |> MemArg, first())::tail1())
        | 0x29uy -> Some(I64Load(r |> MemArg, first())::tail1())
        | 0x2Auy -> Some(F32Load(r |> MemArg, first())::tail1())
        | 0x2Buy -> Some(F64Load(r |> MemArg, first())::tail1())
        | 0x2Cuy -> Some(I32Load8s(r |> MemArg, first())::tail1())
        | 0x2Duy -> Some(I32Load8u(r |> MemArg, first())::tail1())
        | 0x2Euy -> Some(I32Load16s(r |> MemArg, first())::tail1())
        | 0x2Fuy -> Some(I32Load16u(r |> MemArg, first())::tail1())
        | 0x30uy -> Some(I64Load8s(r |> MemArg, first())::tail1())
        | 0x31uy -> Some(I64Load8u(r |> MemArg, first())::tail1())
        | 0x32uy -> Some(I64Load16s(r |> MemArg, first())::tail1())
        | 0x33uy -> Some(I64Load16u(r |> MemArg, first())::tail1())
        | 0x34uy -> Some(I64Load32s(r |> MemArg, first())::tail1())
        | 0x35uy -> Some(I64Load32u(r |> MemArg, first())::tail1())

        | 0x36uy -> Some(I32Store(r |> MemArg, second(), first())::tail2())
        | 0x37uy -> Some(I64Store(r |> MemArg, second(), first())::tail2())
        | 0x38uy -> Some(F32Store(r |> MemArg, second(), first())::tail2())
        | 0x39uy -> Some(F64Store(r |> MemArg, second(), first())::tail2())
        | 0x3Auy -> Some(I32Store8(r |> MemArg, second(), first())::tail2())
        | 0x3Buy -> Some(I32Store16(r |> MemArg, second(), first())::tail2())
        | 0x3Cuy -> Some(I64Store8(r |> MemArg, second(), first())::tail2())
        | 0x3Duy -> Some(I64Store16(r |> MemArg, second(), first())::tail2())
        | 0x3Euy -> Some(I64Store32(r |> MemArg, second(), first())::tail2())

        | 0x3Fuy -> Some( ((MemorySize, r) |> then00)::recent )
        | 0x40uy -> Some( ((GrowMemory, r) |> then00)::recent )

        // 5.4.5  Numeric Instructions

        | 0x41uy -> Some(I32Const(r |> I32)::recent)
        | 0x42uy -> Some(I64Const(r |> I64)::recent)
        | 0x43uy -> Some(F32Const(r |> F32)::recent)
        | 0x44uy -> Some(F64Const(r |> F64)::recent)

        | 0x45uy -> Some(I32Eqz(first())::tail1())
        | 0x46uy -> Some(I32Eq(second(), first())::tail2())
        | 0x47uy -> Some(I32Ne(second(), first())::tail2())
        | 0x48uy -> Some(I32Lts(second(), first())::tail2())
        | 0x49uy -> Some(I32Ltu(second(), first())::tail2())
        | 0x4Auy -> Some(I32Gts(second(), first())::tail2())
        | 0x4Buy -> Some(I32Gtu(second(), first())::tail2())
        | 0x4Cuy -> Some(I32Les(second(), first())::tail2())
        | 0x4Duy -> Some(I32Leu(second(), first())::tail2())
        | 0x4Euy -> Some(I32Ges(second(), first())::tail2())
        | 0x4Fuy -> Some(I32Geu(second(), first())::tail2())

        | 0x50uy -> Some(I64Eqz(first())::tail1())
        | 0x51uy -> Some(I64Eq(second(), first())::tail2())
        | 0x52uy -> Some(I64Ne(second(), first())::tail2())
        | 0x53uy -> Some(I64Lts(second(), first())::tail2())
        | 0x54uy -> Some(I64Ltu(second(), first())::tail2())
        | 0x55uy -> Some(I64Gts(second(), first())::tail2())
        | 0x56uy -> Some(I64Gtu(second(), first())::tail2())
        | 0x57uy -> Some(I64Les(second(), first())::tail2())
        | 0x58uy -> Some(I64Leu(second(), first())::tail2())
        | 0x59uy -> Some(I64Ges(second(), first())::tail2())
        | 0x5Auy -> Some(I64Geu(second(), first())::tail2())
        | 0x5Buy -> Some(F32Eq(second(), first())::tail2())
        | 0x5Cuy -> Some(F32Ne(second(), first())::tail2())
        | 0x5Duy -> Some(F32Lt(second(), first())::tail2())
        | 0x5Euy -> Some(F32Gt(second(), first())::tail2())
        | 0x5Fuy -> Some(F32Le(second(), first())::tail2())

        | 0x60uy -> Some(F32Ge(second(), first())::tail2())
        | 0x61uy -> Some(F64Eq(second(), first())::tail2())
        | 0x62uy -> Some(F64Ne(second(), first())::tail2())
        | 0x63uy -> Some(F64Lt(second(), first())::tail2())
        | 0x64uy -> Some(F64Gt(second(), first())::tail2())
        | 0x65uy -> Some(F64Le(second(), first())::tail2())
        | 0x66uy -> Some(F64Ge(second(), first())::tail2())
        | 0x67uy -> Some(I32Clz(first())::tail1())
        | 0x68uy -> Some(I32Ctz(first())::tail1())
        | 0x69uy -> Some(I32PopCnt(first())::tail1())
        | 0x6Auy -> Some(I32Add(second(), first())::tail2())
        | 0x6Buy -> Some(I32Sub(second(), first())::tail2())
        | 0x6Cuy -> Some(I32Mul(second(), first())::tail2())
        | 0x6Duy -> Some(I32Divs(second(), first())::tail2())
        | 0x6Euy -> Some(I32Divu(second(), first())::tail2())
        | 0x6Fuy -> Some(I32Rems(second(), first())::tail2())

        | 0x70uy -> Some(I32Remu(second(), first())::tail2())
        | 0x71uy -> Some(I32And(second(), first())::tail2())
        | 0x72uy -> Some(I32Or(second(), first())::tail2())
        | 0x73uy -> Some(I32Xor(second(), first())::tail2())
        | 0x74uy -> Some(I32Shl(second(), first())::tail2())
        | 0x75uy -> Some(I32Shrs(second(), first())::tail2())
        | 0x76uy -> Some(I32Shru(second(), first())::tail2())
        | 0x77uy -> Some(I32Rotl(second(), first())::tail2())
        | 0x78uy -> Some(I32Rotr(second(), first())::tail2())
        | 0x79uy -> Some(I64Clz(first())::tail1())
        | 0x7Auy -> Some(I64Ctz(first())::tail1())
        | 0x7Buy -> Some(I64PopCnt(first())::tail1())
        | 0x7Cuy -> Some(I64Add(second(), first())::tail2())
        | 0x7Duy -> Some(I64Sub(second(), first())::tail2())
        | 0x7Euy -> Some(I64Mul(second(), first())::tail2())
        | 0x7Fuy -> Some(I64Divs(second(), first())::tail2())

        | 0x80uy -> Some(I64Divu(second(), first())::tail2())
        | 0x81uy -> Some(I64Rems(second(), first())::tail2())
        | 0x82uy -> Some(I64Remu(second(), first())::tail2())
        | 0x83uy -> Some(I64And(second(), first())::tail2())
        | 0x84uy -> Some(I64Or(second(), first())::tail2())
        | 0x85uy -> Some(I64Xor(second(), first())::tail2())
        | 0x86uy -> Some(I64Shl(second(), first())::tail2())
        | 0x87uy -> Some(I64Shrs(second(), first())::tail2())
        | 0x88uy -> Some(I64Shru(second(), first())::tail2())
        | 0x89uy -> Some(I64Rotl(second(), first())::tail2())
        | 0x8Auy -> Some(I64Rotr(second(), first())::tail2())
        | 0x8Buy -> Some(F32Abs(first())::tail1())
        | 0x8Cuy -> Some(F32Neg(first())::tail1())
        | 0x8Duy -> Some(F32Ceil(first())::tail1())
        | 0x8Euy -> Some(F32Floor(first())::tail1())
        | 0x8Fuy -> Some(F32Trunc(first())::tail1())

        | 0x90uy -> Some(F32Nearest(first())::tail1())
        | 0x91uy -> Some(F32Sqrt(first())::tail1())
        | 0x92uy -> Some(F32Add(second(), first())::tail2())
        | 0x93uy -> Some(F32Sub(second(), first())::tail2())
        | 0x94uy -> Some(F32Mul(second(), first())::tail2())
        | 0x95uy -> Some(F32Div(second(), first())::tail2())
        | 0x96uy -> Some(F32Min(second(), first())::tail2())
        | 0x97uy -> Some(F32Max(second(), first())::tail2())
        | 0x98uy -> Some(F32CopySign(second(), first())::tail2())
        | 0x99uy -> Some(F64Abs(first())::tail1())
        | 0x9Auy -> Some(F64Neg(first())::tail1())
        | 0x9Buy -> Some(F64Ceil(first())::tail1())
        | 0x9Cuy -> Some(F64Floor(first())::tail1())
        | 0x9Duy -> Some(F64Trunc(first())::tail1())
        | 0x9Euy -> Some(F64Nearest(first())::tail1())
        | 0x9Fuy -> Some(F64Sqrt(first())::tail1())

        | 0xA0uy -> Some(F64Add(second(), first())::tail2())
        | 0xA1uy -> Some(F64Sub(second(), first())::tail2())
        | 0xA2uy -> Some(F64Mul(second(), first())::tail2())
        | 0xA3uy -> Some(F64Div(second(), first())::tail2())
        | 0xA4uy -> Some(F64Min(second(), first())::tail2())
        | 0xA5uy -> Some(F64Max(second(), first())::tail2())
        | 0xA6uy -> Some(F64CopySign(second(), first())::tail2())
        | 0xA7uy -> Some(I32WrapI64(first())::tail1())
        | 0xA8uy -> Some(I32TruncsF32(first())::tail1())
        | 0xA9uy -> Some(I32TruncuF32(first())::tail1())
        | 0xAAuy -> Some(I32TruncsF64(first())::tail1())
        | 0xABuy -> Some(I32TruncuF64(first())::tail1())
        | 0xACuy -> Some(I64ExtendsI32(first())::tail1())
        | 0xADuy -> Some(I64ExtenduI32(first())::tail1())
        | 0xAEuy -> Some(I64TruncsF32(first())::tail1())
        | 0xAFuy -> Some(I64TruncuF32(first())::tail1())

        | 0xB0uy -> Some(I64TruncsF64(first())::tail1())
        | 0xB1uy -> Some(I64TruncuF64(first())::tail1())
        | 0xB2uy -> Some(F32ConvertsI32(first())::tail1())
        | 0xB3uy -> Some(F32ConvertuI32(first())::tail1())
        | 0xB4uy -> Some(F32ConvertsI64(first())::tail1())
        | 0xB5uy -> Some(F32ConvertuI64(first())::tail1())
        | 0xB6uy -> Some(F32DemoteF64(first())::tail1())
        | 0xB7uy -> Some(F64ConvertsI32(first())::tail1())
        | 0xB8uy -> Some(F64ConvertuI32(first())::tail1())
        | 0xB9uy -> Some(F64ConvertsI64(first())::tail1())
        | 0xBAuy -> Some(F64ConvertuI64(first())::tail1())
        | 0xBBuy -> Some(F64PromoteF32(first())::tail1())
        | 0xBCuy -> Some(I32ReinterpretF32(first())::tail1())
        | 0xBDuy -> Some(I64ReinterpretF64(first())::tail1())
        | 0xBEuy -> Some(F32ReinterpretI32(first())::tail1())
        | 0xBFuy -> Some(F64ReinterpretI64(first())::tail1())
        
        | _ -> 
            r |> Reverse 1u
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

let TryReadSpecificNumberedSection (sectionReader:BinaryReaderAndContext -> 'section[]) codeOfRequiredSection r : 'section[] =
    match EOF r with
        | true -> [||]
        | false ->
            let backtrackPos = r |> ReadOffset
            let thisHeader = r |> SectionHeader
            match thisHeader with
                | (n, _) when n = byte codeOfRequiredSection -> 
                    r |> sectionReader
                | _ -> 
                    r |> SetReadOffset backtrackPos
                    [||]

let TryReadSpecificNumberedSectionOption sectionReader codeOfRequiredSection r =
    match EOF r with
        | true -> None
        | false ->
            let backtrackPos = r |> ReadOffset
            let thisHeader = r |> SectionHeader
            match thisHeader with
                | (n, _) when n = byte codeOfRequiredSection -> 
                    Some(r |> sectionReader)
                | _ -> 
                    r |> SetReadOffset backtrackPos
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


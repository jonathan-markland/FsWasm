
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
    if r.ReadByte() = theByte 
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
    let newList = new ResizeArray<'recordType>()
    ForEachRecognised recordReaderFunc (fun newRecord -> newList.Add newRecord) theReader |> ignore
    newList.ToArray()

// Read WASM indexes

let TypeIdx   r = WasmTypeIdx  (r |> U32)
let FuncIdx   r = WasmFuncIdx  (r |> U32)
let TableIdx  r = WasmTableIdx (r |> U32)
let MemIdx    r = WasmMemIdx   (r |> U32)
let GlobalIdx r = WasmGlobalIdx(r |> U32)
let LocalIdx  r = WasmLocalIdx (r |> U32)
let LabelIdx  r = WasmLabelIdx (r |> U32)

// Wasm basic reading

let Limits r =
    match r |> Byte with
        | 0x00uy -> 
            let n = r |> U32
            { LimitMin=n; LimitMax=None }
        | 0x01uy ->
            let n = r |> U32
            let m = r |> U32
            { LimitMin=n; LimitMax=Some(m) }
        | _ -> failwith "Unknown limit type code"

let Mutability r =
    match r |> Byte with
        | 0x00uy -> Const_00
        | 0x01uy -> Var_01
        | _ -> failwith "Unknown mutability type code"

let ExportDesc r =
    match r |> Byte with
        | 0x00uy -> ExpFunc_00(r |> FuncIdx)
        | 0x01uy -> ExpTable_01(r |> TableIdx)
        | 0x02uy -> ExpMem_02(r |> MemIdx)
        | 0x03uy -> ExpGlobal_03(r |> GlobalIdx)
        | _ -> failwith "Unrecognised ExportDesc code"

let ValType (r:WasmSerialiser.BinaryReader) =
    match r.ReadByte() with
        | 0x7Fuy -> I32_7F
        | 0x7Euy -> I64_7E
        | 0x7Duy -> F32_7D
        | 0x7Cuy -> F64_7C
        | _ -> failwith "Unrecognised valtype byte"  // TODO: elaborate

// Wasm file reading assistance

let SectionHeader (r:WasmSerialiser.BinaryReader) =
    let sectionType = r.ReadByte()
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
    { TableElementType=AnyFunc_70; TableLimits=tableLimits }

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
        EmptyBlockType_40
    else
        let valType = r |> ValType
        BlockValType(valType)

let ImportDesc r =
    match r |> Byte with
        | 0x00uy -> ImpFunc_00(r |> TypeIdx)
        | 0x01uy -> ImpTable_01(r |> TableType)
        | 0x02uy -> ImpMem_02(r |> MemType)
        | 0x03uy -> ImpGlobal_03(r |> GlobalType)
        | _ -> failwith "Unrecognised ImportDesc code"

// Instructions

let rec InstructionList r =

    r |> MakeArrayWhileSome Instruction

and Instruction (r:WasmSerialiser.BinaryReader) =

    let then00 (code, (r:WasmSerialiser.BinaryReader)) =
        r |> ExpectByte 0x00uy
        code

    let opcodeByte = r |> Byte

    match opcodeByte with

        | 0x00uy -> Some(Unreachable_00)

        | 0x01uy -> Some(Nop_01)

        | 0x02uy -> 
            let blockType = r |> BlockType
            let subBlock = r |> InstructionList
            Some(Block_02_0B(blockType, subBlock))

        | 0x03uy -> 
            let blockType = r |> BlockType
            let subBlock = r |> InstructionList
            Some(Loop_03_0B(blockType, subBlock))

        | 0x04uy -> 
            let blockType = r |> BlockType
            let ifBlock = r |> InstructionList
            let elseByte = r |> Byte
            if not (elseByte = 0x05uy) then
                Some(If_04_0B(blockType, ifBlock))
            else
                let elseBlock = r |> InstructionList
                Some(IfElse_04_05_0B(blockType, ifBlock, elseBlock))

        | 0x0Cuy -> Some(Br_0C (r |> LabelIdx))
        | 0x0Duy -> Some(BrIf_0D (r |> LabelIdx))

        | 0x0Euy -> 
            let tableContent = r |> Vector LabelIdx
            let labelIdx = r |> LabelIdx
            Some(BrTable_0E(tableContent, labelIdx))

        | 0x0Fuy -> Some(Return_0F)
        | 0x10uy -> Some(Call_10(r |> FuncIdx))
        | 0x11uy -> Some(CallIndirect_11_00(r |> TypeIdx))

        // 5.4.2  Parameteric Instructions

        | 0x1Auy -> Some(Drop_1A)
        | 0x1Buy -> Some(Select_1B)

        // 5.4.3  Variable Instructions

        | 0x20uy -> Some(GetLocal_20(r |> LocalIdx))
        | 0x21uy -> Some(SetLocal_21(r |> LocalIdx))
        | 0x22uy -> Some(TeeLocal_22(r |> LocalIdx))
        | 0x23uy -> Some(GetGlobal_23(r |> GlobalIdx))
        | 0x24uy -> Some(SetGlobal_24(r |> GlobalIdx))

        // 5.4.4  Memory Instructions

        | 0x28uy -> Some(I32Load_28(r |> MemArg))
        | 0x29uy -> Some(I64Load_29(r |> MemArg))
        | 0x2Auy -> Some(F32Load_2A(r |> MemArg))
        | 0x2Buy -> Some(F64Load_2B(r |> MemArg))
        | 0x2Cuy -> Some(I32Load8s_2C(r |> MemArg))
        | 0x2Duy -> Some(I32Load8u_2D(r |> MemArg))
        | 0x2Euy -> Some(I32Load16s_2E(r |> MemArg))
        | 0x2Fuy -> Some(I32Load16u_2F(r |> MemArg))
        | 0x30uy -> Some(I64Load8s_30(r |> MemArg))
        | 0x31uy -> Some(I64Load8u_31(r |> MemArg))
        | 0x32uy -> Some(I64Load16s_32(r |> MemArg))
        | 0x33uy -> Some(I64Load16u_33(r |> MemArg))
        | 0x34uy -> Some(I64Load32s_34(r |> MemArg))
        | 0x35uy -> Some(I64Load32u_35(r |> MemArg))
        | 0x36uy -> Some(I32Store_36(r |> MemArg))
        | 0x37uy -> Some(I64Store_37(r |> MemArg))
        | 0x38uy -> Some(F32Store_38(r |> MemArg))
        | 0x39uy -> Some(F64Store_39(r |> MemArg))
        | 0x3Auy -> Some(I32Store8_3A(r |> MemArg))
        | 0x3Buy -> Some(I32Store16_3B(r |> MemArg))
        | 0x3Cuy -> Some(I64Store8_3C(r |> MemArg))
        | 0x3Duy -> Some(I64Store16_3D(r |> MemArg))
        | 0x3Euy -> Some(I64Store32_3E(r |> MemArg))

        | 0x3Fuy -> (Some(MemorySize_3F_00), r) |> then00
        | 0x40uy -> (Some(GrowMemory_40_00), r) |> then00

        // 5.4.5  Numeric Instructions

        | 0x41uy -> Some(I32Const_41(r |> I32))
        | 0x42uy -> Some(I64Const_42(r |> I64))
        | 0x43uy -> Some(F32Const_43(r |> F32))
        | 0x44uy -> Some(F64Const_44(r |> F64))

        | 0x45uy -> Some(I32Eqz_45)
        | 0x46uy -> Some(I32Eq_46)
        | 0x47uy -> Some(I32Ne_47)
        | 0x48uy -> Some(I32Lt_s_48)
        | 0x49uy -> Some(I32Lt_u_49)
        | 0x4Auy -> Some(I32Gt_s_4A)
        | 0x4Buy -> Some(I32Gt_u_4B)
        | 0x4Cuy -> Some(I32Le_s_4C)
        | 0x4Duy -> Some(I32Le_u_4D)
        | 0x4Euy -> Some(I32Ge_s_4E)
        | 0x4Fuy -> Some(I32Ge_u_4F)

        | 0x50uy -> Some(I64Eqz_50)
        | 0x51uy -> Some(I64Eq_51)
        | 0x52uy -> Some(I64Ne_52)
        | 0x53uy -> Some(I64Lt_s_53)
        | 0x54uy -> Some(I64Lt_u_54)
        | 0x55uy -> Some(I64Gt_s_55)
        | 0x56uy -> Some(I64Gt_u_56)
        | 0x57uy -> Some(I64Le_s_57)
        | 0x58uy -> Some(I64Le_u_58)
        | 0x59uy -> Some(I64Ge_s_59)
        | 0x5Auy -> Some(I64Ge_u_5A)
        | 0x5Buy -> Some(F32Eq_5B)
        | 0x5Cuy -> Some(F32Ne_5C)
        | 0x5Duy -> Some(F32Lt_5D)
        | 0x5Euy -> Some(F32Gt_5E)
        | 0x5Fuy -> Some(F32Le_5F)

        | 0x60uy -> Some(F32Ge_60)
        | 0x61uy -> Some(F64Eq_61)
        | 0x62uy -> Some(F64Ne_62)
        | 0x63uy -> Some(F64Lt_63)
        | 0x64uy -> Some(F64Gt_64)
        | 0x65uy -> Some(F64Le_65)
        | 0x66uy -> Some(F64Ge_66)
        | 0x67uy -> Some(I32Clz_67)
        | 0x68uy -> Some(I32Ctz_68)
        | 0x69uy -> Some(I32PopCnt_69)
        | 0x6Auy -> Some(I32Add_6A)
        | 0x6Buy -> Some(I32Sub_6B)
        | 0x6Cuy -> Some(I32Mul_6C)
        | 0x6Duy -> Some(I32Div_s_6D)
        | 0x6Euy -> Some(I32Div_u_6E)
        | 0x6Fuy -> Some(I32Rem_s_6F)

        | 0x70uy -> Some(I32Rem_u_70)
        | 0x71uy -> Some(I32And_71)
        | 0x72uy -> Some(I32Or_72)
        | 0x73uy -> Some(I32Xor_73)
        | 0x74uy -> Some(I32Shl_74)
        | 0x75uy -> Some(I32Shr_s_75)
        | 0x76uy -> Some(I32Shr_u_76)
        | 0x77uy -> Some(I32Rotl_77)
        | 0x78uy -> Some(I32Rotr_78)
        | 0x79uy -> Some(I64Clz_79)
        | 0x7Auy -> Some(I64Ctz_7A)
        | 0x7Buy -> Some(I64PopCnt_7B)
        | 0x7Cuy -> Some(I64Add_7C)
        | 0x7Duy -> Some(I64Sub_7D)
        | 0x7Euy -> Some(I64Mul_7E)
        | 0x7Fuy -> Some(I64Div_s_7F)

        | 0x80uy -> Some(I64Div_u_80)
        | 0x81uy -> Some(I64Rem_s_81)
        | 0x82uy -> Some(I64Rem_u_82)
        | 0x83uy -> Some(I64And_83)
        | 0x84uy -> Some(I64Or_84)
        | 0x85uy -> Some(I64Xor_85)
        | 0x86uy -> Some(I64Shl_86)
        | 0x87uy -> Some(I64Shr_s_87)
        | 0x88uy -> Some(I64Shr_u_88)
        | 0x89uy -> Some(I64Rotl_89)
        | 0x8Auy -> Some(I64Rotr_8A)
        | 0x8Buy -> Some(F32Abs_8B)
        | 0x8Cuy -> Some(F32Neg_8C)
        | 0x8Duy -> Some(F32Ceil_8D)
        | 0x8Euy -> Some(F32Floor_8E)
        | 0x8Fuy -> Some(F32Trunc_8F)

        | 0x90uy -> Some(F32Nearest_90)
        | 0x91uy -> Some(F32Sqrt_91)
        | 0x92uy -> Some(F32Add_92)
        | 0x93uy -> Some(F32Sub_93)
        | 0x94uy -> Some(F32Mul_94)
        | 0x95uy -> Some(F32Div_95)
        | 0x96uy -> Some(F32Min_96)
        | 0x97uy -> Some(F32Max_97)
        | 0x98uy -> Some(F32CopySign_98)
        | 0x99uy -> Some(F64Abs_99)
        | 0x9Auy -> Some(F64Neg_9A)
        | 0x9Buy -> Some(F64Ceil_9B)
        | 0x9Cuy -> Some(F64Floor_9C)
        | 0x9Duy -> Some(F64Trunc_9D)
        | 0x9Euy -> Some(F64Nearest_9E)
        | 0x9Fuy -> Some(F64Sqrt_9F)

        | 0xA0uy -> Some(F64Add_A0)
        | 0xA1uy -> Some(F64Sub_A1)
        | 0xA2uy -> Some(F64Mul_A2)
        | 0xA3uy -> Some(F64Div_A3)
        | 0xA4uy -> Some(F64Min_A4)
        | 0xA5uy -> Some(F64Max_A5)
        | 0xA6uy -> Some(F64CopySign_A6)
        | 0xA7uy -> Some(I32Wrap_I64_A7)
        | 0xA8uy -> Some(I32Trunc_s_F32_A8)
        | 0xA9uy -> Some(I32Trunc_u_F32_A9)
        | 0xAAuy -> Some(I32Trunc_s_F64_AA)
        | 0xABuy -> Some(I32Trunc_u_F64_AB)
        | 0xACuy -> Some(I64Extend_s_i32_AC)
        | 0xADuy -> Some(I64Extend_u_i32_AD)
        | 0xAEuy -> Some(I64Trunc_s_F32_AE)
        | 0xAFuy -> Some(I64Trunc_u_F32_AF)

        | 0xB0uy -> Some(I64Trunc_s_F64_B0)
        | 0xB1uy -> Some(I64Trunc_u_F64_B1)
        | 0xB2uy -> Some(F32Convert_s_i32_B2)
        | 0xB3uy -> Some(F32Convert_u_i32_B3)
        | 0xB4uy -> Some(F32Convert_s_i64_B4)
        | 0xB5uy -> Some(F32Convert_u_i64_B5)
        | 0xB6uy -> Some(F32Demote_F64_B6)
        | 0xB7uy -> Some(F64Convert_s_i32_B7)
        | 0xB8uy -> Some(F64Convert_u_i32_B8)
        | 0xB9uy -> Some(F64Convert_s_i64_B9)
        | 0xBAuy -> Some(F64Convert_u_i64_BA)
        | 0xBBuy -> Some(F64Promote_F32_BB)
        | 0xBCuy -> Some(I32Reinterpret_F32_BC)
        | 0xBDuy -> Some(I64Reinterpret_F64_BD)
        | 0xBEuy -> Some(F32Reinterpret_i32_BE)
        | 0xBFuy -> Some(F64Reinterpret_i64_BF)
        
        | _ -> 
            r.Reverse(1u)
            None

let ReadExpr r =
    let instrList = r |> InstructionList
    r |> ExpectByte 0x0Buy
    instrList

let ReadGlobal r =
    let globalType = r |> GlobalType
    let initialisationExpression = r |> ReadExpr
    { GlobalType=globalType; InitExpr=initialisationExpression }

let ReadExport r = 
    let exportName = r |> NameString
    let exportDesc = r |> ExportDesc
    { nm=exportName; d=exportDesc }

let ReadLocals r =
    let numRepeats = r |> U32
    let theType = r |> ValType
    { NumRepeats=numRepeats; LocalsType=theType }

let ReadFunc r = 
    let theLocals = r |> Vector ReadLocals
    let theExpr = r |> ReadExpr
    { Locals=theLocals; Body=theExpr }

let ReadCode r = 
    let codeSize = r |> U32
    let theFunc = r |> ReadFunc
    { Size=codeSize; Code=theFunc }

let ReadImport r = 
    let moduleName = r |> NameString
    let importName = r |> NameString
    let importDesc = r |> ImportDesc
    { Mod=moduleName; nm=importName; d=importDesc }

let ReadElem r =
    let tableIdx = r |> TableIdx
    let elemExpr = r |> ReadExpr
    let funcIdxArray = r |> Vector FuncIdx
    { TableIndex=tableIdx; Offset=elemExpr; Init=funcIdxArray }

let ReadData r =
    let memIdx = r |> MemIdx
    let offsetExpr = r |> ReadExpr
    let dataBytes = r |> Bytes
    { DataMemoryIndex=memIdx; OffsetExpr=offsetExpr; InitImage=dataBytes }

// Read Wasm Sections

let ReadCustomSec r = 
    let customName = r |> NameString
    let customBytes = r |> Bytes
    WasmCustomSec({ Name=customName; Data=customBytes })

let ReadTypeSec   r = WasmTypeSec(r |> Vector FuncType)
let ReadImportSec r = WasmImportSec(r |> Vector ReadImport)
let ReadFuncSec   r = WasmFuncSec(r |> Vector TypeIdx)
let ReadTableSec  r = WasmTableSec(r |> Vector Table)
let ReadMemSec    r = WasmMemSec(r |> Vector Mem)
let ReadGlobalSec r = WasmGlobalSec(r |> Vector ReadGlobal)
let ReadExportSec r = WasmExportSec(r |> Vector ReadExport)
let ReadCodeSec   r = WasmCodeSec(r |> Vector ReadCode)

let ReadStartSec  r = 
    let startFuncIdx = r |> FuncIdx
    WasmStartSec({ StartFuncIdx=startFuncIdx })

let ReadElemSec r = WasmElemSec(r |> Vector ReadElem)
let ReadDataSec r = WasmDataSec(r |> Vector ReadData)

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

let ReadOptionalCustomSec r = 
    r |> TryReadSpecificNumberedSection ReadCustomSec 0

let ReadCustomSecArray r =
    r |> MakeArrayWhileSome ReadOptionalCustomSec

let ReadCustomThenTrySpecificSection sectionReader codeOfRequiredSection r =
    let optionalCustomSecArray = r |> ReadCustomSecArray
    let optionalSection = r |> TryReadSpecificNumberedSection sectionReader codeOfRequiredSection
    (optionalCustomSecArray, optionalSection)

// Read module

let ReadModule r =

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

    let sec1  = r |> ReadCustomThenTrySpecificSection ReadTypeSec 1 
    let sec2  = r |> ReadCustomThenTrySpecificSection ReadImportSec 2
    let sec3  = r |> ReadCustomThenTrySpecificSection ReadFuncSec 3
    let sec4  = r |> ReadCustomThenTrySpecificSection ReadTableSec 4
    let sec5  = r |> ReadCustomThenTrySpecificSection ReadMemSec 5
    let sec6  = r |> ReadCustomThenTrySpecificSection ReadGlobalSec 6
    let sec7  = r |> ReadCustomThenTrySpecificSection ReadExportSec 7
    let sec8  = r |> ReadCustomThenTrySpecificSection ReadStartSec 8
    let sec9  = r |> ReadCustomThenTrySpecificSection ReadElemSec 9
    let sec10 = r |> ReadCustomThenTrySpecificSection ReadCodeSec 10
    let sec11 = r |> ReadCustomThenTrySpecificSection ReadDataSec 11
    let finalCustom = r |> ReadCustomSecArray

    // Return loaded result:

    {   Custom1 = fst sec1; Types=snd sec1;
        Custom2 = fst sec1; Imports=snd sec2;
        Custom3 = fst sec1; Funcs=snd sec3;
        Custom4 = fst sec1; Tables=snd sec4;
        Custom5 = fst sec1; Mems=snd sec5;
        Custom6 = fst sec1; Globals=snd sec6;
        Custom7 = fst sec1; Exports=snd sec7;
        Custom8 = fst sec1; Start=snd sec8;
        Custom9 = fst sec1; Elems=snd sec9;
        Custom10 = fst sec1; Codes=snd sec10;
        Custom11 = fst sec1; Datas=snd sec11;
        Custom12 = finalCustom;
    }

// Main

[<EntryPoint>]
let main argv =

    let fileImage = File.ReadAllBytes("program (1).wasm");
    let r = new WasmSerialiser.BinaryReader(fileImage)
    let thisModule = r |> ReadModule

    printfn "Hello World from F#!"
    0 // return an integer exit code

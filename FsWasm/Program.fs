
open System
open System.IO
open FsWasmLibrary.Wasm
open System.Reflection.Metadata.Ecma335

// Interfacing the BinaryReader:

let ReadByte    (r:WasmSerialiser.BinaryReader) = r.ReadByte()
let PeekByte    (r:WasmSerialiser.BinaryReader) = r.PeekByte()
let ReadLeb32   (r:WasmSerialiser.BinaryReader) = r.ReadLebUnsigned32()
let ReadLeb64   (r:WasmSerialiser.BinaryReader) = r.ReadLebUnsigned64()
let ReadFloat32 (r:WasmSerialiser.BinaryReader) = r.ReadFloat32()
let ReadFloat64 (r:WasmSerialiser.BinaryReader) = r.ReadDouble64()
let ReadName    (r:WasmSerialiser.BinaryReader) = r.ReadString()
let ReadBytes   (r:WasmSerialiser.BinaryReader) = r.ReadByteVector()

// Interfacing the BinaryReader with Wasm type wrappers, for convenience:

let ReadWasmI32 r = WasmI32(int (r |> ReadLeb32))
let ReadWasmU32 r = WasmU32(r |> ReadLeb32)
let ReadWasmI64 r = WasmI64(int64 (r |> ReadLeb64))
let ReadWasmF32 r = WasmF32(r |> ReadFloat32)
let ReadWasmF64 r = WasmF64(r |> ReadFloat64)

// Generic parsing assistance

let ReadSequence (f:WasmSerialiser.BinaryReader -> 'a) r =
    let mutable elementCount = r |> ReadLeb32
    seq { while elementCount > 0u 
    do
        elementCount <- elementCount - 1u
        yield r |> f }

let ReadVector f r = 
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

// Read WASM indexes

let ReadTypeIdx   r = WasmTypeIdx  (r |> ReadWasmU32)
let ReadFuncIdx   r = WasmFuncIdx  (r |> ReadWasmU32)
let ReadTableIdx  r = WasmTableIdx (r |> ReadWasmU32)
let ReadMemIdx    r = WasmMemIdx   (r |> ReadWasmU32)
let ReadGlobalIdx r = WasmGlobalIdx(r |> ReadWasmU32)
let ReadLocalIdx  r = WasmLocalIdx (r |> ReadWasmU32)
let ReadLabelIdx  r = WasmLabelIdx (r |> ReadWasmU32)

// Wasm basic reading

let ReadLimits r =
    match r |> ReadByte with
        | 0x00uy -> 
            let n = r |> ReadWasmU32
            { Min=n; Max=None }
        | 0x01uy ->
            let n = r |> ReadWasmU32
            let m = r |> ReadWasmU32
            { Min=n; Max=Some(m) }
        | _ -> failwith "Unknown limit type code"

let ReadMut r =
    match r |> ReadByte with
        | 0x00uy -> Const_00
        | 0x01uy -> Var_01
        | _ -> failwith "Unknown mutability type code"

let ReadExportDesc r =
    match r |> ReadByte with
        | 0x00uy -> ExpFunc_00(r |> ReadFuncIdx)
        | 0x01uy -> ExpTable_01(r |> ReadTableIdx)
        | 0x02uy -> ExpMem_02(r |> ReadMemIdx)
        | 0x03uy -> ExpGlobal_03(r |> ReadGlobalIdx)
        | _ -> failwith "Unrecognised ExportDesc code"

let ReadValType (r:WasmSerialiser.BinaryReader) =
    match r.ReadByte() with
        | 0x7Fuy -> I32_7F
        | 0x7Euy -> I64_7E
        | 0x7Duy -> F32_7D
        | 0x7Cuy -> F64_7C
        | _ -> failwith "Unrecognised valtype byte"  // TODO: elaborate

// Wasm file reading assistance

let ReadSectionHeader (r:WasmSerialiser.BinaryReader) =
    let sectionType = r.ReadByte()
    let sectionLength = r |> ReadLeb32
    (sectionType, sectionLength)

// Wasm Type reading

let ReadMemArg (r:WasmSerialiser.BinaryReader) =
    let a = r |> ReadWasmU32
    let o = r |> ReadWasmU32
    { Align=a; Offset=o }

let ReadMemType r =
    let memLimits = r |> ReadLimits
    { MemLim=memLimits }

let ReadMem r = 
    let memType = r |> ReadMemType
    { MemType=memType }

let ReadFuncType r =
    r |> ExpectByte 0x60uy
    let funcInputs = r |> ReadVector ReadValType
    let funcOutputs = r |> ReadVector ReadValType
    { FuncInputs=funcInputs; FuncOutputs=funcOutputs }

let ReadTableType r = 
    r |> ExpectByte 0x70uy   // There is only one kind defined.
    let tableLimits = r |> ReadLimits
    { ElementType=AnyFunc_70; TableLim=tableLimits }

let ReadTable r =
    let tableType = r |> ReadTableType
    { TableType=tableType }

let ReadGlobalType r = 
    let globalType = r |> ReadValType
    let globalMutability = r |> ReadMut
    { Type=globalType; Mutability=globalMutability }

let ReadBlockType (r:WasmSerialiser.BinaryReader) =
    if r.PeekByte() = 0x40uy then 
        r.SkipByte()
        EmptyBlockType_40
    else
        let valType = r |> ReadValType
        BlockValType(valType)

let ReadImportDesc r =
    match r |> ReadByte with
        | 0x00uy -> ImpFunc_00(r |> ReadTypeIdx)
        | 0x01uy -> ImpTable_01(r |> ReadTableType)
        | 0x02uy -> ImpMem_02(r |> ReadMemType)
        | 0x03uy -> ImpGlobal_03(r |> ReadGlobalType)
        | _ -> failwith "Unrecognised ImportDesc code"

// Instructions

let rec ReadInstrList r =

    let newList = new ResizeArray<Instr>()
    ForEachRecognised ReadInstr (fun x -> newList.Add x) r |> ignore
    newList.ToArray()

and ReadInstr (r:WasmSerialiser.BinaryReader) =

    let then00 (code, (r:WasmSerialiser.BinaryReader)) =
        r |> ExpectByte 0x00uy
        code

    let opcodeByte = r |> ReadByte

    match opcodeByte with

        | 0x00uy -> Some(Unreachable_00)

        | 0x01uy -> Some(Nop_01)

        | 0x02uy -> 
            let blockType = r |> ReadBlockType
            let subBlock = r |> ReadInstrList
            Some(Block_02_0B(blockType, subBlock))

        | 0x03uy -> 
            let blockType = r |> ReadBlockType
            let subBlock = r |> ReadInstrList
            Some(Loop_03_0B(blockType, subBlock))

        | 0x04uy -> 
            let blockType = r |> ReadBlockType
            let ifBlock = r |> ReadInstrList
            let elseByte = r |> ReadByte
            if not (elseByte = 0x05uy) then
                Some(If_04_0B(blockType, ifBlock))
            else
                let elseBlock = r |> ReadInstrList
                Some(IfElse_04_05_0B(blockType, ifBlock, elseBlock))

        | 0x0Cuy -> Some(Br_0C (r |> ReadLabelIdx))
        | 0x0Duy -> Some(BrIf_0D (r |> ReadLabelIdx))

        | 0x0Euy -> 
            let tableContent = r |> ReadVector ReadLabelIdx
            let labelIdx = r |> ReadLabelIdx
            Some(BrTable_0E(tableContent, labelIdx))

        | 0x0Fuy -> Some(Return_0F)
        | 0x10uy -> Some(Call_10(r |> ReadFuncIdx))
        | 0x11uy -> Some(CallIndirect_11_00(r |> ReadTypeIdx))

        // 5.4.2  Parameteric Instructions

        | 0x1Auy -> Some(Drop_1A)
        | 0x1Buy -> Some(Select_1B)

        // 5.4.3  Variable Instructions

        | 0x20uy -> Some(GetLocal_20(r |> ReadLocalIdx))
        | 0x21uy -> Some(SetLocal_21(r |> ReadLocalIdx))
        | 0x22uy -> Some(TeeLocal_22(r |> ReadLocalIdx))
        | 0x23uy -> Some(GetGlobal_23(r |> ReadGlobalIdx))
        | 0x24uy -> Some(SetGlobal_24(r |> ReadGlobalIdx))

        // 5.4.4  Memory Instructions

        | 0x28uy -> Some(I32Load_28(r |> ReadMemArg))
        | 0x29uy -> Some(I64Load_29(r |> ReadMemArg))
        | 0x2Auy -> Some(F32Load_2A(r |> ReadMemArg))
        | 0x2Buy -> Some(F64Load_2B(r |> ReadMemArg))
        | 0x2Cuy -> Some(I32Load8s_2C(r |> ReadMemArg))
        | 0x2Duy -> Some(I32Load8u_2D(r |> ReadMemArg))
        | 0x2Euy -> Some(I32Load16s_2E(r |> ReadMemArg))
        | 0x2Fuy -> Some(I32Load16u_2F(r |> ReadMemArg))
        | 0x30uy -> Some(I64Load8s_30(r |> ReadMemArg))
        | 0x31uy -> Some(I64Load8u_31(r |> ReadMemArg))
        | 0x32uy -> Some(I64Load16s_32(r |> ReadMemArg))
        | 0x33uy -> Some(I64Load16u_33(r |> ReadMemArg))
        | 0x34uy -> Some(I64Load32s_34(r |> ReadMemArg))
        | 0x35uy -> Some(I64Load32u_35(r |> ReadMemArg))
        | 0x36uy -> Some(I32Store_36(r |> ReadMemArg))
        | 0x37uy -> Some(I64Store_37(r |> ReadMemArg))
        | 0x38uy -> Some(F32Store_38(r |> ReadMemArg))
        | 0x39uy -> Some(F64Store_39(r |> ReadMemArg))
        | 0x3Auy -> Some(I32Store8_3A(r |> ReadMemArg))
        | 0x3Buy -> Some(I32Store16_3B(r |> ReadMemArg))
        | 0x3Cuy -> Some(I64Store8_3C(r |> ReadMemArg))
        | 0x3Duy -> Some(I64Store16_3D(r |> ReadMemArg))
        | 0x3Euy -> Some(I64Store32_3E(r |> ReadMemArg))

        | 0x3Fuy -> (Some(MemorySize_3F_00), r) |> then00
        | 0x40uy -> (Some(GrowMemory_40_00), r) |> then00

        // 5.4.5  Numeric Instructions

        | 0x41uy -> Some(I32Const_41(r |> ReadWasmI32))
        | 0x42uy -> Some(I64Const_42(r |> ReadWasmI64))
        | 0x43uy -> Some(F32Const_43(r |> ReadWasmF32))
        | 0x44uy -> Some(F64Const_44(r |> ReadWasmF64))

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
    let instrList = r |> ReadInstrList
    r |> ExpectByte 0x0Buy
    instrList

let ReadGlobal r =
    let globalType = r |> ReadGlobalType
    let initialisationExpression = r |> ReadExpr
    { GlobalType=globalType; InitExpr=initialisationExpression }

let ReadExport r = 
    let exportName = r |> ReadName
    let exportDesc = r |> ReadExportDesc
    { nm=exportName; d=exportDesc }

let ReadLocals r =
    let numRepeats = r |> ReadWasmU32
    let theType = r |> ReadValType
    { NumRepeats=numRepeats; LocalsType=theType }

let ReadFunc r = 
    let theLocals = r |> ReadVector ReadLocals
    let theExpr = r |> ReadExpr
    { Locals=theLocals; Body=theExpr }

let ReadCode r = 
    let codeSize = r |> ReadWasmU32
    let theFunc = r |> ReadFunc
    { Size=codeSize; Code=theFunc }

let ReadImport r = 
    let moduleName = r |> ReadName
    let importName = r |> ReadName
    let importDesc = r |> ReadImportDesc
    { Mod=moduleName; nm=importName; d=importDesc }

let ReadElem r =
    let tableIdx = r |> ReadTableIdx
    let elemExpr = r |> ReadExpr
    let funcIdxArray = r |> ReadVector ReadFuncIdx
    { TableIndex=tableIdx; Offset=elemExpr; Init=funcIdxArray }

let ReadData r =
    let memIdx = r |> ReadMemIdx
    let offsetExpr = r |> ReadExpr
    let dataBytes = r |> ReadBytes
    { DataMemoryIndex=memIdx; OffsetExpr=offsetExpr; InitImage=dataBytes }

// Read Wasm Sections

let ReadCustomSec r = 
    let customName = r |> ReadName
    let customBytes = r |> ReadBytes
    WasmCustomSec({ Name=customName; Data=customBytes })

let ReadTypeSec   r = WasmTypeSec(r |> ReadVector ReadFuncType)
let ReadImportSec r = WasmImportSec(r |> ReadVector ReadImport)
let ReadFuncSec   r = WasmFuncSec(r |> ReadVector ReadTypeIdx)
let ReadTableSec  r = WasmTableSec(r |> ReadVector ReadTable)
let ReadMemSec    r = WasmMemSec(r |> ReadVector ReadMem)
let ReadGlobalSec r = WasmGlobalSec(r |> ReadVector ReadGlobal)
let ReadExportSec r = WasmExportSec(r |> ReadVector ReadExport)
let ReadCodeSec   r = WasmCodeSec(r |> ReadVector ReadCode)

let ReadStartSec  r = 
    let startFuncIdx = r |> ReadFuncIdx
    WasmStartSec({ StartFuncIdx=startFuncIdx })

let ReadElemSec r = WasmElemSec(r |> ReadVector ReadElem)
let ReadDataSec r = WasmDataSec(r |> ReadVector ReadData)

// Main

[<EntryPoint>]
let main argv =

    let fileImage = File.ReadAllBytes("program (1).wasm");
    
    let theReader = new WasmSerialiser.BinaryReader(fileImage)

    let magic1 = theReader |> ReadByte
    let magic2 = theReader |> ReadByte
    let magic3 = theReader |> ReadByte
    let magic4 = theReader |> ReadByte
    let magic5 = theReader |> ReadByte
    let magic6 = theReader |> ReadByte
    let magic7 = theReader |> ReadByte
    let magic8 = theReader |> ReadByte

    let sec1 = theReader |> ReadSectionHeader
    let dat1 = theReader |> ReadTypeSec

    let sec2 = theReader |> ReadSectionHeader
    let dat2 = theReader |> ReadImportSec

    let sec3 = theReader |> ReadSectionHeader
    let dat3 = theReader |> ReadFuncSec

    let sec4 = theReader |> ReadSectionHeader
    let dat4 = theReader |> ReadTableSec

    let sec5 = theReader |> ReadSectionHeader
    let dat5 = theReader |> ReadMemSec

    let sec6 = theReader |> ReadSectionHeader
    let dat6 = theReader |> ReadGlobalSec

    let sec7 = theReader |> ReadSectionHeader
    let dat7 = theReader |> ReadExportSec

    let sec8 = theReader |> ReadSectionHeader
    let dat8 = theReader |> ReadStartSec

    let sec9 = theReader |> ReadSectionHeader
    let dat9 = theReader |> ReadElemSec

    let sec10 = theReader |> ReadSectionHeader
    let dat10 = theReader |> ReadCodeSec

    let sec11 = theReader |> ReadSectionHeader
    let dat11 = theReader |> ReadDataSec

    printfn "Hello World from F#!"
    0 // return an integer exit code

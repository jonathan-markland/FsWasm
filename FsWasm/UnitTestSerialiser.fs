module UnitTestSerialiser

open System.Text
open Wasm
open WasmAlgorithms

let ModuleToUnitTestString (fileName:string) (m:Module) =

    let sb = new StringBuilder ()

    let NewLine () =
        sb.AppendLine() |> ignore

    let Add (t:string) = 
        sb.Append t |> ignore

    let Text (t:string) = 
        sb.Append t |> ignore
        sb.AppendLine "" |> ignore

    let Title (t:string) =
        sb.AppendLine "" |> ignore
        sb.AppendLine "" |> ignore
        sb.Append "--- " |> ignore
        sb.Append t |> ignore
        sb.AppendLine " ---" |> ignore
        sb.AppendLine "" |> ignore

    let AddObject obj =
        sb.Append (sprintf "%A" obj) |> ignore

    let AddOptionalObject optionalObj =
        match optionalObj with
            | None -> ()
            | Some(X) -> AddObject X

    let AddArraySection sectionTitle sectionArray =
        sectionArray |> Array.iteri (fun i X -> 
            Title (sprintf "%s [%d]" sectionTitle i)
            AddObject X)

    let AddGenericSection sectionTitle genericSection =
        Title (sprintf "%s" sectionTitle)
        AddOptionalObject genericSection

    // COMMON

    let SingleLineFormatted obj =
        (sprintf "%A" obj)
            .Replace("\r", " ")
            .Replace("\n", " ")
            .Replace("  ", " ")
            .Replace("  ", " ")   // TODO: hackish
            .Replace("  ", " ")

    let AddLimits l =
        match l with
            | { LimitMin=U32(m); LimitMax=None }         -> Add (sprintf "[|%d|]" m)
            | { LimitMin=U32(a); LimitMax=Some(U32(b)) } -> Add (sprintf "[|%d..%d|]" a b)

    let PrettyMutability =
        function 
        | Constant -> "const"
        | Variable -> "var"

    let PrettyValType = 
        function
        | I32Type -> "I32"
        | I64Type -> "I64"
        | F32Type -> "F32"
        | F64Type -> "F64"

    let AddByteArray a leftSideString =
        
        a |> Array.iteri (fun i b -> 
            if (i &&& 15) = 0 then Add leftSideString
            Add (sprintf "%02x " b)
            if (i &&& 15) = 15 then Text "")

    // TYPE SECTION

    let AddValType vt =
        Add (PrettyValType vt)
        Add " "

    let AddValTypes vts =
        vts |> Array.iter AddValType

    let AddFuncType ft =
        AddValTypes ft.ParameterTypes
        Add "-> "
        AddValTypes ft.ReturnTypes

    let AddTypeSecEntry i (types:FuncType[]) =
        Add (sprintf "TypeSec[%d] = " i)
        if i >= types.Length then Add "Error: TypeSec index out of range"
        else AddFuncType types.[i]

    let AddTypeSec types =
        Title "Types section"
        types |> Array.iteri 
            (fun i _ ->
                AddTypeSecEntry i types
                NewLine ())
        
    // FUNC SECTION  (which we indirect through the TypeSec to show the signatures)

    let AddFuncSecEntry (i:int) (funcs:FuncType[]) =
        Add (sprintf "FuncSec[%d] => " i)
        if i >= funcs.Length then Add "Error: Out of range FuncSec index"
        else AddFuncType funcs.[i] 

    let AddFuncSec funcs =
        Title "Funcs section"
        funcs |> Array.iteri (fun i _ ->
            AddFuncSecEntry i funcs
            NewLine ())
        

    // BODY

    let AddBody instructionsList =

        let mutable ip = 0
        let mutable indent = 0

        let addLine s =
            sb.Append (sprintf "  %3d | " ip) |> ignore
            sb.Append (new string(' ', (indent * 2))) |> ignore
            sb.AppendLine s |> ignore
            ip <- ip + 1

        let addIndex s i = 
            addLine (sprintf "%s [%d]" s i)

        let rec addInstructions instructionsList =
            match instructionsList with
                | head::tail -> 
                    addInstruction head
                    addInstructions tail
                | [] -> ()

        and withIndent f =
            indent <- indent + 1
            f ()
            indent <- indent - 1

        and addLoad s memArg instr = 
            addLine (sprintf "%s %s" s (SingleLineFormatted memArg))
            withIndent (fun () -> 
                addInstruction instr)

        and addStore s memArg ins1 ins2 = 
            addLine (sprintf "%s %s" s (SingleLineFormatted memArg))
            withIndent (fun () ->
                addInstruction ins1
                addInstruction ins2)

        and addSet s i instr = 
            addLine (sprintf "%s [%d]" s i)
            withIndent (fun () -> 
                addInstruction instr)

        and addConst c =
            addLine (sprintf "const %A" c)

        and addUnary s ins =
            addLine s
            withIndent (fun () -> 
                addInstruction ins)

        and addBinary s ins1 ins2 =
            addLine s
            withIndent (fun () ->
                addInstruction ins1
                addInstruction ins2)

        and addBlock s bt il =
            let blockIndicator = 
                match bt with 
                    | EmptyBlockType -> ""
                    | BlockValType(bvt) -> PrettyValType bvt
            addLine (sprintf "%s %s" s blockIndicator)
            withIndent (fun () -> 
                addInstructions il)

        and addInstruction instr =

            match instr with

                | Unreachable 
                | Nop 
                | Return
                | Drop
                | Select
                | MemorySize
                | GrowMemory
                    -> addLine (instr.ToString())

                | I32Const(c) -> addConst c
                | I64Const(c) -> addConst c
                | F32Const(c) -> addConst c
                | F64Const(c) -> addConst c

                | GetLocal  (LocalIdx(U32(idx)))         -> addIndex "Get Local"  idx 
                | GetGlobal (GlobalIdx(U32(idx)))        -> addIndex "Get Global" idx 
                | SetLocal  (LocalIdx(U32(idx)), instr)  -> addSet "Set Local"  idx instr
                | TeeLocal  (LocalIdx(U32(idx)), instr)  -> addSet "Tee Local"  idx instr
                | SetGlobal (GlobalIdx(U32(idx)), instr) -> addSet "Set Global" idx instr

                | I32Load    (memArg, instr) -> addLoad "I32Load"    memArg instr
                | I64Load    (memArg, instr) -> addLoad "I64Load"    memArg instr
                | F32Load    (memArg, instr) -> addLoad "F32Load"    memArg instr
                | F64Load    (memArg, instr) -> addLoad "F64Load"    memArg instr
                | I32Load8s  (memArg, instr) -> addLoad "I32Load8s"  memArg instr
                | I32Load8u  (memArg, instr) -> addLoad "I32Load8u"  memArg instr
                | I32Load16s (memArg, instr) -> addLoad "I32Load16s" memArg instr
                | I32Load16u (memArg, instr) -> addLoad "I32Load16u" memArg instr
                | I64Load8s  (memArg, instr) -> addLoad "I64Load8s"  memArg instr
                | I64Load8u  (memArg, instr) -> addLoad "I64Load8u"  memArg instr
                | I64Load16s (memArg, instr) -> addLoad "I64Load16s" memArg instr
                | I64Load16u (memArg, instr) -> addLoad "I64Load16u" memArg instr
                | I64Load32s (memArg, instr) -> addLoad "I64Load32s" memArg instr
                | I64Load32u (memArg, instr) -> addLoad "I64Load32u" memArg instr

                | I32Store   (memArg, ins1, ins2) -> addStore "I32Store"   memArg ins1 ins2
                | I64Store   (memArg, ins1, ins2) -> addStore "I64Store"   memArg ins1 ins2
                | F32Store   (memArg, ins1, ins2) -> addStore "F32Store"   memArg ins1 ins2
                | F64Store   (memArg, ins1, ins2) -> addStore "F64Store"   memArg ins1 ins2
                | I32Store8  (memArg, ins1, ins2) -> addStore "I32Store8"  memArg ins1 ins2
                | I32Store16 (memArg, ins1, ins2) -> addStore "I32Store16" memArg ins1 ins2
                | I64Store8  (memArg, ins1, ins2) -> addStore "I64Store8"  memArg ins1 ins2
                | I64Store16 (memArg, ins1, ins2) -> addStore "I64Store16" memArg ins1 ins2
                | I64Store32 (memArg, ins1, ins2) -> addStore "I64Store32" memArg ins1 ins2

                | I32Eqz            (i) -> addUnary "I32Eqz" i
                | I64Eqz            (i) -> addUnary "I64Eqz" i
                | I32Clz            (i) -> addUnary "I32Clz" i
                | I32Ctz            (i) -> addUnary "I32Ctz" i
                | I32PopCnt         (i) -> addUnary "I32PopCnt" i
                | I64Clz            (i) -> addUnary "I64Clz" i
                | I64Ctz            (i) -> addUnary "I64Ctz" i
                | I64PopCnt         (i) -> addUnary "I64PopCnt" i
                | F32Abs            (i) -> addUnary "F32Abs" i
                | F32Neg            (i) -> addUnary "F32Neg" i
                | F32Ceil           (i) -> addUnary "F32Ceil" i
                | F32Floor          (i) -> addUnary "F32Floor" i
                | F32Trunc          (i) -> addUnary "F32Trunc" i
                | F32Nearest        (i) -> addUnary "F32Nearest" i
                | F32Sqrt           (i) -> addUnary "F32Sqrt" i
                | F64Abs            (i) -> addUnary "F64Abs" i
                | F64Neg            (i) -> addUnary "F64Neg" i
                | F64Ceil           (i) -> addUnary "F64Ceil" i
                | F64Floor          (i) -> addUnary "F64Floor" i
                | F64Trunc          (i) -> addUnary "F64Trunc" i
                | F64Nearest        (i) -> addUnary "F64Nearest" i
                | F64Sqrt           (i) -> addUnary "F64Sqrt" i
                | I32WrapI64        (i) -> addUnary "I32WrapI64" i
                | I32TruncsF32      (i) -> addUnary "I32TruncsF32" i
                | I32TruncuF32      (i) -> addUnary "I32TruncuF32" i
                | I32TruncsF64      (i) -> addUnary "I32TruncsF64" i
                | I32TruncuF64      (i) -> addUnary "I32TruncuF64" i
                | I64ExtendsI32     (i) -> addUnary "I64ExtendsI32" i
                | I64ExtenduI32     (i) -> addUnary "I64ExtenduI32" i
                | I64TruncsF32      (i) -> addUnary "I64TruncsF32" i
                | I64TruncuF32      (i) -> addUnary "I64TruncuF32" i
                | I64TruncsF64      (i) -> addUnary "I64TruncsF64" i
                | I64TruncuF64      (i) -> addUnary "I64TruncuF64" i
                | F32ConvertsI32    (i) -> addUnary "F32ConvertsI32" i
                | F32ConvertuI32    (i) -> addUnary "F32ConvertuI32" i
                | F32ConvertsI64    (i) -> addUnary "F32ConvertsI64" i
                | F32ConvertuI64    (i) -> addUnary "F32ConvertuI64" i
                | F32DemoteF64      (i) -> addUnary "F32DemoteF64" i
                | F64ConvertsI32    (i) -> addUnary "F64ConvertsI32" i
                | F64ConvertuI32    (i) -> addUnary "F64ConvertuI32" i
                | F64ConvertsI64    (i) -> addUnary "F64ConvertsI64" i
                | F64ConvertuI64    (i) -> addUnary "F64ConvertuI64" i
                | F64PromoteF32     (i) -> addUnary "F64PromoteF32" i
                | I32ReinterpretF32 (i) -> addUnary "I32ReinterpretF32" i
                | I64ReinterpretF64 (i) -> addUnary "I64ReinterpretF64" i
                | F32ReinterpretI32 (i) -> addUnary "F32ReinterpretI32" i
                | F64ReinterpretI64 (i) -> addUnary "F64ReinterpretI64" i

                | I32Eq       (a,b) -> addBinary "I32Eq" a b
                | I32Ne       (a,b) -> addBinary "I32Ne" a b
                | I32Lts      (a,b) -> addBinary "I32Lts" a b
                | I32Ltu      (a,b) -> addBinary "I32Ltu" a b
                | I32Gts      (a,b) -> addBinary "I32Gts" a b
                | I32Gtu      (a,b) -> addBinary "I32Gtu" a b
                | I32Les      (a,b) -> addBinary "I32Les" a b
                | I32Leu      (a,b) -> addBinary "I32Leu" a b
                | I32Ges      (a,b) -> addBinary "I32Ges" a b
                | I32Geu      (a,b) -> addBinary "I32Geu" a b
                | I64Eq       (a,b) -> addBinary "I64Eq" a b
                | I64Ne       (a,b) -> addBinary "I64Ne" a b
                | I64Lts      (a,b) -> addBinary "I64Lts" a b
                | I64Ltu      (a,b) -> addBinary "I64Ltu" a b
                | I64Gts      (a,b) -> addBinary "I64Gts" a b
                | I64Gtu      (a,b) -> addBinary "I64Gtu" a b
                | I64Les      (a,b) -> addBinary "I64Les" a b
                | I64Leu      (a,b) -> addBinary "I64Leu" a b
                | I64Ges      (a,b) -> addBinary "I64Ges" a b
                | I64Geu      (a,b) -> addBinary "I64Geu" a b
                | F32Eq       (a,b) -> addBinary "F32Eq" a b
                | F32Ne       (a,b) -> addBinary "F32Ne" a b
                | F32Lt       (a,b) -> addBinary "F32Lt" a b
                | F32Gt       (a,b) -> addBinary "F32Gt" a b
                | F32Le       (a,b) -> addBinary "F32Le" a b
                | F32Ge       (a,b) -> addBinary "F32Ge" a b
                | F64Eq       (a,b) -> addBinary "F64Eq" a b
                | F64Ne       (a,b) -> addBinary "F64Ne" a b
                | F64Lt       (a,b) -> addBinary "F64Lt" a b
                | F64Gt       (a,b) -> addBinary "F64Gt" a b
                | F64Le       (a,b) -> addBinary "F64Le" a b
                | F64Ge       (a,b) -> addBinary "F64Ge" a b
                | I32Add      (a,b) -> addBinary "I32Add" a b
                | I32Sub      (a,b) -> addBinary "I32Sub" a b
                | I32Mul      (a,b) -> addBinary "I32Mul" a b
                | I32Divs     (a,b) -> addBinary "I32Divs" a b
                | I32Divu     (a,b) -> addBinary "I32Divu" a b
                | I32Rems     (a,b) -> addBinary "I32Rems" a b
                | I32Remu     (a,b) -> addBinary "I32Remu" a b
                | I32And      (a,b) -> addBinary "I32And" a b
                | I32Or       (a,b) -> addBinary "I32Or" a b
                | I32Xor      (a,b) -> addBinary "I32Xor" a b
                | I32Shl      (a,b) -> addBinary "I32Shl" a b
                | I32Shrs     (a,b) -> addBinary "I32Shrs" a b
                | I32Shru     (a,b) -> addBinary "I32Shru" a b
                | I32Rotl     (a,b) -> addBinary "I32Rotl" a b
                | I32Rotr     (a,b) -> addBinary "I32Rotr" a b
                | I64Add      (a,b) -> addBinary "I64Add" a b
                | I64Sub      (a,b) -> addBinary "I64Sub" a b
                | I64Mul      (a,b) -> addBinary "I64Mul" a b
                | I64Divs     (a,b) -> addBinary "I64Divs" a b
                | I64Divu     (a,b) -> addBinary "I64Divu" a b
                | I64Rems     (a,b) -> addBinary "I64Rems" a b
                | I64Remu     (a,b) -> addBinary "I64Remu" a b
                | I64And      (a,b) -> addBinary "I64And" a b
                | I64Or       (a,b) -> addBinary "I64Or" a b
                | I64Xor      (a,b) -> addBinary "I64Xor" a b
                | I64Shl      (a,b) -> addBinary "I64Shl" a b
                | I64Shrs     (a,b) -> addBinary "I64Shrs" a b
                | I64Shru     (a,b) -> addBinary "I64Shru" a b
                | I64Rotl     (a,b) -> addBinary "I64Rotl" a b
                | I64Rotr     (a,b) -> addBinary "I64Rotr" a b
                | F32Add      (a,b) -> addBinary "F32Add" a b
                | F32Sub      (a,b) -> addBinary "F32Sub" a b
                | F32Mul      (a,b) -> addBinary "F32Mul" a b
                | F32Div      (a,b) -> addBinary "F32Div" a b
                | F32Min      (a,b) -> addBinary "F32Min" a b
                | F32Max      (a,b) -> addBinary "F32Max" a b
                | F32CopySign (a,b) -> addBinary "F32CopySign" a b
                | F64Add      (a,b) -> addBinary "F64Add" a b
                | F64Sub      (a,b) -> addBinary "F64Sub" a b
                | F64Mul      (a,b) -> addBinary "F64Mul" a b
                | F64Div      (a,b) -> addBinary "F64Div" a b
                | F64Min      (a,b) -> addBinary "F64Min" a b
                | F64Max      (a,b) -> addBinary "F64Max" a b
                | F64CopySign (a,b) -> addBinary "F64CopySign" a b

                | Block (bt,il) -> addBlock "Block" bt il
                | Loop  (bt,il) -> addBlock "Loop" bt il
                | If    (bt,il) -> addBlock "If" bt il

                | IfElse (bt,il1,il2) -> 
                    addLine (sprintf "IfElse %A" bt)
                    withIndent (fun () -> 
                        addInstructions il1
                        addInstructions il2)

                | Br   (LabelIdx(U32(i))) -> addIndex "Br" i
                
                | BrIf (cond, LabelIdx(U32(i))) -> 
                    addIndex "BrIf" i
                    withIndent (fun () -> addInstruction cond)

                | BrTable (ins, idxs, idx) ->
                    addLine "BrTable  ** todo: show table"  // TODO
                    withIndent (fun () -> addInstruction ins)

                | Call (FuncIdx(U32(fidx)), paramList) ->
                    addLine (sprintf "Call FuncIdx[%d]" fidx)  // TODO: show names?
                    withIndent (fun () -> addInstructions paramList)

                | CallIndirect (ft, paramList) ->  // TODO
                    addLine "CallIndirect ** TODO: functype **"
                    withIndent (fun () -> addInstructions paramList)


            


(*
            let withIndentationDo opCode blockType f =
                addLine (sprintf "%s %A" opCode blockType)
                indent <- indent + 1
                f
                indent <- indent - 1

            let indentedList opCode blockType subInstructions =
                withIndentationDo opCode blockType (addInstructions subInstructions)

            let indentedInstr opCode blockType instr =
                withIndentationDo opCode blockType (addInstruction instr)

            let isShortForm instruction =
                match instruction with 
                    | Block(_) -> false
                    | Loop(_) -> false
                    | If(_) -> false
                    | IfElse(_) -> false
                    | _ -> true

            let addLongForm instruction =
                match instruction with 
                    | Block(b,a)    -> indentedList "Block" b a
                    | Loop(b,a)     -> indentedList "Loop" b a
                    | If(b,a)       -> indentedList "If" b a
                    | IfElse(b,i,e) -> 
                        indentedList "If..." b i
                        indentedList "...Else" b e
                    | _ -> ()

            let addInstruction instruction =
                if isShortForm instruction
                then addLine (SingleLineFormatted instruction)
                else addLongForm instruction

            instructionsList |> List.iter addInstruction
            *)

        addInstructions instructionsList


    // CODE SECTION

    let AddCodeSec cso =

        Title "Code section"

        // LOCALS

        let addLocals localsArray =

            let mutable localIndex = 0

            let rec addIndividualLocal repeatCounter t =
                if repeatCounter >= 1u then
                    Text (sprintf "        Local[%d] %A" localIndex t)
                    localIndex <- localIndex + 1
                    addIndividualLocal (repeatCounter - 1u) t

            let addLocals (locals:Locals) =
                match locals.NumRepeats with
                    | U32(repeatCounter) -> addIndividualLocal repeatCounter locals.LocalsType
                NewLine ()

            localsArray |> Array.iter addLocals

        // CODE

        let addCodeDetail i c =
            NewLine ()
            NewLine ()
            Text (sprintf "CodeSec[%d]  (%d bytes)" i (match c with { CodeSize=U32(n) } -> n))
            NewLine ()
            addLocals c.Function.Locals
            AddBody c.Function.Body

        cso |> Array.iteri addCodeDetail 

    // TABLE SECTION

    let AddTableType (t:TableType) =
        Add (sprintf "%A " t.TableElementType)
        AddLimits t.TableLimits

    let AddTableSecEntry i (tables:Table[]) =
        Add (sprintf "TableSec[%d] => " i)
        if i >= tables.Length then Add "Error: Tablesec index out of range"
        else
            match tables.[i] with
                | {TableType=t} -> AddTableType t

    let AddTableSec tables =
        Title "Tables section"
        tables |> Array.iteri 
            (fun i _ ->
                AddTableSecEntry i tables
                NewLine ())

    // MEMS SECTION

    let AddMemType (t:MemoryType) =
        AddLimits t.MemoryLimits
        Add " x 64KB"

    let AddMemSecEntry i (mems:Mem[]) =
        Add (sprintf "MemSec[%d] => " i)
        if i >= mems.Length then Add "Error: MemSec index out of range"
        else
            match mems.[i] with
                | {MemType=t} -> AddMemType t

    let AddMemSec mems =
        Title "Mems section"
        mems |> Array.iteri 
            (fun i _ ->
                AddMemSecEntry i mems
                NewLine ())

    // GLOBAL SECTION

    let AddGlobalType t =
        Add (PrettyMutability t.GlobalMutability)
        Add ":"
        Add (PrettyValType t.GlobalType)

    let AddGlobalTypeAndInstructions gt ins =
        AddGlobalType gt
        NewLine ()
        AddBody ins
        NewLine ()
        NewLine ()

    let AddGlobalSecEntry i (globals:Global[]) =
        Add (sprintf "GlobalSec[%d] => " i)
        if i >= globals.Length then Add "Error: GlobalSec index out of range"
        else
            match globals.[i] with
                | {GlobalType=t; InitExpr=e} -> AddGlobalTypeAndInstructions t e

    let AddGlobalSec globals =
        Title "Globals section"
        globals |> Array.iteri 
            (fun i _ ->
                AddGlobalSecEntry i globals
                NewLine ())


    // IMPORT SECTION

    let AddImportSecEntry i (imports:Import[]) (types:FuncType[]) =
        Add (sprintf "ImportSec[%d] => " i)
        if i >= imports.Length then Add "Error: ImportSec index out of range."
        else
            match imports.[i] with
                | {ImportModuleName=m; ImportName=n; ImportDesc=d} -> 
                    Add (sprintf "from '%s' import '%s' == " m n)
                    match d with
                        | ImportFunc(funcType) -> AddFuncType funcType   // TODO: test
                        | ImportTable(tt)  -> AddTableType tt
                        | ImportMemory(mt) -> AddMemType mt
                        | ImportGlobal(gt) -> AddGlobalType gt 

    let AddImportSec types imports =
        Title "Imports section"
        imports |> Array.iteri 
            (fun i _ ->
                AddImportSecEntry i imports types
                NewLine ())

    // EXPORT SECTION

    let AddExportSecEntry i (exports:Export[]) (functions:FuncType[]) (tables:Table[]) (mems:Mem[]) (globals:Global[]) =
        Add (sprintf "ExportSec[%d] => " i)
        if (i >= exports.Length) then Add "Error: ExportSec index out of range"
        else
            match exports.[i] with
                | {ExportName=n; ExportDesc=d} -> 
                    Add (sprintf "%s == " n)
                    match d with
                        | ExportFunc(FuncIdx(U32(fi)))     -> AddFuncSecEntry   (int fi) functions
                        | ExportTable(TableIdx(U32(ti)))   -> AddTableSecEntry  (int ti) tables
                        | ExportMemory(MemIdx(U32(mi)))    -> AddMemSecEntry    (int mi) mems
                        | ExportGlobal(GlobalIdx(U32(gi))) -> AddGlobalSecEntry (int gi) globals

    let AddExportSec functions tables mems globals exports =
        Title "Exports section"
        exports |> Array.iteri 
            (fun i _ ->
                AddExportSecEntry i exports functions tables mems globals
                NewLine ())

    // START

    let AddStartSec (types:FuncType[]) optionalStartSec =
        Title "Start section"
        match optionalStartSec with
            | Some({ StartFuncIdx=FuncIdx(U32(sfi)) }) -> 
                AddTypeSecEntry (int sfi) types
            | None -> ()

    // ELEM SECTION

    let AddElemSecEntry i (elems:Elem[]) =
        Add (sprintf "ElemSec[%d] => " i)
        if i >= elems.Length then Add "Error: ElemSec index out of range"
        else 
            match elems.[i] with
                | { TableIndex=TableIdx(U32(ti)); OffsetExpr=e; Init=fa } ->
                    Text (sprintf "In table %d, expr =" ti)
                    AddBody e
                    fa |> Array.iteri (fun i fidx -> 
                        match fidx with 
                            | FuncIdx(U32(fi)) -> 
                                Add (sprintf "Table[%d][expr+%d] = FuncSec[%d]" ti i fi)
                                NewLine())

    let AddElemSec elems =
        Title "Elems section"
        elems |> Array.iteri 
            (fun i _ ->
                AddElemSecEntry i elems
                NewLine ())

    // DATA SECTION

    let AddDataSecEntry i (datas:Data[]) =
        Add (sprintf "DataSec[%d] => " i)
        if i >= datas.Length then Add "Error: DataSec index out of range"
        else
            match datas.[i] with
                | { DataMemoryIndex=MemIdx(U32(mi)); OffsetExpr=e; InitImageBytes=imageData } ->
                    Text (sprintf "Fill memory[%d], where the offset-expr and image-data are as follows:" mi)
                    AddBody e
                    AddByteArray imageData "      | "
                    NewLine ()

    let AddDataSec datas =
        Title "Data section"
        datas |> Array.iteri 
            (fun i _ ->
                AddDataSecEntry i datas
                NewLine ())


    // MODULE        

    /// <summary>
    /// Read the WASM file from the reader, and return a tree structure
    /// representing the file.  For convenience, the imports are
    /// amalgamated into the Funcs, Tables, Mems and Globals, for 
    /// convenient indexing per the WebAssembly spec.
    /// </summary>
    let GetConvenientModule rawModule =

        let newLists = rawModule |> GetConvenientLookupTables

        { rawModule with 
            Funcs = newLists.MasterFuncs;
            Tables = newLists.MasterTables; 
            Mems = newLists.MasterMems; 
            Globals = newLists.MasterGlobals }

    let AddModule theModule =

        theModule.Custom1 |> AddArraySection "Custom section #1"     
        theModule.Types |> AddTypeSec 

        theModule.Custom2 |> AddArraySection "Custom section #2"     
        theModule.Imports |> AddImportSec theModule.Types

        theModule.Custom3 |> AddArraySection "Custom section #3"     
        theModule.Funcs |> AddFuncSec

        theModule.Custom4 |> AddArraySection "Custom section #4"     
        theModule.Tables |> AddTableSec 

        theModule.Custom5 |> AddArraySection "Custom section #5"     
        theModule.Mems |> AddMemSec 

        theModule.Custom6 |> AddArraySection "Custom section #6"     
        theModule.Globals |> AddGlobalSec 

        theModule.Custom7 |> AddArraySection "Custom section #7"     
        theModule.Exports |> AddExportSec theModule.Funcs theModule.Tables theModule.Mems theModule.Globals

        theModule.Custom8 |> AddArraySection "Custom section #8"     
        theModule.Start |> AddStartSec theModule.Types

        theModule.Custom9 |> AddArraySection "Custom section #9"     
        theModule.Elems |> AddElemSec

        theModule.Custom10 |> AddArraySection "Custom section #10"    
        theModule.Codes |> AddCodeSec 

        theModule.Custom11 |> AddArraySection "Custom section #11"    
        theModule.Datas |> AddDataSec 

        theModule.Custom12 |> AddArraySection "Custom section #12"    

    Title ("Unit test serialisation for: " + fileName)
    m |> GetConvenientModule |> AddModule
    sb.ToString ()

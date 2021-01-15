module OptimiseCommonRegisterMachine

open CommonRegisterMachineTypes
open PrivateOptimiseCommonRegisterMachine


let WithEmptyList _ _ = [||]


let AssigningInsteadTo otherReg = function   // Note: is counterpart to IsAssignToA
    | Const(_,x)    -> Const(otherReg,x)
    | FetchLoc(_,x) -> FetchLoc(otherReg,x)
    | FetchGlo(_,x) -> FetchGlo(otherReg,x)
    | _             -> failwith "Cannot re-assign to another register"    



/// Optimisations that apply to all target machines.
let Optimise (originalList:CRMInstruction32 list) =

    let originalArray = List.toArray originalList   // TODO: re-write ReplaceAll (etc) for F# lists

    // Note: See the 'Where' functions for the templates...

    // TODO: What should be the strategy here?
    //       Surely we need to run these over and over until none change anything.
    //       Might it be better to prioritise the larger cases first, otherwise the shorter cases
    //       may trigger first, and inhibit a match on a larger case?
    // TODO: We have no traceability of what triggered.
    // TODO: Would we materially benefit from describing dependencies between optimisations?

    let withCompareAndBranch branchingIfTrue (a:CRMInstruction32[]) i =

        let branchCondition =
            match a.[i] with
                | CmpBA cond -> if branchingIfTrue then cond else OppositeCrmConditionFor cond
                | _ -> failwith "Unexpected failure of pattern match"

        let targetLabel =
            match a.[i+1] with
                | BranchRegZNZ(_,_,name) -> name
                | _ -> failwith "Unexpected failure of pattern match" // should never happen

        [|
            SecondaryCmpBranch (branchCondition , targetLabel)
            Barrier
        |]

                

    originalArray
        |> ReplaceAll 3 WherePushBarrierPop  WithEmptyList
        |> ReplaceAll 3 WherePushBarrierDrop WithEmptyList
        |> ReplaceAll 3 WherePushBarrierPeek (fun _ _ -> [| Push(A) |])
        |> ReplaceAll 4 WherePushPopAroundPreserving (fun a i -> [| a.[i+1] |])
        |> ReplaceAll 5 WherePushPopAroundPreservingRequiringRename (fun a i -> [| (a.[i] |> AssigningInsteadTo B) ; a.[i+3] |])
        |> ReplaceAll 3 WhereCmpBAthenBranchANZ (withCompareAndBranch true)
        |> ReplaceAll 3 WhereCmpBAthenBranchAZ  (withCompareAndBranch false)
        |> Array.toList



let OptimiseX8632 (originalList:CRMInstruction32 list) =

    let originalArray = List.toArray originalList   // TODO: re-write ReplaceAll (etc) for F# lists

    let withX8632PushConstant (a:CRMInstruction32[]) i =
        match a.[i] with
            | Const (A,value) -> [| X8632Specific (X8632PushConstant value) ; Barrier |]  // Keep the barrier because we're not saying we drop registers through.  We don't.
            | _ -> failwith "Unexpected failure of pattern match" // should never happen

    let withX8632TwoRegisterEffectiveAddress (a:CRMInstruction32[]) i =
        [|
            X8632Specific (
                match a.[i+1] with
                    | Store(A, Stored8,  B, ofs) -> X8632StoreAatEBXplusEDIplusOffset (ofs, "AL")
                    | Store(A, Stored16, B, ofs) -> X8632StoreAatEBXplusEDIplusOffset (ofs, "AX")
                    | Store(A, Stored32, B, ofs) -> X8632StoreAatEBXplusEDIplusOffset (ofs, "EAX")
                    | _ -> failwith "Unexpected failure of pattern match" // should never happen
                )
            Barrier
        |]

    let withX8632OperateOnLocal32 (a:CRMInstruction32[]) i =

        let localIndex =
            match a.[i] with
                | FetchLoc (A, locIdx) -> locIdx
                | _ -> failwith "Unexpected failure of pattern match" // should never happen

        let opcode, value =
            match a.[i+1] with
                | CalcRegNum(AddRegNum, A, value) ->   "add", value
                | CalcRegNum(SubRegNum, A, value) ->   "sub", value
                | CalcRegNum(AndRegNum, A, value) ->   "and", value
                | CalcRegNum(OrRegNum,  A, value) ->   "or",  value
                | CalcRegNum(XorRegNum, A, value) ->   "xor", value
                | _ -> failwith "Unexpected failure of pattern match" // should never happen

        [|
            X8632Specific (X8632OperateOnLocal32 (opcode, localIndex, value))
            Barrier
        |]
        

    originalArray
        |> ReplaceAll 3 WhereX8632LoadConstPushBarrier  withX8632PushConstant
        |> ReplaceAll 3 WhereX8632StoreIntoLinearMemory withX8632TwoRegisterEffectiveAddress
        |> ReplaceAll 4 WhereX8632OperateOnLocal32      withX8632OperateOnLocal32
        |> Array.toList



let RemoveBarriers (originalList:CRMInstruction32 list) =

    let originalArray = List.toArray originalList   // TODO: re-write ReplaceAll (etc) for F# lists

    originalArray
        |> ReplaceAll 1 WhereBarrier WithEmptyList
        |> Array.toList


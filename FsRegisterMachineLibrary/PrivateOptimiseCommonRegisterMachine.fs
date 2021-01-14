module PrivateOptimiseCommonRegisterMachine

open WasmFileTypes
open CommonRegisterMachineTypes



let ReplaceAll patternLength patternMatcher getReplacementRegarding (sourceArray:CRMInstruction32[]) =

    let sourceLength = sourceArray.Length

    if patternLength >= sourceLength 
    then 
        sourceArray  // Trivially can't be any matches
    else
        let result = new ResizeArray<CRMInstruction32> ()
        let mutable i = 0

        while i <= (sourceLength - patternLength) do
            if patternMatcher i sourceArray
            then 
                result.AddRange(getReplacementRegarding sourceArray i)
                i <- i + patternLength
            else
                result.Add(sourceArray.[i])
                i <- i + 1

        while i < sourceLength do
            result.Add(sourceArray.[i])
            i <- i + 1

        result.ToArray ()




let IsPushA   i = match i with | Push(A)       -> true | _ -> false
let IsPeekA   i = match i with | PeekA         -> true | _ -> false
let IsPopA    i = match i with | Pop(A)        -> true | _ -> false
let IsPopB    i = match i with | Pop(B)        -> true | _ -> false
let IsBarrier i = match i with | Barrier       -> true | _ -> false
let IsDropOne i = match i with | Drop (U32 1u) -> true | _ -> false

let IsLabelDeclaration i = match i with | Label _ -> true | _ -> false

let IsRegisterPreserving = function
    | StoreLoc(_)
    | StoreGlo(_)
    | StoreConst(_)
    | Store(_) -> true
    | _ -> false


let IsAssignToA = function
    | Const(A,_)
    | FetchLoc(A,_)
    | FetchGlo(A,_) 
    // | TODO: every other fetch to A
        -> true
    | _ -> false

let IsLoadConstantToA = function
    | Const(A,_) -> true
    | _ -> false

let IsAddBY = function
    | CalcRegReg (AddRbRa,B,Y) -> true
    | _ -> false

let IsStoreOfAnySizeInAToB = function
    | Store (A,_,B,_) -> true
    | _ -> false

let IsFetchAndStoreUsingSameLocal i1 i2 =
    match i1, i2 with
        | (FetchLoc (A,floc)) , (StoreLoc (A,sloc)) when floc = sloc -> true
        | _ -> false

let IsAddSubAndOrXorAN = function
    | CalcWithConst _ -> true
    | _ -> false

let IsCmpBA = function
    | CmpBA _ -> true
    | _ -> false

let IsBranchANZ = function
    | BranchRegZNZ (A,BNonZero,_) -> true
    | _ -> false

let IsBranchAZ = function
    | BranchRegZNZ (A,BZero,_) -> true
    | _ -> false


let WherePushBarrierPop  i (a:CRMInstruction32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPopA    a.[i+2]
let WherePushBarrierDrop i (a:CRMInstruction32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsDropOne a.[i+2]
let WherePushBarrierPeek i (a:CRMInstruction32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPeekA   a.[i+2]
let WhereBarrier         i (a:CRMInstruction32[]) = IsBarrier a.[i]

// TODO: The CRMInstruction32's should be shown as F# DU cases, not in these target languages!
// TODO: Can we not just use cons-lists matching?

let WherePushPopAroundPreserving i (a:CRMInstruction32[]) = 

    //    push A
    //    let int[@loc2]=A
    //    // ~~~ register barrier ~~~
    //    pop A

    IsPushA a.[i] 
    && IsRegisterPreserving a.[i+1] 
    && IsBarrier a.[i+2] 
    && IsPopA a.[i+3]

let WherePushPopAroundPreservingRequiringRename i (a:CRMInstruction32[]) = 

    //    let A=int[@loc4]
    //    push A
    //    // ~~~ register barrier ~~~
    //    let A=int[@loc1]
    //    pop B

    IsAssignToA a.[i]
    && IsPushA a.[i+1]
    && IsBarrier a.[i+2] 
    && IsAssignToA a.[i+3]
    && IsPopB a.[i+4]

let WhereCmpBAthenBranchANZ i (a:CRMInstruction32[]) = 

    // CmpLtsBA  (etc)
    // BranchANZ (LabelName "wasm_l4")
    // Barrier   // Important: This barrier implies the target at wasm_l4 also does not receive registers.

    IsCmpBA a.[i]
    && IsBranchANZ a.[i+1]
    && IsBarrier a.[i+2]

let WhereCmpBAthenBranchAZ i (a:CRMInstruction32[]) = 

    // CmpLtsBA  (etc)
    // BranchAZ  (LabelName "wasm_l4")
    // Barrier   // Important: This barrier implies the target at wasm_l4 also does not receive registers.

    IsCmpBA a.[i]
    && IsBranchAZ a.[i+1]
    && IsBarrier a.[i+2]

let WhereX8632LoadConstPushBarrier i (a:CRMInstruction32[]) = 

    //    mov EAX,16
    //    push EAX
    //    ; ~~~ register barrier ~~~

    IsLoadConstantToA a.[i]
    && IsPushA a.[i+1]
    && IsBarrier a.[i+2] 
    
let WhereX8632StoreIntoLinearMemory i (a:CRMInstruction32[]) = 

    //     add EBX,EDI
    //     mov [EBX],AX  // NB: EBX possibly plus an offset
    //     ; ~~~ register barrier ~~~

    IsAddBY a.[i]
    && IsStoreOfAnySizeInAToB a.[i+1]
    && IsBarrier a.[i+2] 

let WhereX8632OperateOnLocal32  i (a:CRMInstruction32[]) = 

    //     mov EAX,[EBP+12]  ; @loc3
    //     add EAX,2  // alternatives:   sub and or xor
    //     mov [EBP+12],EAX  ; @loc3
    //     ; ~~~ register barrier ~~~

    IsFetchAndStoreUsingSameLocal a.[i] a.[i+2]
    && IsAddSubAndOrXorAN a.[i+1]
    && IsBarrier a.[i+3]


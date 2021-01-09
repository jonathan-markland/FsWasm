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
    | Store16A(_)
    | StoreGlo(_)
    | StoreConst8(_)
    | StoreConst16(_)
    | StoreConst32(_)
    | Store8A(_)
    | Store16A(_)
    | Store32A(_)
        -> true
    | _ -> false


let IsAssignToA = function
    | Const(A,_)
    | FetchLoc(A,_)
    | FetchGlo(A,_) 
    // | TODO: every other fetch to A
        -> true
    | _ -> false



let WherePushBarrierPop  i (a:CRMInstruction32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPopA    a.[i+2]
let WherePushBarrierDrop i (a:CRMInstruction32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsDropOne a.[i+2]
let WherePushBarrierPeek i (a:CRMInstruction32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPeekA   a.[i+2]
let WhereBarrier         i (a:CRMInstruction32[]) = IsBarrier a.[i]

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

let WherePushPopAroundRegisterBarrierAndLabel i (a:CRMInstruction32[]) =

    //     push A
    //     // ~~~ register barrier ~~~
    //     label wasm_l1
    //     pop A

    IsPushA a.[i]
    && IsBarrier a.[i+1]
    && IsLabelDeclaration a.[i+2]
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

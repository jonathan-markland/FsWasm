module PrivateOptimiseSimpleReg32

open SimpleReg32



let ReplaceAll patternLength patternMatcher getReplacementRegarding (sourceArray:InstrSimpleReg32[]) =

    let sourceLength = sourceArray.Length

    if patternLength >= sourceLength 
    then 
        sourceArray  // Trivially can't be any matches
    else
        let result = new ResizeArray<InstrSimpleReg32> ()
        let mutable i = 0

        while i < (sourceLength - patternLength) do
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




let IsPushA   i = match i with | PushA   -> true | _ -> false
let IsPeekA   i = match i with | PeekA   -> true | _ -> false
let IsPopA    i = match i with | PopA    -> true | _ -> false
let IsBarrier i = match i with | Barrier -> true | _ -> false
let IsDrop    i = match i with | Drop    -> true | _ -> false

let IsRegisterPreserving i = 
    match i with 
        | StoreALoc(_)
        | Store16AtoB(_)
        | StoreAGlo(_)
        | StoreConst8toA(_)
        | StoreConst16toA(_)
        | StoreConst32toA(_)
        | StoreConst8toY(_)
        | StoreConst16toY(_)
        | StoreConst32toY(_)
        | Store8AtoB  (_)
        | Store16AtoB (_)
        | Store32AtoB (_)
        | Store8AtoY  (_)
        | Store16AtoY (_)
        | Store32AtoY (_)
            -> true
        | _ -> false



let WherePushBarrierPop  i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPopA a.[i+2]
let WherePushBarrierDrop i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsDrop a.[i+2]
let WherePushBarrierPeek i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPeekA a.[i+2]
let WhereBarrier         i (a:InstrSimpleReg32[]) = IsBarrier a.[i]

let WherePushPopAroundPreserving i (a:InstrSimpleReg32[]) = 

    //    push A
    //    let int[@loc2]=A
    //    // ~~~ register barrier ~~~
    //    pop A

    IsPushA a.[i] 
    && IsRegisterPreserving a.[i+1] 
    && IsBarrier a.[i+2] 
    && IsPopA a.[i+3]




//    let A=int[@loc4]
//    push A
//    // ~~~ register barrier ~~~
//    let A=int[@loc1]
//    pop B
//    add A,B

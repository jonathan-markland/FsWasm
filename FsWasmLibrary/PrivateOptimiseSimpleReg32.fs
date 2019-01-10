module PrivateOptimiseSimpleReg32

open SimpleReg32



let ReplaceAll patternLength patternMatcher replaceWith (sourceArray:InstrSimpleReg32[]) =

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
                i <- i + patternLength
                result.AddRange(replaceWith)
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




let WherePushBarrierPop  i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPopA a.[i+2]
let WherePushBarrierDrop i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsDrop a.[i+2]
let WherePushBarrierPeek i (a:InstrSimpleReg32[]) = IsPushA a.[i] && IsBarrier a.[i+1] && IsPeekA a.[i+2]
let WhereBarrier         i (a:InstrSimpleReg32[]) = IsBarrier a.[i]


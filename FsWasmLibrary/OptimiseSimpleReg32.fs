module OptimiseSimpleReg32

open SimpleReg32
open PrivateOptimiseSimpleReg32



let WithEmptyList _ _ = [||]



let Optimise (originalList:InstrSimpleReg32 list) =

    let originalArray = List.toArray originalList   // TODO: re-write ReplaceAll (etc) for F# lists


    originalArray
        |> ReplaceAll 3 WherePushBarrierPop  WithEmptyList
        |> ReplaceAll 3 WherePushBarrierDrop WithEmptyList
        |> ReplaceAll 3 WherePushBarrierPeek (fun _ _ -> [| PushA |])
        |> ReplaceAll 4 WherePushPopAroundPreserving (fun a i -> [| a.[i+1] |])
        |> Array.toList



let RemoveBarriers (originalList:InstrSimpleReg32 list) =

    let originalArray = List.toArray originalList   // TODO: re-write ReplaceAll (etc) for F# lists

    originalArray
        |> ReplaceAll 1 WhereBarrier WithEmptyList
        |> Array.toList


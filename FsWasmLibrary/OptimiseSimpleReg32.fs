module OptimiseSimpleReg32

open SimpleReg32
open PrivateOptimiseSimpleReg32



let Optimise (originalList:InstrSimpleReg32 list) =

    let originalArray = List.toArray originalList   // TODO: re-write ReplaceAll (etc) for F# lists

    originalArray
        |> ReplaceAll 3 WherePushBarrierPop  [||]
        |> ReplaceAll 3 WherePushBarrierDrop [||]
        |> ReplaceAll 3 WherePushBarrierPeek [| PushA |]
        |> Array.toList



let RemoveBarriers (originalList:InstrSimpleReg32 list) =

    let originalArray = List.toArray originalList   // TODO: re-write ReplaceAll (etc) for F# lists

    originalArray
        |> ReplaceAll 1 WhereBarrier  [||]   
        |> Array.toList


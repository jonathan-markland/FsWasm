module OptimiseSimpleReg32

open SimpleReg32
open PrivateOptimiseSimpleReg32



let Optimise (originalArray:InstrSimpleReg32[]) =

    originalArray
        |> ReplaceAll 3 WherePushBarrierPop  [||]
        |> ReplaceAll 3 WherePushBarrierDrop [||]
        |> ReplaceAll 3 WherePushBarrierPeek [| PushA |]



let RemoveBarriers (originalArray:InstrSimpleReg32[]) =  // <- Must only ever be final

    originalArray
        |> ReplaceAll 1 WhereBarrier  [||]   


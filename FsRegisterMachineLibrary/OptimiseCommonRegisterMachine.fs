module OptimiseCommonRegisterMachine

open CommonRegisterMachineTypes
open PrivateOptimiseCommonRegisterMachine



let WithEmptyList _ _ = [||]


let AssigningInsteadTo otherReg = function   // Note: is counterpart to IsAssignToA
    | Const(_,x)    -> Const(otherReg,x)
    | FetchLoc(_,x) -> FetchLoc(otherReg,x)
    | FetchGlo(_,x) -> FetchGlo(otherReg,x)
    | _             -> failwith "Cannot re-assign to another register"    



let Optimise (originalList:InstrSimpleReg32 list) =

    let originalArray = List.toArray originalList   // TODO: re-write ReplaceAll (etc) for F# lists

    // Note: See the 'Where' functions for the templates...

    originalArray
        |> ReplaceAll 3 WherePushBarrierPop  WithEmptyList
        |> ReplaceAll 3 WherePushBarrierDrop WithEmptyList
        |> ReplaceAll 3 WherePushBarrierPeek (fun _ _ -> [| Push(A) |])
        |> ReplaceAll 4 WherePushPopAroundPreserving (fun a i -> [| a.[i+1] |])
        |> ReplaceAll 5 WherePushPopAroundPreservingRequiringRename (fun a i -> [| (a.[i] |> AssigningInsteadTo B) ; a.[i+3] |])
        |> Array.toList



let RemoveBarriers (originalList:InstrSimpleReg32 list) =

    let originalArray = List.toArray originalList   // TODO: re-write ReplaceAll (etc) for F# lists

    originalArray
        |> ReplaceAll 1 WhereBarrier WithEmptyList
        |> Array.toList


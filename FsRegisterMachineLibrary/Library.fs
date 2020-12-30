module Library

open System.Text
open WasmFileTypes
open BWToCRMConfigurationTypes
open CommonRegisterMachineTypes
open OptimiseCommonRegisterMachine



let WriteOutHexDump (command:string) (byteSeparator:string) (hexPrefix:string) writeLine (byteArray:byte[]) =

    let sb = new StringBuilder(16 * (2 + byteSeparator.Length + hexPrefix.Length) + command.Length)

    byteArray |> Array.iteri (fun i byteVal -> 

        let c = i &&& 15
    
        sb.Append (
            match c with
                | 0 -> sprintf "%s %s%02X" command hexPrefix byteVal
                | _ -> sprintf "%s%s%02X" byteSeparator hexPrefix byteVal
            )
            |> ignore

        match c with
            | 15 -> 
                writeLine (sb.ToString())
                sb.Clear() |> ignore
            | _ -> ()
    )

    if sb.Length > 0 then writeLine (sb.ToString())



let WriteOutWasmStart writeOutBranchToEntryLabel writeOut toComment startOption moduleFuncsArray =
    match startOption with 
        | Some { StartFuncIdx = startFuncIdx } -> 
            writeOutBranchToEntryLabel writeOut startFuncIdx moduleFuncsArray
        | None -> 
            "No entry point in this translation" |> toComment |> writeOut



let private ReturnsSingleValue (ft:FuncType) =
    match ft.ReturnTypes.Length with
        | 0 -> false
        | 1 -> true
        | _ -> failwith "Cannot translate function that returns more than one value"



let WriteOutInstructions writeOut translate funcInstructions thisFuncType config =

    let optimisationPhase1 = 
        match config with
            | TranslationConfiguration(_,FullyOptimised) -> funcInstructions |> Optimise
            | TranslationConfiguration(_,NoOptimisation) -> funcInstructions

    let optimisationPhase2 =
        match config with
            | TranslationConfiguration(WithBarriers,_)    -> optimisationPhase1
            | TranslationConfiguration(WithoutBarriers,_) -> optimisationPhase1 |> RemoveBarriers

    let finalInstructions = optimisationPhase2

    // Kick off the whole thing here:

    finalInstructions |> List.iter (fun instruction -> translate instruction |> List.iter writeOut)

    // Handle the function's return (may need pop into A):

    let returnHandlingCode = 
        match thisFuncType |> ReturnsSingleValue with
            | true  -> translate (Pop A)  // TODO: not ideal construction of temporary
            | false -> []

    returnHandlingCode |> List.iter writeOut





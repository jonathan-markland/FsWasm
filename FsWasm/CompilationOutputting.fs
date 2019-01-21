module CompilationOutputting

open System.Text



let OutputForDebug f =

    let writeOutData s = printfn "DATA> %s" s
    let writeOutVar  s = printfn "VAR>  %s" s
    let writeOutCode s = printfn "CODE> %s" s

    f writeOutData writeOutCode writeOutVar



let OutputInFinalOrder f =
    
    let dataStringBuilder = new StringBuilder ()
    let varStringBuilder  = new StringBuilder ()
    let codeStringBuilder = new StringBuilder ()

    let writeOutData s = dataStringBuilder.AppendLine(s) |> ignore
    let writeOutVar  s = varStringBuilder.AppendLine(s)  |> ignore
    let writeOutCode s = codeStringBuilder.AppendLine(s) |> ignore

    f writeOutData writeOutCode writeOutVar

    printf "%s" (dataStringBuilder.ToString())
    printf "%s" (varStringBuilder.ToString())
    printf "%s" (codeStringBuilder.ToString())



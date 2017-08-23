[<EntryPoint>]
let main argv =
    match argv with
    | [| projectRootPath |] ->
        Decompiler.decompile (DecompilationProject.load projectRootPath)
    | _ -> printfn "Usage: unwind-mc <project-root-path>"
    0

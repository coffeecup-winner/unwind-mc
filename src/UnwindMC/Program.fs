open UnwindMC.Decompilation

[<EntryPoint>]
let main argv =
    match argv with
    | [| projectRootPath |] ->
        let decompiler = new Decompiler(DecompilationProject.Load(projectRootPath))
        decompiler.Decompile()
    | _ -> printfn "Usage: unwind-mc <project-root-path>"
    0

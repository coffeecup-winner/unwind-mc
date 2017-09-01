module TempDirectory

open System
open System.IO

type T(dirPath: string) =
    let path: string = dirPath

    do
        Directory.CreateDirectory(path) |> ignore

    member self.Path: string = path
    
    interface IDisposable with
        member self.Dispose(): unit =
            Directory.Delete(path, true)
